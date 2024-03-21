use std::{
    collections::HashMap,
    path::MAIN_SEPARATOR,
    process::{Child, Command, Stdio},
};

use parsy::{CodeRange, Eaten};
use reshell_checker::DevelopedSingleCmdCall;
use reshell_parser::ast::{
    CmdArg, CmdCall, CmdEnvVar, CmdEnvVarValue, CmdFlagArg, CmdFlagNameArg, CmdFlagValueArg,
    CmdPath, CmdPipe, CmdPipeType, CmdValueMakingArg, FlagValueSeparator, RuntimeCodeRange,
    SingleCmdCall,
};

use crate::{
    context::{Context, DepsScopeCreationData, ScopeContent},
    display::value_to_str,
    errors::{ExecErrorNature, ExecResult},
    expr::{eval_computed_string, eval_expr, eval_literal_value},
    functions::{call_fn_value, FnCallType, FnPossibleCallArgs},
    gc::GcReadOnlyCell,
    pretty::{PrettyPrintOptions, PrettyPrintable},
    values::{LocatedValue, RuntimeCmdAlias, RuntimeFnValue, RuntimeValue},
};

pub fn run_cmd(
    call: &Eaten<CmdCall>,
    ctx: &mut Context,
    capture_stdout: bool,
) -> ExecResult<(Option<String>, Option<LocatedValue>)> {
    ctx.ensure_no_ctrl_c_press(call.at)?;

    let CmdCall { base, pipes } = &call.data;

    let chain = [(base, None)]
        .into_iter()
        .chain(
            pipes
                .iter()
                .map(|CmdPipe { pipe_type, cmd }| (cmd, Some(pipe_type))),
        )
        .map(|(call, pipe_type)| {
            build_cmd_data(call, ctx).map(|cmd_data| (cmd_data, pipe_type.copied()))
        })
        .collect::<Result<Vec<_>, _>>()?;

    let pipe_types = chain
        .iter()
        .map(|(_, pipe_type)| *pipe_type)
        .collect::<Vec<_>>();

    // TODO: optimize (this should only be an assertion)
    let chain_has_fn_call = chain.iter().any(|(cmd_data, _)| match cmd_data.target {
        EvaluatedCmdTarget::ExternalCommand(_) => false,
        EvaluatedCmdTarget::Function(_) => true,
    });

    if chain_has_fn_call {
        if capture_stdout {
            return Err(ctx.error(call.at, "only external commands' output can be captured"));
        }

        let mut last_return_value = None::<LocatedValue>;

        let chain_len = chain.len();

        for (i, (cmd_data, pipe_type)) in chain.into_iter().enumerate() {
            if let Some(pipe_type) = pipe_type {
                assert_eq!(pipe_type.data, CmdPipeType::Value);
            }

            let EvaluatedCmdData {
                target,
                args: EvaluatedCmdArgs { env_vars, args },
                call_at,
            } = cmd_data;

            if !env_vars.is_empty() {
                return Err(ctx.error(
                    call_at,
                    "environment variables are not supported for function calls",
                ));
            }

            let func = match target {
                EvaluatedCmdTarget::ExternalCommand(_) => unreachable!(),
                EvaluatedCmdTarget::Function(func) => func,
            };

            last_return_value = call_fn_value(
                RuntimeCodeRange::Parsed(call_at),
                &func,
                FnPossibleCallArgs::ParsedCmdArgs {
                    call_type: pipe_type.map(|pipe_type| {
                        assert_eq!(pipe_type.data, CmdPipeType::Value);

                        FnCallType::Piped(last_return_value.unwrap())
                    }),

                    args,
                },
                ctx,
            )?;

            if i + 1 < chain_len && last_return_value.is_none() {
                return Err(ctx.error(call_at, "this function call did not return a value"));
            }
        }

        return Ok((None, last_return_value));
    }

    let mut children: Vec<(Child, CodeRange)> = Vec::with_capacity(chain.len());

    for (i, (cmd_data, pipe_type)) in chain.into_iter().enumerate() {
        let next_pipe_type = pipe_types
            .get(i + 1)
            .and_then(|pipe_type| pipe_type.map(|pipe_type| pipe_type.data));

        let EvaluatedCmdData {
            target,
            args,
            call_at,
        } = cmd_data;

        let cmd_name = match target {
            EvaluatedCmdTarget::ExternalCommand(name) => name,
            EvaluatedCmdTarget::Function(_) => unreachable!(),
        };

        let child = exec_cmd(
            &cmd_name,
            args,
            pipe_type,
            next_pipe_type,
            capture_stdout,
            &mut children,
            ctx,
        )?;

        children.push((child, call_at));
    }

    let mut last_output = None;

    for (child, at) in children.into_iter().rev() {
        let output = child.wait_with_output();

        ctx.reset_ctrl_c_press_indicator();

        let output = output.map_err(|err| {
            ctx.error(
                at,
                ExecErrorNature::CommandFailed {
                    message: format!("command failed: {err}"),
                    exit_status: None,
                },
            )
        })?;

        if !output.status.success() {
            return Err(ctx.error(
                at,
                ExecErrorNature::CommandFailed {
                    message: format!(
                        "command failed{}",
                        match output.status.code() {
                            Some(code) => format!(" with status code {code}"),
                            None => String::new(),
                        }
                    ),
                    exit_status: output.status.code(),
                },
            ));
        }

        last_output = Some(output.stdout);
    }

    let captured = if capture_stdout {
        // Invalid UTF-8 output will be handled with "unknown" symbols
        let mut out = String::from_utf8_lossy(&last_output.unwrap()).into_owned();

        if out.ends_with('\n') {
            out.pop();
        }

        Some(out)
    } else {
        None
    };

    Ok((captured, None))
}

fn build_cmd_data(call: &Eaten<SingleCmdCall>, ctx: &mut Context) -> ExecResult<EvaluatedCmdData> {
    let mut args = EvaluatedCmdArgs {
        env_vars: HashMap::new(),
        args: vec![],
    };

    let developed = ctx.get_developed_cmd_call(call);

    // let mut target = None;

    let DevelopedSingleCmdCall {
        at: _,
        is_function,
        developed_aliases,
    } = &*developed;

    let mut target = None::<EvaluatedCmdTarget>;

    for (i, developed_alias) in developed_aliases.iter().rev().enumerate() {
        let runtime_alias = ctx
            .visible_scopes()
            .find_map(|scope| {
                scope
                    .cmd_aliases
                    .get(&developed_alias.called_alias_name.data)
            })
            .unwrap_or_else(|| {
                ctx.panic(
                    developed_alias.called_alias_name.at,
                    "runtime alias not found (= bug in checker)",
                )
            });

        let RuntimeCmdAlias {
            name_declared_at,
            alias_content: _,
            parent_scopes,
            captured_deps,
        } = &*runtime_alias.value;

        ctx.create_and_push_scope_with_deps(
            RuntimeCodeRange::Parsed(*name_declared_at),
            DepsScopeCreationData::CapturedDeps(captured_deps.clone()),
            // This scope won't be used anyway, it is only here to provide access to the deps scope
            ScopeContent::new(),
            parent_scopes.clone(),
            None,
        );

        let alias_inner_content = ctx.get_developed_cmd_alias_content(developed_alias);

        complete_cmd_data(&alias_inner_content, &mut args, ctx)?;

        if i == 0 {
            target = Some(evaluate_cmd_target(&alias_inner_content, ctx)?);
        }

        ctx.pop_scope();
    }

    complete_cmd_data(call, &mut args, ctx)?;

    let target = match target {
        Some(target) => target,
        None => evaluate_cmd_target(call, ctx)?,
    };

    match target {
        EvaluatedCmdTarget::Function(_) => assert!(is_function),
        EvaluatedCmdTarget::ExternalCommand(_) => assert!(!is_function),
    }

    Ok(EvaluatedCmdData {
        args,
        call_at: call.at,
        target,
    })
}

fn complete_cmd_data(
    call: &Eaten<SingleCmdCall>,
    out: &mut EvaluatedCmdArgs,
    ctx: &mut Context,
) -> ExecResult<()> {
    let SingleCmdCall {
        path: _,
        env_vars,
        args,
    } = &call.data;

    for env_var in &env_vars.data {
        let CmdEnvVar { name, value } = &env_var.data;

        out.env_vars
            .insert(name.data.clone(), compute_env_var_value(&value.data, ctx)?);
    }

    for arg in &args.data {
        out.args.push((eval_cmd_arg(&arg.data, ctx)?, arg.at));
    }

    Ok(())
}

fn evaluate_cmd_target(
    call: &Eaten<SingleCmdCall>,
    ctx: &mut Context,
) -> ExecResult<EvaluatedCmdTarget> {
    let developed = ctx.get_developed_cmd_call(call);

    let cmd_path = &call.data.path;

    if developed.is_function {
        let func = match &cmd_path.data {
            CmdPath::RawString(name) => GcReadOnlyCell::clone(ctx.get_visible_fn_value(name)?),

            CmdPath::Expr(expr) => {
                let value = eval_expr(&expr.data, ctx)?;

                match value {
                    RuntimeValue::Function(func) => func,
                    _ => {
                        return Err(ctx.error(
                            cmd_path.at,
                            format!(
                                "expected a function, found a {}",
                                value
                                    .get_type()
                                    .render_colored(ctx, PrettyPrintOptions::inline())
                            ),
                        ))
                    }
                }
            }

            CmdPath::Direct(_) | CmdPath::ComputedString(_) => unreachable!(),
        };

        return Ok(EvaluatedCmdTarget::Function(func));
    }

    Ok(EvaluatedCmdTarget::ExternalCommand(match &cmd_path.data {
        CmdPath::RawString(raw) => raw.clone(),
        CmdPath::Direct(direct) => direct.clone(),
        CmdPath::Expr(_) => unreachable!(),
        CmdPath::ComputedString(c_str) => c_str.forge_here(eval_computed_string(c_str, ctx)?),
    }))
}

struct EvaluatedCmdData {
    target: EvaluatedCmdTarget,
    args: EvaluatedCmdArgs,
    call_at: CodeRange,
}

struct EvaluatedCmdArgs {
    env_vars: HashMap<String, String>,
    args: Vec<(CmdArgResult, CodeRange)>,
}

enum EvaluatedCmdTarget {
    ExternalCommand(Eaten<String>),
    Function(GcReadOnlyCell<RuntimeFnValue>),
}

fn exec_cmd(
    name: &Eaten<String>,
    args: EvaluatedCmdArgs,
    pipe_type: Option<Eaten<CmdPipeType>>,
    next_pipe_type: Option<CmdPipeType>,
    capture_stdout: bool,
    children: &mut [(Child, CodeRange)],
    ctx: &mut Context,
) -> ExecResult<Child> {
    let EvaluatedCmdArgs { env_vars, args } = args;

    let mut args_str = vec![];

    for (arg, arg_at) in args {
        append_cmd_arg_as_string(arg, arg_at, &mut args_str, ctx)?;
    }

    let cmd_path = treat_cwd_raw(name, ctx)?;

    let child = Command::new(cmd_path)
        .envs(env_vars)
        .args(args_str)
        .stdin(match children.last_mut() {
            Some((child, _)) => match pipe_type.unwrap().data {
                CmdPipeType::Stdout => Stdio::from(child.stdout.take().unwrap()),
                CmdPipeType::Stderr => Stdio::from(child.stderr.take().unwrap()),
                CmdPipeType::Value => ctx.panic(
                    pipe_type.unwrap().at,
                    "values pipe is not applicable to commands (= bug in checker)",
                ),
            },
            None => Stdio::inherit(),
        })
        .stdout(
            if capture_stdout || next_pipe_type == Some(CmdPipeType::Stdout) {
                Stdio::piped()
            } else {
                Stdio::inherit()
            },
        )
        .stderr(if next_pipe_type == Some(CmdPipeType::Stderr) {
            Stdio::piped()
        } else {
            Stdio::inherit()
        })
        .spawn()
        .map_err(|err| {
            ctx.error(
                name.at,
                ExecErrorNature::CommandFailedToStart {
                    message: format!("failed to start command '{}': {err}", name.data),
                },
            )
        })?;

    Ok(child)
}

fn append_cmd_arg_as_string(
    cmd_arg_result: CmdArgResult,
    cmd_arg_result_at: CodeRange,
    args_str: &mut Vec<String>,
    ctx: &mut Context,
) -> ExecResult<()> {
    match cmd_arg_result {
        CmdArgResult::Single(value) => match value {
            CmdSingleArgResult::Basic(value) => {
                args_str.push(value_to_str(&value.value, cmd_arg_result_at, ctx)?)
            }

            CmdSingleArgResult::Flag { name, value } => {
                let name = match &name.data {
                    CmdFlagNameArg::Short(short) => format!("-{short}"),
                    CmdFlagNameArg::Long(long) => format!("--{long}"),
                };

                match value {
                    Some(FlagArgValueResult { value, value_sep }) => {
                        let value = value_to_str(&value.value, value.from, ctx)?;

                        match value_sep {
                            FlagValueSeparator::Space => {
                                args_str.push(name);
                                args_str.push(value);
                            }

                            FlagValueSeparator::Equal => args_str.push(format!("{name}={value}")),
                        }
                    }

                    None => {
                        args_str.push(name);
                    }
                }
            }

            CmdSingleArgResult::RestSeparator(_) => {
                // TODO: make this a constant in the parser or something
                args_str.push("--".to_owned());
            }
        },

        CmdArgResult::Spreaded(items) => {
            for item in items {
                append_cmd_arg_as_string(
                    CmdArgResult::Single(item),
                    cmd_arg_result_at,
                    args_str,
                    ctx,
                )?;
            }
        }
    }

    Ok(())
}

fn compute_env_var_value(value: &CmdEnvVarValue, ctx: &mut Context) -> ExecResult<String> {
    match value {
        CmdEnvVarValue::Raw(raw) => treat_cwd_raw(raw, ctx),
        CmdEnvVarValue::ComputedString(computed_str) => eval_computed_string(computed_str, ctx),
        CmdEnvVarValue::Expr(expr) => value_to_str(&eval_expr(&expr.data, ctx)?, expr.at, ctx),
    }
}

pub fn eval_cmd_arg(arg: &CmdArg, ctx: &mut Context) -> ExecResult<CmdArgResult> {
    match arg {
        CmdArg::ValueMaking(value_making) => eval_cmd_value_making_arg(value_making, ctx)
            .map(|value| CmdArgResult::Single(CmdSingleArgResult::Basic(value))),

        CmdArg::RestSeparator(eaten) => Ok(CmdArgResult::Single(
            CmdSingleArgResult::RestSeparator(*eaten),
        )),

        CmdArg::Flag(CmdFlagArg { name, value }) => {
            Ok(CmdArgResult::Single(CmdSingleArgResult::Flag {
                name: name.clone(),
                value: value
                    .as_ref()
                    .map(|CmdFlagValueArg { value, value_sep }| {
                        eval_cmd_value_making_arg(&value.data, ctx).map(|value| {
                            FlagArgValueResult {
                                value,
                                value_sep: *value_sep,
                            }
                        })
                    })
                    .transpose()?,
            }))
        }

        CmdArg::SpreadVar(var_name) => {
            let var = ctx.get_visible_var(var_name).unwrap_or_else(|| {
                ctx.panic(
                    var_name.at,
                    "(spread) variable was not found (= bug in checker)",
                )
            });

            let var_value = var.value.read(var_name.at);

            let value = var_value.as_ref().ok_or_else(|| {
                ctx.error(
                    var_name.at,
                    "trying to use variable before it is assigned a value",
                )
            })?;

            let RuntimeValue::ArgSpread(items) = &value.value else {
                return Err(ctx.error(
                    var_name.at,
                    format!(
                        "expected a spread value, found a {}",
                        value
                            .value
                            .get_type()
                            .render_colored(ctx, PrettyPrintOptions::inline())
                    ),
                ));
            };

            Ok(CmdArgResult::Spreaded(Vec::clone(items)))
        }
    }
}

pub fn eval_cmd_value_making_arg(
    arg: &CmdValueMakingArg,
    ctx: &mut Context,
) -> ExecResult<LocatedValue> {
    let (value_at, value) = match arg {
        CmdValueMakingArg::LiteralValue(lit_val) => (lit_val.at, eval_literal_value(&lit_val.data)),

        CmdValueMakingArg::ComputedString(computed_str) => (
            computed_str.at,
            RuntimeValue::String(eval_computed_string(computed_str, ctx)?),
        ),

        CmdValueMakingArg::VarName(name) => (
            name.at,
            ctx.get_visible_var(name)
                .unwrap_or_else(|| ctx.panic(name.at, "variable was not found (= bug in checker)"))
                .value
                .read(name.at)
                .as_ref()
                .ok_or_else(|| {
                    ctx.error(
                        name.at,
                        "trying to use variable before it is assigned a value",
                    )
                })?
                .value
                .clone(),
        ),

        CmdValueMakingArg::ParenExpr(expr) => (expr.at, eval_expr(&expr.data, ctx)?),

        CmdValueMakingArg::CmdCall(call) => (
            call.at,
            RuntimeValue::String(run_cmd(call, ctx, true)?.0.unwrap()),
        ),

        CmdValueMakingArg::Raw(raw) => (raw.at, RuntimeValue::String(treat_cwd_raw(raw, ctx)?)),
    };

    Ok(LocatedValue::new(value, RuntimeCodeRange::Parsed(value_at)))
}

fn treat_cwd_raw(raw: &Eaten<String>, ctx: &Context) -> ExecResult<String> {
    let home_dir = || {
        ctx.home_dir()
            .ok_or_else(|| ctx.error(raw.at, "home directory was not defined in context"))?
            .to_str()
            .ok_or_else(|| {
                ctx.error(
                    raw.at,
                    "home directory path contains invalid UTF-8 characters",
                )
            })
    };

    let out = if raw.data == "~" {
        home_dir()?.to_owned()
    } else if let Some(rest) = raw
        .data
        .strip_prefix("~/")
        .or_else(|| raw.data.strip_prefix("~\\"))
    {
        format!("{}{MAIN_SEPARATOR}{rest}", home_dir()?)
    } else {
        raw.data.clone()
    };

    Ok(out)
}

#[derive(Debug, Clone)]
pub enum CmdArgResult {
    Single(CmdSingleArgResult),
    Spreaded(Vec<CmdSingleArgResult>),
}

#[derive(Debug, Clone)]
pub enum CmdSingleArgResult {
    Basic(LocatedValue),
    Flag {
        name: Eaten<CmdFlagNameArg>,
        value: Option<FlagArgValueResult>,
    },
    RestSeparator(Eaten<()>),
}

#[derive(Debug, Clone)]
pub struct FlagArgValueResult {
    pub value: LocatedValue,
    pub value_sep: FlagValueSeparator,
}

#[derive(Debug)]
pub enum CmdEvalArg {
    Value(RuntimeValue),
    Flag {
        name: Eaten<String>,
        value: Option<RuntimeValue>,
    },
}
