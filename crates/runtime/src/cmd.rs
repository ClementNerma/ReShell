use std::{
    collections::HashMap,
    path::MAIN_SEPARATOR,
    process::{Child, Command, Stdio},
};

use parsy::{CodeRange, Eaten};
use reshell_checker::output::DevelopedSingleCmdCall;
use reshell_parser::ast::{
    CmdArg, CmdCall, CmdComputedString, CmdComputedStringPiece, CmdEnvVar, CmdFlagArg,
    CmdFlagNameArg, CmdFlagValueArg, CmdPath, CmdPipe, CmdPipeType, CmdValueMakingArg,
    FlagValueSeparator, RuntimeCodeRange, SingleCmdCall,
};

use crate::{
    context::{Context, DepsScopeCreationData},
    errors::{ExecErrorNature, ExecResult},
    expr::{eval_computed_string, eval_expr, eval_literal_value},
    functions::{call_fn_value, FnCallType, FnPossibleCallArgs},
    gc::GcReadOnlyCell,
    pretty::{PrettyPrintOptions, PrettyPrintable},
    values::{value_to_str, LocatedValue, RuntimeCmdAlias, RuntimeFnValue, RuntimeValue},
};

#[derive(Clone, Copy)]
pub struct CmdExecParams {
    pub capture: Option<CmdPipeCapture>,
}

#[derive(Clone, Copy)]
pub enum CmdPipeCapture {
    Stdout,
    Stderr,
    Both,
}

pub fn run_cmd(
    call: &Eaten<CmdCall>,
    ctx: &mut Context,
    params: CmdExecParams,
) -> ExecResult<CmdExecResult> {
    let CmdExecParams { capture } = params;

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
        if capture.is_some() {
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

            let return_value = call_fn_value(
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

            last_return_value = return_value.map(|loc_value| {
                LocatedValue::new(loc_value.value, RuntimeCodeRange::Parsed(call_at))
            });

            if i + 1 < chain_len && last_return_value.is_none() {
                return Err(ctx.error(call_at, "piped function did not return a value"));
            }
        }

        return Ok(CmdExecResult::Returned(last_return_value));
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
            ExecCmdArgs {
                name: &cmd_name,
                args,
                pipe_type,
                next_pipe_type,
                params,
            },
            &mut children,
            ctx,
        )?;

        children.push((child, call_at));
    }

    let mut last_output = None;

    for (i, (child, at)) in children.into_iter().rev().enumerate() {
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

        if i == 0 {
            last_output = Some(output.stdout);
        }
    }

    let captured = if capture.is_some() {
        // Invalid UTF-8 output will be handled with "unknown" symbols
        let mut out = String::from_utf8_lossy(&last_output.unwrap()).into_owned();

        if out.ends_with('\n') {
            out.pop();
        }

        Some(out)
    } else {
        None
    };

    Ok(match captured {
        Some(string) => CmdExecResult::Captured(string),
        None => CmdExecResult::None,
    })
}

pub enum CmdExecResult {
    Returned(Option<LocatedValue>),
    Captured(String),
    None,
}

impl CmdExecResult {
    pub fn as_returned(self) -> Option<Option<LocatedValue>> {
        match self {
            CmdExecResult::Returned(inner) => Some(inner),
            CmdExecResult::Captured(_) => None,
            CmdExecResult::None => None,
        }
    }

    pub fn as_captured(self) -> Option<String> {
        match self {
            CmdExecResult::Returned(_) => None,
            CmdExecResult::Captured(inner) => Some(inner),
            CmdExecResult::None => None,
        }
    }
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
            None,
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

        let LocatedValue { value, from } = eval_cmd_value_making_arg(&value.data, ctx)?;

        out.env_vars.insert(
            name.data.clone(),
            value_to_str(
                &value,
                "environment variables can have take stringifyable values",
                from,
                ctx,
            )?,
        );
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

            CmdPath::CmdComputedString(cc_str) => {
                assert!(cc_str.data.only_literal().is_some());

                let string = eval_cmd_computed_string(cc_str, ctx)?;

                GcReadOnlyCell::clone(ctx.get_visible_fn_value(&cc_str.forge_here(string))?)
            }

            CmdPath::Direct(_) | CmdPath::ComputedString(_) => {
                unreachable!()
            }
        };

        return Ok(EvaluatedCmdTarget::Function(func));
    }

    Ok(EvaluatedCmdTarget::ExternalCommand(match &cmd_path.data {
        CmdPath::Direct(cc_str) => cc_str.forge_here(eval_cmd_computed_string(cc_str, ctx)?),
        CmdPath::Expr(_) => unreachable!(),
        CmdPath::ComputedString(c_str) => c_str.forge_here(eval_computed_string(c_str, ctx)?),
        CmdPath::CmdComputedString(cc_str) => {
            cc_str.forge_here(eval_cmd_computed_string(cc_str, ctx)?)
        }
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

struct ExecCmdArgs<'a> {
    name: &'a Eaten<String>,
    args: EvaluatedCmdArgs,
    pipe_type: Option<Eaten<CmdPipeType>>,
    next_pipe_type: Option<CmdPipeType>,
    params: CmdExecParams,
}

fn exec_cmd(
    args: ExecCmdArgs,
    children: &mut [(Child, CodeRange)],
    ctx: &mut Context,
) -> ExecResult<Child> {
    let ExecCmdArgs {
        name,
        args,
        pipe_type,
        next_pipe_type,
        params,
    } = args;

    let CmdExecParams { capture } = params;

    let EvaluatedCmdArgs { env_vars, args } = args;

    let mut args_str = vec![];

    for (arg, arg_at) in args {
        append_cmd_arg_as_string(arg, arg_at, &mut args_str, ctx)?;
    }

    // Determine the command name (or path)
    let cmd_path = treat_cwd_raw(name, ctx)?;

    // Resolve the command name to a binary's path
    let cmd_path = ctx
        .binaries_resolver()
        .resolve_binary_path(&cmd_path)
        .map_err(|err| ctx.error(name.at, err.to_string()))?;

    // This instruction both canonicalizes AND simplifies the path (to avoid UNC like '\\?\C:\...' on Windows)
    // This is required because the command path is transmitted as the program's first argument on most platforms,
    // and some programs may use it to refer as themselves while in the same time not supporting UNC paths
    let cmd_path = dunce::canonicalize(&cmd_path)
        // A command may be able to be run without the user actually being able to access the file itself
        // e.g. Windows Store applications' binaries
        .unwrap_or(cmd_path);

    // Actually run the command
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
            if next_pipe_type == Some(CmdPipeType::Stdout)
                || matches!(capture, Some(CmdPipeCapture::Stdout | CmdPipeCapture::Both))
            {
                Stdio::piped()
            } else {
                Stdio::inherit()
            },
        )
        .stderr(
            if next_pipe_type == Some(CmdPipeType::Stderr)
                || matches!(capture, Some(CmdPipeCapture::Stderr | CmdPipeCapture::Both))
            {
                Stdio::piped()
            } else {
                Stdio::inherit()
            },
        )
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
            CmdSingleArgResult::Basic(value) => args_str.push(value_to_str(
                &value.value,
                "arguments to external commands must be stringifyable",
                cmd_arg_result_at,
                ctx,
            )?),

            CmdSingleArgResult::Flag { name, value } => {
                let name = name.data.back_to_string();

                match value {
                    Some(FlagArgValueResult { value, value_sep }) => {
                        let value = value_to_str(
                            &value.value,
                            "arguments to external commands must be stringifyable",
                            value.from,
                            ctx,
                        )?;

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

pub fn eval_cmd_arg(arg: &CmdArg, ctx: &mut Context) -> ExecResult<CmdArgResult> {
    match arg {
        CmdArg::ValueMaking(value_making) => eval_cmd_value_making_arg(value_making, ctx)
            .map(|value| CmdArgResult::Single(CmdSingleArgResult::Basic(value))),

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

            match &var_value.value {
                RuntimeValue::List(items) => {
                    let spreaded = items
                        .read_promise_no_write()
                        .iter()
                        .map(|item| {
                            value_to_str(
                                item,
                                "spreaded arguments to external commands must be stringifyable",
                                var_value.from,
                                ctx,
                            )
                            .map(|str| {
                                CmdSingleArgResult::Basic(LocatedValue::new(
                                    RuntimeValue::String(str),
                                    var_value.from,
                                ))
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    Ok(CmdArgResult::Spreaded(spreaded))
                }
                RuntimeValue::ArgSpread(spread) => Ok(CmdArgResult::Spreaded(Vec::clone(spread))),
                _ => Err(ctx.error(
                    var_name.at,
                    format!(
                        "expected a spread value, found a {}",
                        var_value
                            .value
                            .get_type()
                            .render_colored(ctx, PrettyPrintOptions::inline())
                    ),
                )),
            }
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

        CmdValueMakingArg::CmdComputedString(computed_str) => (
            computed_str.at,
            RuntimeValue::String(eval_cmd_computed_string(computed_str, ctx)?),
        ),

        CmdValueMakingArg::ParenExpr(expr) => (expr.at, eval_expr(&expr.data, ctx)?),

        CmdValueMakingArg::CmdOutput(call) => {
            let cmd_result = run_cmd(
                call,
                ctx,
                CmdExecParams {
                    capture: Some(CmdPipeCapture::Stdout),
                },
            )?;

            (
                call.at,
                RuntimeValue::String(cmd_result.as_captured().unwrap()),
            )
        }
    };

    Ok(LocatedValue::new(value, RuntimeCodeRange::Parsed(value_at)))
}

pub fn eval_cmd_computed_string(
    value: &Eaten<CmdComputedString>,
    ctx: &mut Context,
) -> ExecResult<String> {
    value
        .data
        .pieces
        .iter()
        .map(|piece| eval_cmd_computed_string_piece(piece, ctx))
        .collect::<Result<String, _>>()
}

fn eval_cmd_computed_string_piece(
    piece: &Eaten<CmdComputedStringPiece>,
    ctx: &mut Context,
) -> ExecResult<String> {
    match &piece.data {
        CmdComputedStringPiece::Literal(str) => Ok(str.clone()),
        CmdComputedStringPiece::Escaped(char) => Ok(char.to_string()),
        CmdComputedStringPiece::Variable(var_name) => Ok(value_to_str(
            &ctx.get_visible_var(var_name)
                .unwrap_or_else(|| {
                    ctx.panic(var_name.at, "variable was not found (= bug in checker)")
                })
                .value
                .read(var_name.at)
                .value,
            "only stringifyable variables can be used inside computable strings",
            var_name.at,
            ctx,
        )?),
    }
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
