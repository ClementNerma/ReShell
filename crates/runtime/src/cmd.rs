use std::{
    borrow::Cow,
    collections::HashMap,
    process::{Child, Command, Stdio},
};

use parsy::{CodeRange, Eaten};
use reshell_parser::ast::{
    CmdArg, CmdCall, CmdEnvVar, CmdEnvVarValue, CmdPath, CmdPipe, CmdPipeType, Expr, FnCall,
    FnCallArg, RuntimeCodeRange, SingleCmdCall,
};

use crate::{
    context::{Context, DepsScopeCreationData, ScopeContent},
    display::value_to_str,
    errors::{ExecErrorContent, ExecResult},
    expr::{eval_computed_string, eval_expr, eval_literal_value},
    functions::{call_fn, call_fn_value, FnCallResult, FnPossibleCallArgs},
    pretty::{PrettyPrintOptions, PrettyPrintable},
    values::{LocatedValue, RuntimeCmdAlias, RuntimeValue},
};

// TODO: this should be able to return an InstrRet
pub fn run_cmd(
    call: &Eaten<CmdCall>,
    ctx: &mut Context,
    capture_stdout: bool,
) -> ExecResult<(Option<String>, Option<LocatedValue>)> {
    let CmdCall { base, pipes } = &call.data;

    let chain = [(base, None)]
        .into_iter()
        .chain(
            pipes
                .iter()
                .map(|CmdPipe { pipe_type, cmd }| (cmd, Some(pipe_type))),
        )
        .map(|(call, pipe_type)| {
            single_call_to_chain_el(call, ctx).map(|chain_el| (chain_el, pipe_type.copied()))
        })
        .collect::<Result<Vec<_>, _>>()?;

    let mut children: Vec<(Child, CodeRange)> = vec![];

    let pipe_types = chain
        .iter()
        .map(|(_, pipe_type)| *pipe_type)
        .collect::<Vec<_>>();

    let mut last_return_value = None;

    for (i, (item, pipe_type)) in chain.into_iter().enumerate() {
        let next_pipe_type = pipe_types
            .get(i + 1)
            .and_then(|pipe_type| pipe_type.map(|pipe_type| pipe_type.data));

        let CmdChainEl {
            content,
            aliases_deps_scopes,
        } = item;

        let aliases_deps_scopes_len = aliases_deps_scopes.len();

        for alias_dep_scope in aliases_deps_scopes {
            ctx.create_and_push_scope_with_deps(
                RuntimeCodeRange::CodeRange(call.at),
                DepsScopeCreationData::Retrieved(alias_dep_scope),
                ScopeContent::new(),
                ctx.generate_parent_scopes_list(),
                None,
            );
        }

        match content {
            CmdChainElContent::NonCmd(call) => {
                // for now (TODO)
                assert!(!pipe_types.iter().any(|pipe_type| pipe_type.is_some()));

                let NonCmdCall {
                    at,
                    nature,
                    call_args,
                } = call;

                let result = match nature {
                    NonCmdCallNature::Function { is_var_name, name } => {
                        // TODO: handle non-success cases (e.g. Thrown)
                        call_fn(
                            &Eaten::ate(
                                at,
                                FnCall {
                                    is_var_name,
                                    name,
                                    call_args,
                                },
                            ),
                            ctx,
                        )?
                    }

                    NonCmdCallNature::Expr(expr) => {
                        let result = eval_expr(&expr.data, ctx)?;

                        let func = match result {
                            RuntimeValue::Function(func) => func,

                            _ => {
                                return Err(ctx.error(
                                    expr.at,
                                    format!(
                                        "expected a function, got a {}",
                                        result
                                            .get_type()
                                            .render_colored(ctx, PrettyPrintOptions::inline())
                                    ),
                                ));
                            }
                        };

                        call_fn_value(
                            RuntimeCodeRange::CodeRange(at),
                            &func,
                            FnPossibleCallArgs::Parsed(&call_args),
                            ctx,
                        )?
                    }
                };

                match result {
                    FnCallResult::Success { returned } => {
                        if let Some(returned) = returned {
                            // Only if there is no function to run afterwards
                            if next_pipe_type.is_none() {
                                last_return_value = Some(returned);
                            }
                        }
                    }

                    FnCallResult::Thrown(_) => todo!(),
                }
            }

            CmdChainElContent::Cmd(item) => {
                let CmdCallEl {
                    path,
                    env_vars,
                    args,
                    at,
                } = item;

                let cmd_path = treat_cwd_raw(&path, ctx)?;

                let child = Command::new(&cmd_path)
                    .envs(env_vars)
                    .args(args.clone())
                    .stdin(match children.last_mut() {
                        Some((child, _)) => match pipe_type {
                            Some(pipe_type) => match pipe_type.data {
                                CmdPipeType::Stdout => Stdio::from(child.stdout.take().unwrap()),
                                CmdPipeType::Stderr => Stdio::from(child.stderr.take().unwrap()),
                                // CmdPipeType::Both => todo!(),
                            },
                            None => unreachable!(),
                        },
                        None => Stdio::inherit(),
                    })
                    .stdout(
                        if capture_stdout || next_pipe_type == Some(CmdPipeType::Stdout) {
                            // println!("(piped) {}", name.data);
                            Stdio::piped()
                        } else {
                            // println!("(inherit) {}", name.data);
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
                            path.at,
                            ExecErrorContent::CommandFailed {
                                message: format!("failed to start command '{}': {err}", path.data),
                                exit_status: None,
                            },
                        )
                    })?;

                children.push((child, at));
            }
        }

        for _ in 0..aliases_deps_scopes_len {
            ctx.pop_scope();
        }
    }

    let mut last_output = None;

    for (child, at) in children.into_iter().rev() {
        let output = child.wait_with_output().map_err(|err| {
            ctx.error(
                at,
                ExecErrorContent::CommandFailed {
                    message: format!("command failed: {err}"),
                    exit_status: None,
                },
            )
        })?;

        if !output.status.success() {
            return Err(ctx.error(
                at,
                ExecErrorContent::CommandFailed {
                    message: format!(
                        "command failed{}",
                        match output.status.code() {
                            Some(code) => format!(" (exit status: {code})"),
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

    Ok((captured, last_return_value))
}

fn single_call_to_chain_el(
    call: &Eaten<SingleCmdCall>,
    ctx: &mut Context,
) -> ExecResult<CmdChainEl> {
    let inner = |call: &Eaten<SingleCmdCall>, ctx: &mut Context| -> ExecResult<CmdChainElContent> {
        let SingleCmdCall {
            env_vars,
            path,
            args,
        } = &call.data;

        if let Some(call) = check_if_cmd_is_fn(call, ctx)? {
            return Ok(CmdChainElContent::NonCmd(call));
        }

        let mut eval_env_vars = HashMap::with_capacity(env_vars.data.len());

        for env_var in &env_vars.data {
            let CmdEnvVar { name, value } = &env_var.data;

            eval_env_vars.insert(name.data.clone(), compute_env_var_value(&value.data, ctx)?);
        }

        let mut eval_args = Vec::with_capacity(args.data.len());

        for arg in &args.data {
            match eval_cmd_arg(&arg.data, ctx)? {
                CmdArgResult::Single(value) => {
                    eval_args.push(value_to_str(&value, arg.at, ctx)?);
                }

                CmdArgResult::Spreaded(values) => {
                    for value in values {
                        eval_args.push(value_to_str(&value, arg.at, ctx)?);
                    }
                }
            }
        }

        Ok(CmdChainElContent::Cmd(CmdCallEl {
            path: match &path.data {
                CmdPath::RawString(raw) => raw.clone(),
                CmdPath::ComputedString(str) => str.forge_here(eval_computed_string(str, ctx)?),
                CmdPath::Direct(direct) => direct.clone(),
                CmdPath::Expr(_) => unreachable!(),
            },
            env_vars: eval_env_vars,
            args: eval_args,
            at: call.at,
        }))
    };

    let (call, cmd_aliases) = match develop_aliases(call, ctx) {
        Some((call, cmd_aliases)) => (Cow::Owned(call), cmd_aliases),
        None => (Cow::Borrowed(call), vec![]),
    };

    let cmd_aliases_len = cmd_aliases.len();

    for cmd_alias in cmd_aliases {
        let RuntimeCmdAlias {
            name_declared_at,
            alias_content: _,
            parent_scopes,
            captured_deps,
        } = cmd_alias;

        ctx.create_and_push_scope_with_deps(
            RuntimeCodeRange::CodeRange(name_declared_at),
            DepsScopeCreationData::CapturedDeps(captured_deps),
            // This scope won't be used anyway, it is only here to provide access to the deps scope
            ScopeContent::new(),
            parent_scopes,
            None,
        );
    }

    inner(&call, ctx).map(|content| CmdChainEl {
        content,
        aliases_deps_scopes: (0..cmd_aliases_len)
            .map(|_| ctx.pop_scope_and_get_deps().unwrap())
            .collect(),
    })
}

#[derive(Debug)]
struct CmdChainEl {
    content: CmdChainElContent,
    aliases_deps_scopes: Vec<ScopeContent>,
}

#[derive(Debug)]
enum CmdChainElContent {
    Cmd(CmdCallEl),
    NonCmd(NonCmdCall),
}

#[derive(Debug)]
struct NonCmdCall {
    at: CodeRange,
    nature: NonCmdCallNature,
    call_args: Eaten<Vec<Eaten<FnCallArg>>>,
}

#[derive(Debug)]
enum NonCmdCallNature {
    Function {
        is_var_name: bool,
        name: Eaten<String>,
    },
    Expr(Eaten<Expr>),
}

#[derive(Debug)]
struct CmdCallEl {
    path: Eaten<String>,
    env_vars: HashMap<String, String>,
    args: Vec<String>,
    at: CodeRange,
}

fn compute_env_var_value(value: &CmdEnvVarValue, ctx: &mut Context) -> ExecResult<String> {
    match value {
        CmdEnvVarValue::Raw(raw) => treat_cwd_raw(raw, ctx),
        CmdEnvVarValue::ComputedString(computed_str) => eval_computed_string(computed_str, ctx),
        CmdEnvVarValue::Expr(expr) => value_to_str(&eval_expr(&expr.data, ctx)?, expr.at, ctx),
    }
}

fn develop_aliases(
    call: &Eaten<SingleCmdCall>,
    ctx: &mut Context,
) -> Option<(Eaten<SingleCmdCall>, Vec<RuntimeCmdAlias>)> {
    let mut developed_call = Cow::Borrowed(call);
    let mut aliases = vec![];

    while let Some(cmd_path) = match &developed_call.data.path.data {
        CmdPath::RawString(raw) => Some(raw),
        CmdPath::ComputedString(_) | CmdPath::Expr(_) | CmdPath::Direct(_) => None,
    } {
        let Some(cmd_alias) = ctx
            .visible_scopes()
            .find_map(|scope| scope.cmd_aliases.get(&cmd_path.data))
        else {
            break;
        };

        let mut call = developed_call.into_owned();

        let SingleCmdCall {
            env_vars,
            path,
            args,
        } = &*cmd_alias.alias_content;

        call.data.path.change_value(path.data.clone());

        call.data
            .env_vars
            .data
            .splice(0..0, env_vars.data.iter().cloned());

        call.data.args.data.splice(0..0, args.data.iter().cloned());

        // TODO: avoid cloning (use a [`GcReadOnlyCell`])
        aliases.push(cmd_alias.clone());

        developed_call = Cow::Owned(call);
    }

    match developed_call {
        Cow::Borrowed(_) => None,
        Cow::Owned(call) => Some((call, aliases)),
    }
}

fn check_if_cmd_is_fn(
    call: &Eaten<SingleCmdCall>,
    ctx: &mut Context,
) -> ExecResult<Option<NonCmdCall>> {
    let SingleCmdCall {
        path,
        args,
        env_vars,
    } = &call.data;

    match &path.data {
        CmdPath::Direct(_) => Ok(None),
        CmdPath::ComputedString(_) => Ok(None),
        CmdPath::RawString(raw) => match ctx.get_visible_fn(raw) {
            None => Ok(None),
            Some(_) => {
                if !env_vars.data.is_empty() {
                    return Err(ctx.error(
                        env_vars.at,
                        "inline environment variables are not supported for function calls",
                    ));
                }

                Ok(Some(NonCmdCall {
                    at: call.at,

                    nature: NonCmdCallNature::Function {
                        is_var_name: false,
                        name: raw.clone(),
                    },

                    call_args: Eaten::ate(
                        args.at,
                        args.data
                            .iter()
                            .map(|arg| Eaten::ate(arg.at, FnCallArg::CmdArg(arg.clone())))
                            .collect(),
                    ),
                }))
            }
        },

        CmdPath::Expr(expr) => {
            if !env_vars.data.is_empty() {
                return Err(ctx.error(
                    env_vars.at,
                    "inline environment variables are not supported for function calls",
                ));
            }

            Ok(Some(NonCmdCall {
                at: call.at,

                nature: NonCmdCallNature::Expr(expr.map_ref(|expr| *expr.clone())),

                call_args: Eaten::ate(
                    args.at,
                    args.data
                        .iter()
                        .map(|arg| Eaten::ate(arg.at, FnCallArg::CmdArg(arg.clone())))
                        .collect(),
                ),
            }))
        }
    }
}

pub fn eval_cmd_arg(arg: &CmdArg, ctx: &mut Context) -> ExecResult<CmdArgResult> {
    match arg {
        CmdArg::LiteralValue(lit_val) => {
            Ok(CmdArgResult::Single(eval_literal_value(&lit_val.data)))
        }

        CmdArg::ComputedString(computed_str) => Ok(CmdArgResult::Single(
            eval_computed_string(computed_str, ctx).map(RuntimeValue::String)?,
        )),

        CmdArg::VarName(name) => Ok(CmdArgResult::Single(
            ctx.get_visible_var(name)
                .ok_or_else(|| ctx.error(name.at, "variable was not found"))?
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
        )),

        CmdArg::FnAsValue(name) => Ok(CmdArgResult::Single(RuntimeValue::Function(
            ctx.get_visible_fn_value(name)?.clone(),
        ))),

        CmdArg::ParenExpr(expr) => Ok(CmdArgResult::Single(eval_expr(&expr.data, ctx)?)),

        CmdArg::CmdCall(call) => Ok(CmdArgResult::Single(RuntimeValue::String(
            run_cmd(call, ctx, true)?.0.unwrap(),
        ))),

        CmdArg::Raw(raw) => Ok(CmdArgResult::Single(RuntimeValue::String(treat_cwd_raw(
            raw, ctx,
        )?))),

        CmdArg::SpreadVar(var_name) => {
            let var = ctx
                .get_visible_var(var_name)
                .ok_or_else(|| ctx.error(var_name.at, "variable was not found"))?;

            let var_value = var.value.read(var_name.at);

            let value = var_value.as_ref().ok_or_else(|| {
                ctx.error(
                    var_name.at,
                    "trying to use variable before it is assigned a value",
                )
            })?;

            let RuntimeValue::List(items) = &value.value else {
                return Err(ctx.error(
                    var_name.at,
                    format!(
                        "expected a list to spread, found a {}",
                        value
                            .value
                            .get_type()
                            .render_colored(ctx, PrettyPrintOptions::inline())
                    ),
                ));
            };

            let items = items.read(var_name.at).iter().cloned().collect();

            Ok(CmdArgResult::Spreaded(items))
        }
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
    } else if let Some(rest) = raw.data.strip_prefix("~/") {
        format!("{}/{rest}", home_dir()?)
    } else {
        raw.data.clone()
    };

    Ok(out)
}

pub enum CmdArgResult {
    Single(RuntimeValue),
    Spreaded(Vec<RuntimeValue>),
}
