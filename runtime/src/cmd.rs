use std::{
    collections::HashMap,
    process::{Child, Command, Stdio},
};

use parsy::{CodeRange, Eaten};
use reshell_parser::ast::{
    CmdArg, CmdCall, CmdEnvVar, CmdEnvVarValue, CmdPath, CmdPipe, CmdPipeType, FnCall, FnCallArg,
    SingleCmdCall,
};

use crate::{
    display::{readable_value_type, value_to_str},
    errors::{ExecErrorContent, ExecResult},
    expr::{eval_computed_string, eval_expr, eval_literal_value},
    functions::call_fn,
    scoping::Context,
    values::RuntimeValue,
};

pub fn run_cmd(
    call: &Eaten<CmdCall>,
    ctx: &mut Context,
    capture_stdout: bool,
) -> ExecResult<Option<String>> {
    let CmdCall { base, pipes } = &call.data;

    let chain = [(base, None)]
        .into_iter()
        .chain(
            pipes
                .iter()
                .map(|CmdPipe { cmd, pipe_type }| (cmd, Some(*pipe_type))),
        )
        .collect::<Vec<_>>();

    let mut found = None;

    for (call, _) in &chain {
        if let Some(call) = check_if_cmd_is_fn(call, ctx, capture_stdout)? {
            if !pipes.is_empty() {
                return Err(ctx.error(call.data.name.at, "pipes cannot be applied on functions"));
            }

            assert!(found.is_none());
            found = Some(call);
        }
    }

    if let Some(call) = found {
        call_fn(&call, ctx)?;
        return Ok(None);
    }

    #[derive(Debug)]
    struct CmdChainEl {
        path: Eaten<String>,
        env_vars: HashMap<String, String>,
        args: Vec<String>,
        pipe_type: Option<Eaten<CmdPipeType>>,
        at: CodeRange,
    }

    let chain: Vec<CmdChainEl> = chain
        .iter()
        .map(|(single_call, pipe_type)| {
            let SingleCmdCall {
                env_vars,
                path,
                args,
            } = &single_call.data;

            let mut eval_env_vars = HashMap::with_capacity(single_call.data.env_vars.data.len());

            for env_var in &env_vars.data {
                let CmdEnvVar { name, value } = &env_var.data;

                let value = match &value.data {
                    CmdEnvVarValue::Raw(raw) => raw.data.clone(),
                    CmdEnvVarValue::ComputedString(computed_str) => {
                        eval_computed_string(computed_str, ctx)?
                    }
                    CmdEnvVarValue::Expr(expr) => {
                        value_to_str(&eval_expr(&expr.data, ctx)?, expr.at, ctx)?
                    }
                };

                eval_env_vars.insert(name.data.clone(), value);
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

            Ok(CmdChainEl {
                path: match &path.data {
                    CmdPath::Raw(raw) => raw.clone(),
                    CmdPath::ComputedString(str) => str.forge_here(eval_computed_string(str, ctx)?),
                },
                env_vars: eval_env_vars,
                args: eval_args,
                pipe_type: *pipe_type,
                at: single_call.at,
            })
        })
        .collect::<Result<_, _>>()?;

    let mut children: Vec<(Child, CodeRange)> = vec![];

    for (item, next_item) in chain
        .iter()
        .zip(chain.iter().skip(1).map(Some).chain([None].into_iter()))
    {
        let CmdChainEl {
            path,
            env_vars,
            args,
            pipe_type,
            at,
        } = item;

        let next_pipe_type = next_item
            .and_then(|item| item.pipe_type)
            .map(|pipe_type| pipe_type.data);

        let child = Command::new(&path.data)
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

        children.push((child, *at));
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

    Ok(if capture_stdout {
        // TODO: what to do if not valid UTF8?
        Some(String::from_utf8_lossy(&last_output.unwrap()).into_owned())
    } else {
        None
    })
}

fn check_if_cmd_is_fn(
    call: &Eaten<SingleCmdCall>,
    ctx: &mut Context,
    capture_stdout: bool,
) -> ExecResult<Option<Eaten<FnCall>>> {
    let SingleCmdCall {
        path,
        args,
        env_vars,
    } = &call.data;

    let (name, is_var) = match &path.data {
        CmdPath::Raw(raw) => match ctx.get_fn(raw) {
            Some(_) => (raw, false),
            None => return Ok(None),
        },
        CmdPath::ComputedString(_) => return Ok(None),
    };

    if !env_vars.data.is_empty() {
        return Err(ctx.error(
            env_vars.at,
            "inline environment variables are not supported for function calls",
        ));
    }

    if capture_stdout {
        return Err(ctx.error(call.at, "cannot capture output of a function call"));
    }

    let fn_call = FnCall {
        is_var_name: is_var,
        name: name.clone(),
        call_args: Eaten {
            at: args.at,
            data: args
                .data
                .iter()
                .map(|arg| FnCallArg::CmdArg(arg.clone()))
                .collect(),
        },
    };

    Ok(Some(Eaten {
        at: call.at,
        data: fn_call,
    }))
}

pub fn eval_cmd_arg(arg: &CmdArg, ctx: &mut Context) -> ExecResult<CmdArgResult> {
    match arg {
        CmdArg::LiteralValue(lit_val) => {
            Ok(CmdArgResult::Single(eval_literal_value(&lit_val.data)))
        }
        CmdArg::ComputedString(computed_str) => Ok(CmdArgResult::Single(
            eval_computed_string(computed_str, ctx).map(RuntimeValue::String)?,
        )),
        CmdArg::VarName(name) => Ok(CmdArgResult::Single(ctx.get_var_value(name)?.clone())),
        CmdArg::FnAsValue(name) => Ok(CmdArgResult::Single(RuntimeValue::Function(
            ctx.get_fn_value(name)?.clone(),
        ))),
        CmdArg::ParenExpr(expr) => Ok(CmdArgResult::Single(eval_expr(&expr.data, ctx)?)),
        CmdArg::CmdCall(call) => Ok(CmdArgResult::Single(RuntimeValue::String(
            run_cmd(call, ctx, true)?.unwrap(),
        ))),
        CmdArg::Raw(raw) => Ok(CmdArgResult::Single(RuntimeValue::String(raw.data.clone()))),
        CmdArg::SpreadVar(var_name) => {
            let value = ctx.get_var_value(var_name)?;

            let RuntimeValue::List(items) = &value else {
                return Err(ctx.error(var_name.at, format!("expected a list to spread, found a {}", readable_value_type(value))));
            };

            Ok(CmdArgResult::Spreaded(items.clone()))
        }
    }
}

pub enum CmdArgResult {
    Single(RuntimeValue),
    Spreaded(Vec<RuntimeValue>),
}
