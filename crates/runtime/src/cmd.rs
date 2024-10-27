use std::{
    collections::HashMap,
    io::Write,
    path::MAIN_SEPARATOR,
    process::{Child, Command, Stdio},
};

use parsy::{CodeRange, Eaten};
use reshell_checker::output::DevelopedSingleCmdCall;
use reshell_parser::ast::{
    CmdArg, CmdCall, CmdCallBase, CmdEnvVar, CmdExternalPath, CmdFlagArg, CmdFlagValueArg, CmdPath,
    CmdPipe, CmdPipeType, CmdRawString, CmdRawStringPiece, CmdSpreadArg, CmdValueMakingArg,
    FlagValueSeparator, FnCallNature, RuntimeCodeRange, RuntimeEaten, SingleCmdCall,
};
use reshell_shared::pretty::{PrettyPrintOptions, PrettyPrintable};

use crate::{
    context::{Context, DepsScopeCreationData},
    errors::{ExecErrorNature, ExecResult},
    expr::{eval_computed_string, eval_expr, eval_literal_value, lambda_to_value},
    functions::{call_fn_value, find_applicable_method, FnCallInfos, FnPossibleCallArgs},
    gc::GcReadOnlyCell,
    values::{
        value_to_str, CmdArgValue, CmdFlagValue, LocatedValue, RuntimeCmdAlias, RuntimeFnValue,
        RuntimeValue,
    },
};

#[derive(Clone, Copy)]
pub struct CmdExecParams {
    /// How the command's output should be captured
    pub capture: Option<CmdPipeCapture>,

    /// Hide output from non-captured pipes
    pub silent: bool,
}

/// What pipes to capture from a command's output
#[derive(Clone, Copy)]
pub enum CmdPipeCapture {
    Stdout,
    Stderr,
    // Both,
}

pub fn run_cmd(
    call: &Eaten<CmdCall>,
    ctx: &mut Context,
    params: CmdExecParams,
) -> ExecResult<CmdExecResult> {
    let CmdExecParams { capture, silent: _ } = params;

    // Interrupt before command execution if Ctrl+C was pressed
    ctx.ensure_no_ctrl_c_press(call.at)?;

    // Prepare a uniform command chain for easier processing
    let CmdCall { base, pipes } = &call.data;

    let (base, from_value) = match base {
        CmdCallBase::Expr(expr) => {
            let value = eval_expr(&expr.data, ctx)?;

            (
                None,
                Some(LocatedValue::new(RuntimeCodeRange::Parsed(expr.at), value)),
            )
        }

        CmdCallBase::SingleCmdCall(cmd_call) => (Some(cmd_call), None),
    };

    let chain = base
        .into_iter()
        .map(|base| (base, None))
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

    let mut state = from_value.map(CmdChainState::Value);

    for (i, (cmd_data, pipe_type)) in chain.into_iter().enumerate() {
        let next_pipe_type = pipe_types
            .get(i + 1)
            .and_then(|pipe_type| pipe_type.map(|pipe_type| pipe_type.data));

        let EvaluatedCmdData {
            target,
            args,
            call_at,
        } = cmd_data;

        state = Some(match target {
            EvaluatedCmdTarget::ExternalCommand(cmd_name) => {
                // TODO: deduplicate sub routines
                let (piped_string, children) = match state {
                    Some(CmdChainState::NoValue(from)) => {
                        if i > 0 {
                            return Err(ctx.error(from, "this function returned nothing ; cannot pipe it into a command's input"));
                        }

                        (None, None)
                    }

                    Some(CmdChainState::Value(loc_val)) => {
                        let input = match loc_val.value {
                            RuntimeValue::String(string) => string,

                            _ => {
                                return Err(ctx.error(
                                    loc_val.from,
                                    format!(
                                        "expected a string to pipe into the next command, instead found a: {}",
                                        loc_val
                                            .value
                                            .compute_type()
                                            .render_colored(ctx.type_alias_store(), PrettyPrintOptions::inline())
                                    )
                                ));
                            }
                        };

                        (Some(input), None)
                    }

                    Some(CmdChainState::Commands(children)) => (None, Some(children)),

                    None => {
                        assert_eq!(i, 0);

                        (None, None)
                    }
                };

                let mut children = children.unwrap_or_default();

                let child = exec_cmd(
                    ExecCmdArgs {
                        name: &cmd_name,
                        args,
                        pipe_type,
                        next_pipe_type,
                        params,
                    },
                    piped_string
                        .map(CmdInput::String)
                        .or_else(|| children.last_mut().map(|(child, _)| CmdInput::Child(child))),
                    ctx,
                )?;

                children.push((child, call_at));

                CmdChainState::Commands(children)
            }

            EvaluatedCmdTarget::Function(_) | EvaluatedCmdTarget::Method(_) => {
                if let Some(pipe_type) = pipe_type {
                    assert_eq!(pipe_type.data, CmdPipeType::ValueOrStdout);
                }

                let EvaluatedCmdArgs { env_vars, args } = args;

                assert!(env_vars.is_empty());

                let piped_value = match state {
                    Some(CmdChainState::NoValue(from)) => {
                        if i > 0 {
                            return Err(ctx.error(from, "this function returned nothing ; cannot pipe it into another function"));
                        }

                        None
                    }

                    Some(CmdChainState::Commands(children)) => {
                        let from = children.last().unwrap().1;

                        let captured = wait_for_commands_ending(
                            children,
                            Some(match pipe_type.unwrap().data {
                                CmdPipeType::Stderr => CmdPipeCapture::Stderr,
                                CmdPipeType::ValueOrStdout => CmdPipeCapture::Stdout,
                            }),
                            ctx,
                        )?
                        .unwrap();

                        Some(LocatedValue::new(
                            RuntimeCodeRange::Parsed(from),
                            RuntimeValue::String(captured),
                        ))
                    }

                    Some(CmdChainState::Value(value)) => Some(value),

                    None => None,
                };

                let func = match target {
                    EvaluatedCmdTarget::ExternalCommand(_) => unreachable!(),
                    EvaluatedCmdTarget::Function(func) => func,
                    EvaluatedCmdTarget::Method(name) => {
                        let first_arg = match &piped_value {
                            Some(value) => value,
                            None => args
                                .iter()
                                .find_map(|(arg, _)| {
                                    let arg = match arg {
                                        CmdArgResult::Single(single) => single,
                                        CmdArgResult::Spreaded(spreaded) => spreaded.first()?,
                                    };

                                    match arg {
                                        SingleCmdArgResult::Basic(value) => Some(value),
                                        SingleCmdArgResult::Flag(_) => None,
                                    }
                                })
                                .ok_or_else(|| {
                                    ctx.error(
                                        call_at,
                                        "please provide at least one argument to run the method on",
                                    )
                                })?,
                        };

                        let method = find_applicable_method(&name, &first_arg.value, ctx)?;

                        method.value.clone()
                    }
                };

                let return_value = call_fn_value(
                    RuntimeCodeRange::Parsed(call_at),
                    &func,
                    FnCallInfos {
                        nature: if func.is_method {
                            FnCallNature::Method
                        } else {
                            FnCallNature::NamedFunction
                        },
                        args: FnPossibleCallArgs::ParsedCmdArgs(args),
                        piped: piped_value,
                    },
                    ctx,
                )?;

                match return_value {
                    Some(value) => CmdChainState::Value(value),
                    None => CmdChainState::NoValue(call_at),
                }
            }
        });
    }

    let state = state.unwrap();

    match capture {
        Some(capture) => Ok(match state {
            CmdChainState::NoValue(from) => {
                return Err(ctx.error(from, "this returned nothing to capture"));
            }

            CmdChainState::Value(loc_val) => {
                let captured = match loc_val.value {
                    RuntimeValue::String(string) => string,

                    _ => {
                        return Err(ctx.error(
                            loc_val.from,
                            format!(
                                "expected a string to capture, found a: {}",
                                loc_val.value.compute_type().render_colored(
                                    ctx.type_alias_store(),
                                    PrettyPrintOptions::inline()
                                )
                            ),
                        ));
                    }
                };

                // Just a little assertion
                match capture {
                    CmdPipeCapture::Stdout => {}
                    CmdPipeCapture::Stderr /*| CmdPipeCapture::Both*/ => unreachable!(),
                }

                CmdExecResult::Captured(captured)
            }

            CmdChainState::Commands(children) => {
                let captured = wait_for_commands_ending(children, Some(capture), ctx)?;

                CmdExecResult::Captured(captured.unwrap())
            }
        }),

        None => Ok(match state {
            CmdChainState::NoValue(_) => CmdExecResult::None,
            CmdChainState::Value(loc_val) => CmdExecResult::Returned(loc_val),
            CmdChainState::Commands(children) => {
                wait_for_commands_ending(children, None, ctx)?;
                CmdExecResult::None
            }
        }),
    }
}

/// Internal representation of command chain state
enum CmdChainState {
    NoValue(CodeRange),
    Value(LocatedValue),
    Commands(Vec<(Child, CodeRange)>),
}

/// Result of a command execution
pub enum CmdExecResult {
    Returned(LocatedValue),
    Captured(String),
    None,
}

impl CmdExecResult {
    pub fn as_returned(self) -> Option<LocatedValue> {
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

/// Build command execution data
fn build_cmd_data(call: &Eaten<SingleCmdCall>, ctx: &mut Context) -> ExecResult<EvaluatedCmdData> {
    let mut args = EvaluatedCmdArgs {
        env_vars: HashMap::new(),
        args: vec![],
    };

    let developed = ctx.get_developed_cmd_call(call);

    let DevelopedSingleCmdCall {
        at: _,
        is_function,
        developed_aliases,
    } = &*developed;

    let mut target = None::<EvaluatedCmdTarget>;

    for (i, developed_alias) in developed_aliases.iter().rev().enumerate() {
        let runtime_alias = ctx
            .visible_scopes_content()
            .find_map(|scope| scope.cmd_aliases.get(&developed_alias.alias_called_at.data))
            .unwrap_or_else(|| {
                ctx.panic(
                    developed_alias.alias_called_at.at,
                    "runtime alias not found",
                )
            });

        let RuntimeCmdAlias {
            name: _,
            name_declared_at: _,
            content: _,
            content_scope_id,
            parent_scopes,
            captured_deps,
        } = &*runtime_alias.value;

        ctx.create_and_push_scope_with_deps(
            *content_scope_id,
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
        EvaluatedCmdTarget::Method(_) | EvaluatedCmdTarget::Function(_) => assert!(is_function),
        EvaluatedCmdTarget::ExternalCommand(_) => assert!(!is_function),
    }

    Ok(EvaluatedCmdData {
        args,
        call_at: call.at,
        target,
    })
}

/// Complete a commands' data with evaluated arguments and environment variables
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

        let LocatedValue { value, from } = eval_cmd_value_making_arg(value, ctx)?;

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
        out.args.push((eval_cmd_arg(arg, ctx)?, arg.at));
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
            CmdPath::Method(name) => EvaluatedCmdTarget::Method(name.clone()),

            CmdPath::Raw(name) => EvaluatedCmdTarget::Function(GcReadOnlyCell::clone(
                ctx.get_visible_fn_value(&name.forge_here(name.data.to_owned()))?,
            )),

            CmdPath::External(_) => {
                unreachable!()
            }
        };

        return Ok(func);
    }

    Ok(EvaluatedCmdTarget::ExternalCommand(match &cmd_path.data {
        CmdPath::Raw(name) => name.clone(),

        CmdPath::External(path) => match path {
            CmdExternalPath::Raw(name) | CmdExternalPath::LiteralString(name) => name.clone(),
            CmdExternalPath::ComputedString(c_str) => {
                c_str.forge_here(eval_computed_string(&c_str.data, ctx)?)
            }
        },

        CmdPath::Method(_) => unreachable!(),
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
    Method(Eaten<String>),
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
    input: Option<CmdInput<'_>>,
    ctx: &mut Context,
) -> ExecResult<Child> {
    let ExecCmdArgs {
        name,
        args,
        pipe_type,
        next_pipe_type,
        params,
    } = args;

    let CmdExecParams { capture, silent } = params;

    let EvaluatedCmdArgs { env_vars, args } = args;

    let mut args_str = vec![];

    for (arg, arg_at) in args {
        append_cmd_arg_as_string(arg, arg_at, &mut args_str, ctx)?;
    }

    // Determine the command name (or path)
    let cmd_path =
        try_replace_home_dir_tilde(&name.data, ctx).map_err(|err| ctx.error(name.at, err))?;

    // Resolve the command name to a binary's path
    let cmd_path = ctx
        .binaries_resolver()
        .resolve_binary_path(&cmd_path)
        .map_err(|err| {
            ctx.error(
                name.at,
                ExecErrorNature::CommandFailedToStart {
                    message: err.to_string(),
                },
            )
        })?;

    // Actually run the command
    let child = Command::new(cmd_path)
        .envs(env_vars)
        .args(args_str)
        .stdin(match input {
            Some(CmdInput::Child(child)) => match pipe_type.unwrap().data {
                CmdPipeType::ValueOrStdout => Stdio::from(child.stdout.take().unwrap()),
                CmdPipeType::Stderr => Stdio::from(child.stderr.take().unwrap()),
            },

            Some(CmdInput::String(string)) => {
                // Unfortunately, it's not possible to provide a direct string as an input to a command
                // We actually need to provide an actual file descriptor (as is a usual stdin "pipe")
                // So we create a new pair of pipes here...
                let (reader, mut writer) = std::pipe::pipe().unwrap();

                // ...write the string to one end...
                writer.write_all(string.as_bytes()).unwrap();

                // ...and then transform the other to pipe it into the command as soon as it spawns!
                Stdio::from(reader)
            }

            None => {
                if silent {
                    Stdio::piped()
                } else {
                    Stdio::inherit()
                }
            }
        })
        .stdout(
            if next_pipe_type == Some(CmdPipeType::ValueOrStdout)
                || matches!(
                    capture,
                    Some(CmdPipeCapture::Stdout /*| CmdPipeCapture::Both*/)
                )
                || silent
            {
                Stdio::piped()
            } else {
                Stdio::inherit()
            },
        )
        .stderr(
            if next_pipe_type == Some(CmdPipeType::Stderr)
                || matches!(
                    capture,
                    Some(CmdPipeCapture::Stderr /*| CmdPipeCapture::Both*/)
                )
                || silent
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

enum CmdInput<'a> {
    Child(&'a mut Child),
    String(String),
}

fn append_cmd_arg_as_string(
    cmd_arg_result: CmdArgResult,
    cmd_arg_result_at: CodeRange,
    args_str: &mut Vec<String>,
    ctx: &mut Context,
) -> ExecResult<()> {
    match cmd_arg_result {
        CmdArgResult::Single(value) => match value {
            SingleCmdArgResult::Basic(value) => args_str.push(value_to_str(
                &value.value,
                "arguments to external commands must be stringifyable",
                cmd_arg_result_at,
                ctx,
            )?),

            SingleCmdArgResult::Flag(CmdFlagValue { name, value }) => {
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

pub fn eval_cmd_arg(arg: &Eaten<CmdArg>, ctx: &mut Context) -> ExecResult<CmdArgResult> {
    match &arg.data {
        CmdArg::ValueMaking(value_making) => eval_cmd_value_making_arg(value_making, ctx)
            .map(|value| CmdArgResult::Single(SingleCmdArgResult::Basic(value))),

        CmdArg::Flag(CmdFlagArg { name, value }) => Ok(CmdArgResult::Single(
            SingleCmdArgResult::Flag(CmdFlagValue {
                name: RuntimeEaten::from(name.clone()),
                value: value
                    .as_ref()
                    .map(|CmdFlagValueArg { value, value_sep }| {
                        eval_cmd_value_making_arg(value, ctx).map(|value| FlagArgValueResult {
                            value,
                            value_sep: *value_sep,
                        })
                    })
                    .transpose()?,
            }),
        )),

        CmdArg::Spread(spread) => {
            let spread_value = match &spread.data {
                CmdSpreadArg::Variable(var_name) => {
                    let var = ctx.get_visible_var(var_name);
                    var.value.read(var_name.at).value.clone()
                }

                CmdSpreadArg::Expr(expr) => eval_expr(expr, ctx)?,
            };

            match spread_value {
                RuntimeValue::List(items) => {
                    let spreaded = items
                        .read_promise_no_write()
                        .iter()
                        .map(|item| match item {
                            RuntimeValue::CmdArg(arg) => {
                                Ok(SingleCmdArgResult::from(CmdArgValue::clone(arg)))
                            }

                            _ => value_to_str(
                                item,
                                "spreaded arguments to external commands must be stringifyable",
                                spread.at,
                                ctx,
                            )
                            .map(|str| {
                                SingleCmdArgResult::Basic(LocatedValue::new(
                                    RuntimeCodeRange::Parsed(spread.at),
                                    RuntimeValue::String(str),
                                ))
                            }),
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    Ok(CmdArgResult::Spreaded(spreaded))
                }

                _ => Err(ctx.error(
                    spread.at,
                    format!(
                        "expected a spread value, found a {}",
                        spread_value
                            .compute_type()
                            .render_colored(ctx.type_alias_store(), PrettyPrintOptions::inline())
                    ),
                )),
            }
        }
    }
}

fn eval_cmd_value_making_arg(
    arg: &Eaten<CmdValueMakingArg>,
    ctx: &mut Context,
) -> ExecResult<LocatedValue> {
    let value = match &arg.data {
        CmdValueMakingArg::LiteralValue(lit_val) => eval_literal_value(lit_val),

        CmdValueMakingArg::Variable(name) => {
            ctx.get_visible_var(name).value.read(name.at).value.clone()
        }

        CmdValueMakingArg::ComputedString(computed_str) => {
            RuntimeValue::String(eval_computed_string(computed_str, ctx)?)
        }

        CmdValueMakingArg::CmdRawString(computed_str) => {
            RuntimeValue::String(eval_cmd_raw_string(computed_str, ctx)?)
        }
        CmdValueMakingArg::ParenExpr(expr) => eval_expr(&expr.data, ctx)?,

        CmdValueMakingArg::Lambda(func) => lambda_to_value(&func.data, ctx),

        CmdValueMakingArg::InlineCmdCall(call) => RuntimeValue::CmdCall {
            content_at: call.at,
        },

        CmdValueMakingArg::CmdOutput(call) => {
            let cmd_result = run_cmd(
                call,
                ctx,
                CmdExecParams {
                    capture: Some(CmdPipeCapture::Stdout),
                    silent: false,
                },
            )?;

            RuntimeValue::String(cmd_result.as_captured().unwrap())
        }
    };

    Ok(LocatedValue::new(RuntimeCodeRange::Parsed(arg.at), value))
}

pub fn eval_cmd_raw_string(value: &Eaten<CmdRawString>, ctx: &mut Context) -> ExecResult<String> {
    value
        .data
        .pieces
        .iter()
        .enumerate()
        .map(|(i, piece)| eval_cmd_raw_string_piece(piece, i == 0, ctx))
        .collect::<Result<String, _>>()
}

fn eval_cmd_raw_string_piece(
    piece: &Eaten<CmdRawStringPiece>,
    transmute_tilde: bool,
    ctx: &mut Context,
) -> ExecResult<String> {
    match &piece.data {
        CmdRawStringPiece::Literal(str) => {
            if transmute_tilde {
                try_replace_home_dir_tilde(str, ctx).map_err(|err| ctx.error(piece.at, err))
            } else {
                Ok(str.clone())
            }
        }

        CmdRawStringPiece::Variable(var_name) => Ok(value_to_str(
            &ctx.get_visible_var(var_name).value.read(var_name.at).value,
            "only stringifyable variables can be used inside computable strings",
            var_name.at,
            ctx,
        )?),
    }
}

pub fn try_replace_home_dir_tilde(raw: &str, ctx: &Context) -> Result<String, &'static str> {
    let home_dir = || {
        ctx.home_dir()
            .ok_or("home directory was not defined in context")?
            .to_str()
            .ok_or("home directory path contains invalid UTF-8 characters")
    };

    let out = if raw == "~" {
        home_dir()?.to_owned()
    } else if let Some(rest) = raw.strip_prefix("~/").or_else(|| raw.strip_prefix("~\\")) {
        format!("{}{MAIN_SEPARATOR}{rest}", home_dir()?)
    } else {
        raw.to_owned()
    };

    Ok(out)
}

fn wait_for_commands_ending(
    children: Vec<(Child, CodeRange)>,
    capture: Option<CmdPipeCapture>,
    ctx: &Context,
) -> ExecResult<Option<String>> {
    assert!(!children.is_empty());

    let mut final_output = None;

    // Evaluate each subcommand
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
            final_output = capture.map(|capture| match capture {
                CmdPipeCapture::Stdout => output.stdout,
                CmdPipeCapture::Stderr => output.stderr,
                // CmdPipeCapture::Both => {
                //     todo!()
                // }
            });
        }
    }

    Ok(final_output.map(|captured| {
        // Invalid UTF-8 output will be handled with "unknown" symbols
        let mut out = String::from_utf8_lossy(&captured).into_owned();

        if out.ends_with('\n') {
            out.pop();
        }

        out
    }))
}

#[derive(Debug, Clone)]
pub enum CmdArgResult {
    Single(SingleCmdArgResult),
    Spreaded(Vec<SingleCmdArgResult>),
}

#[derive(Debug, Clone)]
pub enum SingleCmdArgResult {
    Basic(LocatedValue),
    Flag(CmdFlagValue),
}

impl From<CmdArgValue> for SingleCmdArgResult {
    fn from(value: CmdArgValue) -> Self {
        match value {
            CmdArgValue::Basic(data) => Self::Basic(data),
            CmdArgValue::Flag(data) => Self::Flag(data),
        }
    }
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
