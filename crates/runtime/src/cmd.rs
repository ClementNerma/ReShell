//! Commands execution

use std::{
    collections::HashMap,
    fs::File,
    io::{PipeReader, PipeWriter, Read, Write},
    path::{MAIN_SEPARATOR, Path},
    process::{Child, Command, Stdio},
    sync::Arc,
};

use parsy::{CodeRange, Span};
use reshell_checker::output::DevelopedSingleCmdCall;
use reshell_parser::ast::{
    CmdArg, CmdCall, CmdCallBase, CmdCaptureType, CmdEnvVar, CmdExternalPath, CmdFlagArg,
    CmdFlagValueArg, CmdOutputCapture, CmdPath, CmdPipe, CmdPipeType, CmdRawString,
    CmdRawStringPiece, CmdRedirects, CmdValueMakingArg, FlagValueSeparator, FnCallNature,
    RuntimeCodeRange, RuntimeSpan, SingleCmdCall,
};
use reshell_prettify::{PrettyPrintOptions, PrettyPrintable};

use crate::{
    context::Context,
    errors::{ExecActualErrorNature, ExecResult},
    expr::{
        eval_computed_string, eval_expr, eval_list, eval_list_spread_value, eval_literal_value,
        lambda_to_value,
    },
    functions::{FnCallInfos, FnPossibleCallArgs, call_fn_value, find_applicable_method},
    values::{
        CmdArgValue, CmdFlagValue, LocatedValue, RuntimeCmdAlias, RuntimeFnValue, RuntimeValue,
        value_to_str,
    },
};

#[derive(Clone, Copy)]
pub struct CmdExecParams {
    /// How the command's output should be captured
    pub capture: Option<CmdCaptureType>,

    /// Hide output from non-captured pipes
    pub silent: bool,
}

pub fn run_cmd(
    call: &Span<CmdCall>,
    ctx: &mut Context,
    params: CmdExecParams,
) -> ExecResult<CmdExecResult> {
    let CmdExecParams { capture, silent: _ } = params;

    // Interrupt before command execution if Ctrl+C was pressed
    ctx.ensure_no_ctrl_c_press(call.at)?;

    // Prepare a uniform command chain for easier processing
    let CmdCall { base, pipes } = &call.data;

    let (base, from_value) = match &**base {
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
            args_at,
            redirects,
        } = cmd_data;

        state = Some(match target {
            EvaluatedCmdTarget::ExternalCommand(cmd_name) => {
                // TODO: deduplicate sub routines
                let (piped_string, children) = match state {
                    Some(CmdChainState::NoValue(from)) => {
                        if i > 0 {
                            return Err(ctx.hard_error(from, "this function returned nothing ; cannot pipe it into a command's input"));
                        }

                        (None, None)
                    }

                    Some(CmdChainState::Value(loc_val)) => {
                        let input = match loc_val.value {
                            RuntimeValue::String(string) => string,

                            _ => {
                                return Err(ctx.hard_error(
                                    loc_val.from,
                                    format!(
                                        "expected a string to pipe into the next command, instead found a: {}",
                                        loc_val
                                            .value
                                            .compute_type()
                                            .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
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
                        redirects,
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
                let EvaluatedCmdArgs { env_vars, args } = args;

                assert!(env_vars.is_empty());

                let piped_value = match state {
                    Some(CmdChainState::NoValue(from)) => {
                        if i > 0 {
                            return Err(ctx.hard_error(from, "this function returned nothing ; cannot pipe it into another function"));
                        }

                        None
                    }

                    Some(CmdChainState::Commands(children)) => {
                        let from = children.last().unwrap().1;

                        let captured = wait_for_commands_ending(
                            children,
                            Some(match pipe_type.unwrap().data {
                                CmdPipeType::Stderr => CmdCaptureType::Stderr,
                                CmdPipeType::ValueOrStdout => CmdCaptureType::Stdout,
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
                                    ctx.hard_error(
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
                        args: FnPossibleCallArgs::ParsedCmdArgs { at: args_at, args },
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
                return Err(ctx.hard_error(from, "this returned nothing to capture"));
            }

            CmdChainState::Value(loc_val) => {
                let captured = match loc_val.value {
                    RuntimeValue::String(string) => string,

                    _ => {
                        return Err(ctx.hard_error(
                            loc_val.from,
                            format!(
                                "expected a string to capture, found a: {}",
                                loc_val
                                    .value
                                    .compute_type()
                                    .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                            ),
                        ));
                    }
                };

                // Just a little assertion
                match capture {
                    CmdCaptureType::Stdout => {}
                    CmdCaptureType::Stderr => unreachable!(),
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
    Commands(Vec<(SpawnedCmd, CodeRange)>),
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
fn build_cmd_data<'a>(
    call: &'a Span<SingleCmdCall>,
    ctx: &mut Context,
) -> ExecResult<EvaluatedCmdData<'a>> {
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

        ctx.create_and_push_scope_detailed(
            *content_scope_id,
            captured_deps.clone(),
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
        call_at: call.at,
        args,
        args_at: call.data.args.at,
        target,
        redirects: call.data.redirects.as_ref(),
    })
}

/// Complete a commands' data with evaluated arguments and environment variables
fn complete_cmd_data(
    call: &Span<SingleCmdCall>,
    out: &mut EvaluatedCmdArgs,
    ctx: &mut Context,
) -> ExecResult<()> {
    let SingleCmdCall {
        path: _,
        env_vars,
        args,
        redirects: _,
    } = &call.data;

    for env_var in &env_vars.data {
        let CmdEnvVar { name, value } = &env_var.data;

        let LocatedValue { value, from } = eval_cmd_value_making_arg(value, ctx)?;

        out.env_vars.insert(
            name.data.clone(),
            value_to_str(
                &value,
                from,
                "environment variables can only have stringifyable values",
                ctx,
            )?,
        );
    }

    for arg in &args.data {
        out.args.push((eval_cmd_arg(arg, ctx)?, arg.at));
    }

    Ok(())
}

/// Evaluate the target of a command call
fn evaluate_cmd_target(
    call: &Span<SingleCmdCall>,
    ctx: &mut Context,
) -> ExecResult<EvaluatedCmdTarget> {
    let developed = ctx.get_developed_cmd_call(call);

    let cmd_path = &call.data.path;

    if developed.is_function {
        let func = match &cmd_path.data {
            CmdPath::Method(name) => EvaluatedCmdTarget::Method(name.clone()),

            CmdPath::Raw(name) => EvaluatedCmdTarget::Function(Arc::clone(
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
            CmdExternalPath::RawString(r_str) => r_str.forge_here(eval_cmd_raw_string(r_str, ctx)?),
            CmdExternalPath::LiteralString(name) => name.clone(),
            CmdExternalPath::ComputedString(c_str) => {
                c_str.forge_here(eval_computed_string(&c_str.data, ctx)?)
            }
        },

        CmdPath::Method(_) => unreachable!(),
    }))
}

struct EvaluatedCmdData<'a> {
    target: EvaluatedCmdTarget,
    args: EvaluatedCmdArgs,
    call_at: CodeRange,
    args_at: CodeRange,
    redirects: Option<&'a Span<CmdRedirects>>,
}

struct EvaluatedCmdArgs {
    env_vars: HashMap<String, String>,
    args: Vec<(CmdArgResult, CodeRange)>,
}

/// Target of a command call
enum EvaluatedCmdTarget {
    /// Target is an external command (will be resolved through [`crate::bin_resolver::BinariesResolver`])
    ExternalCommand(Span<String>),

    /// Target is a function
    ///
    /// It is backed by an [`Arc`] to avoid needless cloning
    Function(Arc<RuntimeFnValue>),

    /// Target is a method (will be resolved later)
    Method(Span<String>),
}

/// Arguments for the command's execution
struct ExecCmdArgs<'a, 'b> {
    /// Name of the command
    name: &'a Span<String>,

    /// Arguments for the command
    args: EvaluatedCmdArgs,

    /// Data being piped to this command
    pipe_type: Option<Span<CmdPipeType>>,

    /// Pipe for this command's output
    next_pipe_type: Option<CmdPipeType>,

    /// Execution parameters
    params: CmdExecParams,

    /// File and stream redirects (e.g. STDOUT / STDERR)
    redirects: Option<&'b Span<CmdRedirects>>,
}

/// Execute a command
fn exec_cmd(
    args: ExecCmdArgs,
    input: Option<CmdInput<'_>>,
    ctx: &mut Context,
) -> ExecResult<SpawnedCmd> {
    let ExecCmdArgs {
        name,
        args,
        pipe_type,
        next_pipe_type,
        params,
        redirects,
    } = args;

    let CmdExecParams { capture, silent } = params;

    let EvaluatedCmdArgs { env_vars, args } = args;

    let mut args_str = vec![];

    for (arg, arg_at) in args {
        append_cmd_arg_as_string(arg, arg_at, &mut args_str, false, ctx)?;
    }

    // Determine the command name (or path)
    let cmd_path =
        try_replace_home_dir_tilde(&name.data, ctx).map_err(|err| ctx.hard_error(name.at, err))?;

    // Resolve the command name to a binary's path
    let cmd_path = ctx
        .binaries_resolver()
        .resolve_binary_path(&cmd_path)
        .map_err(|err| {
            ctx.hard_error(
                name.at,
                ExecActualErrorNature::CommandFailedToStart {
                    message: err.to_string(),
                },
            )
        })?;

    // Compute the pipes to provide to the child process
    let StdioPipesForChild {
        stdout,
        stdout_reader,
        stderr,
        stderr_reader,
    } = build_pipes_for_child(
        capture,
        silent,
        next_pipe_type,
        redirects.as_ref().map(|span| &span.data),
        ctx,
    )?;

    // Actually run the command
    let child = Command::new(cmd_path)
        .envs(env_vars)
        .args(args_str)
        .stdin(match input {
            Some(CmdInput::Child(SpawnedCmd {
                child,
                stdout,
                stderr,
            })) => match pipe_type.unwrap().data {
                CmdPipeType::ValueOrStdout => match stdout.take() {
                    Some(stdout) => Stdio::from(stdout),
                    None => Stdio::from(child.stdout.take().unwrap()),
                },
                CmdPipeType::Stderr => match stderr.take() {
                    Some(stderr) => Stdio::from(stderr),
                    None => Stdio::from(child.stderr.take().unwrap()),
                },
            },

            Some(CmdInput::String(string)) => {
                // Unfortunately, it's not possible to provide a direct string as an input to a command
                // We actually need to provide an actual file descriptor (as is a usual stdin "pipe")
                // So we create a new pair of pipes here...
                let (reader, mut writer) = std::io::pipe().unwrap();

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
        .stdout(stdout)
        .stderr(stderr)
        .spawn()
        .map_err(|err| {
            ctx.hard_error(
                name.at,
                ExecActualErrorNature::CommandFailedToStart {
                    message: format!("failed to start command '{}': {err}", name.data),
                },
            )
        })?;

    Ok(SpawnedCmd {
        child,
        stdout: stdout_reader,
        stderr: stderr_reader,
    })
}

/// Input for a command
enum CmdInput<'a> {
    /// Pipe a child process into the command
    Child(&'a mut SpawnedCmd),

    /// Pipe a string into the command
    String(String),
}

/// Spawned command
struct SpawnedCmd {
    /// Child process handler
    child: Child,

    /// STDOUT stream
    stdout: Option<PipeReader>,

    /// STDERR stream
    stderr: Option<PipeReader>,
}

/// Build the output pipes required for a child process
fn build_pipes_for_child(
    capture: Option<CmdCaptureType>,
    silent: bool,
    next_pipe_type: Option<CmdPipeType>,
    redirects: Option<&CmdRedirects>,
    ctx: &mut Context,
) -> ExecResult<StdioPipesForChild> {
    let capturing_stdout = next_pipe_type == Some(CmdPipeType::ValueOrStdout)
        || matches!(capture, Some(CmdCaptureType::Stdout))
        || silent;

    let capturing_stderr = next_pipe_type == Some(CmdPipeType::Stderr)
        || matches!(capture, Some(CmdCaptureType::Stderr))
        || silent;

    enum PipeType {
        Custom {
            reader: PipeReader,
            writer: PipeWriter,
        },
        Stdout,
        Stderr,
        File(File),
        Piped,
    }

    let (stdout, stderr) = match redirects {
        Some(redirects) => match redirects {
            CmdRedirects::StdoutToFile(path) => {
                assert!(!capturing_stdout);

                (
                    PipeType::File(open_redirect_file(path, ctx)?),
                    PipeType::Stderr,
                )
            }

            CmdRedirects::StderrToFile(path) => {
                assert!(!capturing_stderr);

                (
                    PipeType::Stdout,
                    PipeType::File(open_redirect_file(path, ctx)?),
                )
            }

            CmdRedirects::StderrToStdout => {
                assert!(!capturing_stderr);

                if capturing_stdout {
                    let (reader, writer) = std::io::pipe().unwrap();

                    (
                        PipeType::Custom {
                            reader: reader.try_clone().unwrap(),
                            writer: writer.try_clone().unwrap(),
                        },
                        PipeType::Custom { reader, writer },
                    )
                } else {
                    (PipeType::Stdout, PipeType::Stdout)
                }
            }

            CmdRedirects::StdoutToStderr => {
                assert!(!capturing_stdout);

                if capturing_stderr {
                    let (reader, writer) = std::io::pipe().unwrap();

                    (
                        PipeType::Custom {
                            reader: reader.try_clone().unwrap(),
                            writer: writer.try_clone().unwrap(),
                        },
                        PipeType::Custom { reader, writer },
                    )
                } else {
                    (PipeType::Stderr, PipeType::Stderr)
                }
            }

            CmdRedirects::StdoutAndStderrToFile(path) => {
                assert!(!capturing_stdout);
                assert!(!capturing_stderr);

                let file = open_redirect_file(path, ctx)?;

                let file_bis = file.try_clone().map_err(|err| {
                    ctx.hard_error(path.at, format!("failed to duplicate file handler: {err}"))
                })?;

                (PipeType::File(file), PipeType::File(file_bis))
            }

            CmdRedirects::StdoutToFileAndStderrToFile {
                path_for_stdout,
                path_for_stderr,
            } => {
                assert!(!capturing_stdout);
                assert!(!capturing_stderr);

                (
                    PipeType::File(open_redirect_file(path_for_stdout, ctx)?),
                    PipeType::File(open_redirect_file(path_for_stderr, ctx)?),
                )
            }
        },

        None => (
            if capturing_stdout {
                PipeType::Piped
            } else {
                PipeType::Stdout
            },
            if capturing_stderr {
                PipeType::Piped
            } else {
                PipeType::Stderr
            },
        ),
    };

    let (stdout, stdout_reader) = match stdout {
        PipeType::Custom { reader, writer } => (Stdio::from(writer), Some(reader)),
        PipeType::Stdout => (Stdio::inherit(), None),
        PipeType::Stderr => (std::io::stderr().into(), None),
        PipeType::Piped => (Stdio::piped(), None),
        PipeType::File(file) => (Stdio::from(file), None),
    };

    let (stderr, stderr_reader) = match stderr {
        PipeType::Custom { reader, writer } => (Stdio::from(writer), Some(reader)),
        PipeType::Stdout => (std::io::stdout().into(), None),
        PipeType::Stderr => (Stdio::inherit(), None),
        PipeType::Piped => (Stdio::piped(), None),
        PipeType::File(file) => (Stdio::from(file), None),
    };

    Ok(StdioPipesForChild {
        stdout,
        stdout_reader,
        stderr,
        stderr_reader,
    })
}

/// Open a file that will be used to redirect SDTOUT or STDERR into
fn open_redirect_file(path: &Span<CmdRawString>, ctx: &mut Context) -> ExecResult<File> {
    let path_str = eval_cmd_raw_string(path, ctx)?;

    if !Path::new(".")
        .join(Path::new(&path_str).parent().unwrap_or(Path::new("")))
        .is_dir()
    {
        return Err(ctx.hard_error(
            path.at,
            "the parent directory of this file does not exist".to_string(),
        ));
    }

    File::create(&path_str).map_err(|err| {
        ctx.hard_error(
            path.at,
            format!("failed to open file at path '{path_str}': {err}"),
        )
    })
}

/// STDIO (STDOUT / STDERR) pipes for a child process
///
/// Used by [`exec_cmd`]
struct StdioPipesForChild {
    stdout: Stdio,
    stdout_reader: Option<PipeReader>,
    stderr: Stdio,
    stderr_reader: Option<PipeReader>,
}

/// Append a command's argument as a string
///
/// May be spread into multiple arguments
fn append_cmd_arg_as_string(
    cmd_arg_result: CmdArgResult,
    cmd_arg_result_at: CodeRange,
    args_str: &mut Vec<String>,
    in_rest: bool,
    ctx: &mut Context,
) -> ExecResult<()> {
    let type_error_tip = if in_rest {
        "(in rest argument) values provided to external commands must be stringifyable"
    } else {
        "values provided to external commands must be stringifyable"
    };

    match cmd_arg_result {
        CmdArgResult::Single(value) => match value {
            SingleCmdArgResult::Basic(value) => args_str.push(value_to_str(
                &value.value,
                cmd_arg_result_at,
                type_error_tip,
                ctx,
            )?),

            SingleCmdArgResult::Flag(CmdFlagValue { name, value }) => {
                let name = name.data.back_to_string();

                match value {
                    Some(FlagArgValueResult { value, value_sep }) => {
                        let value = value_to_str(&value.value, value.from, type_error_tip, ctx)?;

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
                    true,
                    ctx,
                )?;
            }
        }
    }

    Ok(())
}

/// Evaluate a command argument's value
pub fn eval_cmd_arg(arg: &Span<CmdArg>, ctx: &mut Context) -> ExecResult<CmdArgResult> {
    match &arg.data {
        CmdArg::ValueMaking(value_making) => eval_cmd_value_making_arg(value_making, ctx)
            .map(|value| CmdArgResult::Single(SingleCmdArgResult::Basic(value))),

        CmdArg::Flag(CmdFlagArg { name, value }) => Ok(CmdArgResult::Single(
            SingleCmdArgResult::Flag(CmdFlagValue {
                name: RuntimeSpan::from(name.clone()),
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

        CmdArg::Spread(spread_value) => {
            let items = eval_list_spread_value(spread_value, ctx)?;

            let spreaded = items
                .read_promise_no_write()
                .iter()
                .map(|item| match item {
                    RuntimeValue::CmdArg(arg) => SingleCmdArgResult::from(CmdArgValue::clone(arg)),

                    _ => SingleCmdArgResult::Basic(LocatedValue::new(
                        RuntimeCodeRange::Parsed(spread_value.at),
                        item.clone(),
                    )),
                })
                .collect::<Vec<_>>();

            Ok(CmdArgResult::Spreaded(spreaded))
        }
    }
}

/// Evaluate a command value-making argument
fn eval_cmd_value_making_arg(
    arg: &Span<CmdValueMakingArg>,
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

        CmdValueMakingArg::List(items) => eval_list(items, ctx)?,

        CmdValueMakingArg::CmdRawString(computed_str) => {
            RuntimeValue::String(eval_cmd_raw_string(computed_str, ctx)?)
        }
        CmdValueMakingArg::ParenExpr(expr) => eval_expr(&expr.data, ctx)?,

        CmdValueMakingArg::Lambda(func) => lambda_to_value(&func.data, ctx),

        CmdValueMakingArg::InlineCmdCall(call) => RuntimeValue::CmdCall {
            content_at: call.at,
        },
    };

    Ok(LocatedValue::new(RuntimeCodeRange::Parsed(arg.at), value))
}

/// Evaluate a command raw string
pub fn eval_cmd_raw_string(value: &Span<CmdRawString>, ctx: &mut Context) -> ExecResult<String> {
    value
        .data
        .pieces
        .iter()
        .enumerate()
        .map(|(i, piece)| eval_cmd_raw_string_piece(piece, i == 0, ctx))
        .collect::<Result<String, _>>()
}

/// Evaluate a single piece of a command raw string
fn eval_cmd_raw_string_piece(
    piece: &Span<CmdRawStringPiece>,
    transmute_tilde: bool,
    ctx: &mut Context,
) -> ExecResult<String> {
    match &piece.data {
        CmdRawStringPiece::Literal(str) => {
            if transmute_tilde {
                try_replace_home_dir_tilde(str, ctx).map_err(|err| ctx.hard_error(piece.at, err))
            } else {
                Ok(str.clone())
            }
        }

        CmdRawStringPiece::Variable(var_name) => value_to_str(
            &ctx.get_visible_var(var_name).value.read(var_name.at).value,
            var_name.at,
            "only stringifyable variables can be used inside computable strings",
            ctx,
        ),

        CmdRawStringPiece::CmdCapturedOutput(capture) => capture_cmd_output(capture, ctx),
    }
}

/// Try replacing the `~` symbol at the beginning of a string by the actual home directory path
///
/// Used for expanding this symbol in arguments provided to commands
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

/// Wait for a set of spawned commands to complete
fn wait_for_commands_ending(
    children: Vec<(SpawnedCmd, CodeRange)>,
    capture: Option<CmdCaptureType>,
    ctx: &Context,
) -> ExecResult<Option<String>> {
    assert!(!children.is_empty());

    let mut final_output = None;

    // Evaluate each subcommand
    for (i, (mut spawned, at)) in children.into_iter().rev().enumerate() {
        let output = spawned.child.wait_with_output();

        ctx.reset_ctrl_c_press_indicator();

        let output = output.map_err(|err| {
            ctx.hard_error(
                at,
                ExecActualErrorNature::CommandFailed {
                    message: format!("command failed: {err}"),
                    exit_status: None,
                },
            )
        })?;

        if !output.status.success() {
            return Err(ctx.hard_error(
                at,
                ExecActualErrorNature::CommandFailed {
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

        if i == 0
            && let Some(capture) = capture
        {
            final_output = Some(match capture {
                CmdCaptureType::Stdout => match spawned.stdout.take() {
                    Some(mut stdout) => {
                        let mut buf = vec![];

                        stdout.read_to_end(&mut buf).map_err(|err| {
                            ctx.hard_error(at, format!("failed to read command's STDOUT: {err}"))
                        })?;

                        buf
                    }

                    None => output.stdout,
                },

                CmdCaptureType::Stderr => match spawned.stderr.take() {
                    Some(mut stderr) => {
                        let mut buf = vec![];

                        stderr.read_to_end(&mut buf).map_err(|err| {
                            ctx.hard_error(at, format!("failed to read command's STDERR: {err}"))
                        })?;

                        buf
                    }

                    None => output.stderr,
                },
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

/// Capture a command's output
pub fn capture_cmd_output(capture: &CmdOutputCapture, ctx: &mut Context) -> ExecResult<String> {
    let CmdOutputCapture { capture, cmd_call } = capture;

    let cmd_result = run_cmd(
        cmd_call,
        ctx,
        CmdExecParams {
            capture: Some(capture.data),
            silent: false,
        },
    )?;

    Ok(cmd_result.as_captured().unwrap())
}

/// Result of a command argument's evaluation, which may result in either
/// a single value, or multiple ones (spreading)
#[derive(Debug, Clone)]
pub enum CmdArgResult {
    Single(SingleCmdArgResult),
    Spreaded(Vec<SingleCmdArgResult>),
}

/// Single value part of a command argument's evaluated vlaue
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

/// Evaluation result of a flag argument's value
#[derive(Debug, Clone)]
pub struct FlagArgValueResult {
    /// The argument's value
    pub value: LocatedValue,

    /// The argument's separator
    ///
    /// Used to put back the original string together if the command is external
    pub value_sep: FlagValueSeparator,
}

/// Evaluation result of a command argument
#[derive(Debug)]
pub enum CmdEvalArg {
    /// The argument evaluated to a value
    Value(RuntimeValue),

    /// The argument evaluated to a flag
    Flag {
        name: Span<String>,
        value: Option<RuntimeValue>,
    },
}
