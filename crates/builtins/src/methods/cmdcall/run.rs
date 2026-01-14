use reshell_parser::ast::CmdCaptureType;
use reshell_runtime::{
    cmd::{CmdExecParams, run_cmd},
    errors::{ExecActualErrorNature, ExecError},
};

use crate::define_internal_fn;

define_internal_fn!(
    "run",

    (
        cmd_call: RequiredArg<CmdCallType> = Arg::method_self(),
        silent: PresenceFlag = Arg::long_flag("silent"),
        ignore_failure: PresenceFlag = Arg::long_flag("ignore-failure"),
        capture_stderr: PresenceFlag = Arg::long_flag("capture-stderr")
    )

    -> VoidType
);

fn run() -> Runner {
    Runner::new(
        |at,
         Args {
             cmd_call,
             silent,
             ignore_failure,
             capture_stderr,
         },
         _,
         ctx| {
            let cmd = ctx.get_cmd_call_used_as_value(cmd_call);

            match run_cmd(
                &cmd,
                ctx,
                CmdExecParams {
                    capture: if silent {
                        Some(if capture_stderr {
                            CmdCaptureType::Stderr
                        } else {
                            CmdCaptureType::Stdout
                        })
                    } else {
                        None
                    },

                    silent,
                },
            ) {
                Ok(_) => Ok(None),
                Err(err) => match err {
                    ExecError::ActualError(err) => {
                        if err.at.parsed_range() != Some(cmd_call) {
                            return Err(ExecError::ActualError(err));
                        }

                        match err.nature {
                            ExecActualErrorNature::CommandFailedToStart { message } => {
                                Err(ctx.throw(at, format!("failed to start command: {message}")))
                            }

                            ExecActualErrorNature::CommandFailed {
                                message,
                                exit_status: _,
                            } => {
                                if ignore_failure {
                                    Ok(None)
                                } else {
                                    Err(ctx.throw(at, message))
                                }
                            }

                            ExecActualErrorNature::ParsingErr(_)
                            | ExecActualErrorNature::CheckingErr(_)
                            | ExecActualErrorNature::Thrown { message: _, at: _ }
                            | ExecActualErrorNature::CtrlC
                            | ExecActualErrorNature::Custom(_) => Err(ExecError::ActualError(err)),
                        }
                    }

                    ExecError::InternalPropagation(_) | ExecError::TopPropagation(_) => Err(err),
                },
            }
        },
    )
}
