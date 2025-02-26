use reshell_parser::ast::CmdCaptureType;
use reshell_runtime::{
    cmd::{run_cmd, CmdExecParams},
    errors::ExecErrorNature,
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
                Err(err) => {
                    if err.at.parsed_range() != Some(cmd_call) {
                        return Err(err);
                    }

                    match err.nature {
                        ExecErrorNature::CommandFailedToStart { message } => {
                            if ignore_failure {
                                Ok(None)
                            } else {
                                Err(ctx.throw(at, format!("failed to start command: {message}")))
                            }
                        }

                        ExecErrorNature::CommandFailed {
                            message,
                            exit_status: _,
                        } => {
                            if ignore_failure {
                                Ok(None)
                            } else {
                                Err(ctx.throw(at, message))
                            }
                        }

                        ExecErrorNature::ParsingErr(_)
                        | ExecErrorNature::CheckingErr(_)
                        | ExecErrorNature::Thrown { message: _, at: _ }
                        | ExecErrorNature::CtrlC
                        | ExecErrorNature::FailureExit { code: _ }
                        | ExecErrorNature::Custom(_)
                        | ExecErrorNature::NotAnError(_)
                        | ExecErrorNature::InternalPropagation(_) => Err(err),
                    }
                }
            }
        },
    )
}
