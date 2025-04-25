use reshell_parser::ast::CmdCaptureType;
use reshell_runtime::{
    cmd::{CmdExecParams, CmdExecResult, run_cmd},
    errors::ExecErrorNature,
};

use crate::define_internal_fn;

define_internal_fn!(
    "capture",

    (
        cmd_call: RequiredArg<CmdCallType> = Arg::method_self(),
        capture_stderr: PresenceFlag = Arg::long_flag("capture-stderr")
    )

    -> VoidType
);

fn run() -> Runner {
    Runner::new(
        |at,
         Args {
             cmd_call,
             capture_stderr,
         },
         _,
         ctx| {
            let cmd = ctx.get_cmd_call_used_as_value(cmd_call);

            let exec_result = run_cmd(
                &cmd,
                ctx,
                CmdExecParams {
                    capture: Some(if capture_stderr {
                        CmdCaptureType::Stderr
                    } else {
                        CmdCaptureType::Stdout
                    }),

                    silent: false,
                },
            );

            let captured = exec_result.map_err(|err| {
                if err.at.parsed_range() != Some(cmd_call) {
                    return err;
                }

                match err.nature {
                    ExecErrorNature::CommandFailedToStart { message } => {
                        ctx.throw(at, format!("failed to start command: {message}"))
                    }

                    ExecErrorNature::CommandFailed {
                        message,
                        exit_status: _,
                    } => ctx.throw(at, message),

                    ExecErrorNature::ParsingErr(_)
                    | ExecErrorNature::CheckingErr(_)
                    | ExecErrorNature::Thrown { message: _, at: _ }
                    | ExecErrorNature::CtrlC
                    | ExecErrorNature::FailureExit { code: _ }
                    | ExecErrorNature::Custom(_)
                    | ExecErrorNature::NotAnError(_)
                    | ExecErrorNature::InternalPropagation(_) => err,
                }
            })?;

            let captured = match captured {
                CmdExecResult::Returned(_) | CmdExecResult::None => unreachable!(),
                CmdExecResult::Captured(captured) => captured,
            };

            Ok(Some(RuntimeValue::String(captured)))
        },
    )
}
