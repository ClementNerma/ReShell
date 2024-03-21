use reshell_runtime::{
    cmd::{run_cmd, CmdExecParams, CmdPipeCapture},
    errors::ExecErrorNature,
};

use crate::define_internal_fn;

define_internal_fn!(
    "run",

    (
        cmd_call: RequiredArg<CmdCallType> = Arg::method_self(),
        silent: PresenceFlag = Arg::long_flag("silent"),
        ignore_failure: PresenceFlag = Arg::long_flag("ignore-failure")
    )

    -> None
);

fn run() -> Runner {
    Runner::new(
        |_,
         Args {
             cmd_call,
             silent,
             ignore_failure,
         },
         _,
         ctx| {
            let cmd = ctx.get_cmd_call_used_as_value(cmd_call);

            // TODO: ensure the error is for the call itself, not inside one of its inner arguments (e.g. closure) etc.
            match run_cmd(
                &cmd,
                ctx,
                CmdExecParams {
                    capture: if silent {
                        Some(CmdPipeCapture::Both)
                    } else {
                        None
                    },
                },
            ) {
                Ok(_) => Ok(None),
                Err(err) => match err.nature {
                    ExecErrorNature::CommandFailedToStart { message } => {
                        if ignore_failure {
                            Ok(None)
                        } else {
                            Err(ctx.throw(cmd_call, format!("failed to start command: {message}")))
                        }
                    }

                    ExecErrorNature::CommandFailed {
                        message,
                        exit_status: _,
                    } => {
                        if ignore_failure {
                            Ok(None)
                        } else {
                            Err(ctx.throw(cmd_call, message))
                        }
                    }

                    ExecErrorNature::ParsingErr(_)
                    | ExecErrorNature::Thrown { message: _, at: _ }
                    | ExecErrorNature::Exit { code: _ }
                    | ExecErrorNature::CtrlC
                    | ExecErrorNature::Custom(_) => Err(err),
                },
            }
        },
    )
}
