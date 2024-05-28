use reshell_runtime::{
    cmd::{run_cmd, CmdExecParams, CmdPipeCapture},
    errors::ExecErrorNature,
};

use crate::define_internal_fn;

define_internal_fn!(
    "runCmd",

    (
        cmd_call: RequiredArg<CmdCallType> = Arg::method_self(),
        silent: PresenceFlag = Arg::long_flag("silent")
    )

    -> None
);

fn run() -> Runner {
    Runner::new(|_, Args { cmd_call, silent }, _, ctx| {
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
                    Err(ctx.throw(cmd_call, format!("failed to start command: {message}")))
                }

                ExecErrorNature::CommandFailed {
                    message,
                    exit_status: _,
                } => Err(ctx.throw(cmd_call, message)),

                ExecErrorNature::ParsingErr(_)
                | ExecErrorNature::Thrown { message: _, at: _ }
                | ExecErrorNature::Exit { code: _ }
                | ExecErrorNature::CtrlC
                | ExecErrorNature::Custom(_) => Err(err),
            },
        }
    })
}
