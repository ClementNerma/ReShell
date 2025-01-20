use reshell_runtime::{
    cmd::{run_cmd, CmdExecParams},
    errors::ExecErrorNature,
};

use crate::define_internal_fn;

define_internal_fn!(
    "succeeds",

    (
        cmd_call: RequiredArg<CmdCallType> = Arg::method_self(),
        silent: PresenceFlag = Arg::long_flag("silent")
    )

    -> BoolType
);

fn run() -> Runner {
    Runner::new(|_, Args { cmd_call, silent }, _, ctx| {
        let cmd = ctx.get_cmd_call_used_as_value(cmd_call);

        let cmd_result = run_cmd(
            &cmd,
            ctx,
            CmdExecParams {
                capture: None,
                silent,
            },
        );

        match cmd_result {
            Ok(_) => Ok(Some(RuntimeValue::Bool(true))),

            Err(err) => match err.nature {
                ExecErrorNature::CommandFailedToStart { message: _ }
                | ExecErrorNature::CommandFailed {
                    message: _,
                    exit_status: _,
                } => Ok(Some(RuntimeValue::Bool(false))),

                ExecErrorNature::ParsingErr(_)
                | ExecErrorNature::CheckingErr(_)
                | ExecErrorNature::Thrown { message: _, at: _ }
                | ExecErrorNature::CtrlC
                | ExecErrorNature::FailureExit { code: _ }
                | ExecErrorNature::Custom(_)
                | ExecErrorNature::NotAnError(_) => Err(err),
            },
        }
    })
}
