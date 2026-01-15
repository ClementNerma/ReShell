use reshell_runtime::{
    cmd::{CmdExecParams, run_cmd},
    errors::{ExecActualErrorNature, ExecError},
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
        let cmd = ctx.get_cmd_call_used_as_value(cmd_call.content_at);

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

            Err(err) => match err {
                ExecError::ActualError(err) => match err.nature {
                    ExecActualErrorNature::CommandFailedToStart { message: _ }
                    | ExecActualErrorNature::CommandFailed {
                        message: _,
                        exit_status: _,
                    } => Ok(Some(RuntimeValue::Bool(false))),

                    _ => Err(ExecError::ActualError(err)),
                },
                ExecError::InternalPropagation(_) | ExecError::TopPropagation(_) => Err(err),
            },
        }
    })
}
