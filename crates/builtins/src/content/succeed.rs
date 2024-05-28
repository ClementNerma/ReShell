use reshell_runtime::{cmd::run_cmd, errors::ExecErrorNature};

use crate::define_internal_fn;

define_internal_fn!(
    "succeed",

    (
        cmd_call: RequiredArg<CmdCallType> = Arg::positional("cmd_call")
    )

    -> Some(BoolType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { cmd_call }, _, ctx| {
        let cmd = ctx.get_cmd_call_used_as_value(cmd_call);

        // TODO: ensure the error is for the call itself, not inside one of its inner arguments (e.g. closure) etc.
        match run_cmd(&cmd, ctx, false) {
            Ok(_) => Ok(Some(RuntimeValue::Bool(true))),
            Err(err) => match err.nature {
                ExecErrorNature::CommandFailedToStart { message: _ }
                | ExecErrorNature::CommandFailed {
                    message: _,
                    exit_status: _,
                } => Ok(Some(RuntimeValue::Bool(false))),

                ExecErrorNature::ParsingErr(_)
                | ExecErrorNature::Thrown { value: _ }
                | ExecErrorNature::Exit { code: _ }
                | ExecErrorNature::CtrlC
                | ExecErrorNature::Custom(_) => Err(err),
            },
        }
    })
}
