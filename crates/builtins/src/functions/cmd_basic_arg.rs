use reshell_runtime::cmd::CmdSingleArgResult;

use crate::define_internal_fn;

define_internal_fn!(
    "cmdBasicArg",

    (
        value: RequiredArg<StringType> = Arg::positional("arg")
    )

    -> Some(CmdArgType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { value }, at, _| {
        Ok(Some(RuntimeValue::CmdArg(Box::new(
            CmdSingleArgResult::Basic(LocatedValue::new(RuntimeValue::String(value), at.value)),
        ))))
    })
}
