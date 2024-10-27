use reshell_parser::ast::{CmdFlagNameArg, FlagValueSeparator};
use reshell_runtime::{
    cmd::FlagArgValueResult,
    values::{CmdArgValue, CmdFlagValue},
};

crate::define_internal_fn!(
    "cmdFlag",

    (
        name: RequiredArg<StringType> = Arg::positional("name"),
        value: OptionalArg<AnyType> = Arg::positional("value")
    )

    -> Some(CmdArgType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { name, value }, args_at, ctx| {
        Ok(Some(RuntimeValue::CmdArg(Box::new(CmdArgValue::Flag(
            CmdFlagValue {
                name: RuntimeEaten {
                    at: args_at.name,
                    data: CmdFlagNameArg::dynamic(name)
                        .map_err(|err| ctx.throw(args_at.name, err))?,
                },
                value: value.map(|value| FlagArgValueResult {
                    value: LocatedValue::new(args_at.value.unwrap(), value),
                    value_sep: FlagValueSeparator::Equal,
                }),
            },
        )))))
    })
}
