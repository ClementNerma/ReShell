use reshell_parser::ast::{CmdFlagArgName, FlagValueSeparator};
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

    -> CmdArgType
);

fn run() -> Runner {
    Runner::new(|_, Args { name, value }, args_at, ctx| {
        Ok(Some(RuntimeValue::CmdArg(Box::new(CmdArgValue::Flag(
            CmdFlagValue {
                name: RuntimeSpan {
                    at: args_at.name,
                    data: CmdFlagArgName::dynamic(name)
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
