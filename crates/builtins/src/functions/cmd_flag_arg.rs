use parsy::Eaten;
use reshell_parser::ast::{CmdFlagNameArg, FlagValueSeparator};
use reshell_runtime::cmd::{CmdSingleArgResult, FlagArgValueResult};

use crate::define_internal_fn;

define_internal_fn!(
    "cmdFlagArg",

    (
        name: RequiredArg<StringType> = Arg::positional("name"),
        value: OptionalArg<StringType> = Arg::positional("value")
    )

    -> Some(CmdArgType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { name, value }, at, ctx| {
        let mut name_chars = name.chars();

        let flag_name = match (name_chars.next(), name_chars.next()) {
            (None, None) => return Err(ctx.error(at.name, "Flag name cannot be empty")),
            (Some(c), None) => CmdFlagNameArg::Short(c),
            (Some(_), Some(_)) => CmdFlagNameArg::LongNoConvert(name),
            (None, Some(_)) => unreachable!(),
        };

        Ok(Some(RuntimeValue::CmdArg(Box::new(
            CmdSingleArgResult::Flag {
                name: match at.name {
                    RuntimeCodeRange::Parsed(parsed) => {
                        RuntimeEaten::Parsed(Eaten::ate(parsed, flag_name))
                    }

                    RuntimeCodeRange::Internal(loc) => RuntimeEaten::Internal(flag_name, loc),
                },
                value: value.map(|value| FlagArgValueResult {
                    value: LocatedValue::new(RuntimeValue::String(value), at.value.unwrap()),
                    value_sep: FlagValueSeparator::Equal,
                }),
            },
        ))))
    })
}
