use jiff::{Zoned, fmt::rfc2822};

use crate::types::DateTimeValue;

crate::define_internal_fn!(
    "toString",

    (
        moment: RequiredArg<CustomType<DateTimeValue>> = Arg::method_self(),
        format: OptionalArg<StringType> = Arg::positional("format")
    )

    -> StringType
);

fn run() -> Runner {
    Runner::new(|at, Args { moment, format }, _, ctx| {
        let out = match format {
            Some(format) => jiff::fmt::strtime::format(&format, &moment as &Zoned),
            None => rfc2822::to_string(&moment),
        }
        .map_err(|err| ctx.throw(at, err.to_string()))?;

        Ok(Some(RuntimeValue::String(out)))
    })
}
