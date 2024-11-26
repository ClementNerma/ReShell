use jiff::{fmt::rfc2822, Zoned};

use crate::functions::DateTimeValue;

crate::define_internal_fn!(
    "toString",

    (
        moment: RequiredArg<CustomType<DateTimeValue>> = Arg::method_self(),
        format: OptionalArg<StringType> = Arg::positional("format")
    )

    -> Some(StringType::value_type())
);

fn run() -> Runner {
    Runner::new(|at, Args { moment, format }, _, ctx| {
        // TODO: check if format is valid (otherwise the program currently panics)

        let out = match format {
            Some(format) => jiff::fmt::strtime::format(&format, &moment as &Zoned),
            None => rfc2822::to_string(&moment),
        }
        .map_err(|err| ctx.throw(at, err.to_string()))?;

        Ok(Some(RuntimeValue::String(out)))
    })
}
