use jiff::fmt::rfc2822;

use crate::functions::DateTimeValue;

crate::define_internal_fn!(
    "toString",

    (
        moment: RequiredArg<CustomType<DateTimeValue>> = Arg::method_self(),
        format: OptionalArg<StringType> = Arg::positional("format")
    )

    -> Some(StringType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|at, Args { moment, format }, _, ctx| {
        let out = match format {
            Some(format) => moment.strftime(&format).to_string(),

            None => rfc2822::to_string(&moment)
                .map_err(|err| ctx.throw(at, format!("Failed to format date/time: {err}")))?,
        };

        Ok(Some(RuntimeValue::String(out)))
    })
}
