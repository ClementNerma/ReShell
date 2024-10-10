use time::format_description;
use time::format_description::well_known::Rfc2822;

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
    Runner::new(|at, Args { moment, format }, args_at, ctx| {
        let out = match format {
            Some(format) => {
                let format = format_description::parse(&format).map_err(|err| {
                    ctx.throw(
                        args_at.format.unwrap(),
                        format!("Failed to parse date/time formatting: {err}"),
                    )
                })?;

                moment
                    .0
                    .format(&format)
                    .map_err(|err| ctx.throw(at, format!("Failed to format date/time: {err}")))?
            }

            None => moment
                .0
                .format(&Rfc2822)
                .map_err(|err| ctx.throw(at, format!("Failed to format date/time: {err}")))?,
        };

        Ok(Some(RuntimeValue::String(out)))
    })
}
