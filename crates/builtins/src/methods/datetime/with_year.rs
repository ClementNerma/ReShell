use std::sync::Arc;

use jiff::civil::Date;
use reshell_prettify::{PrettyPrintOptions, PrettyPrintable};

use crate::types::DateTimeValue;

crate::define_internal_fn!(
    "withYear",

    (
        moment: RequiredArg<CustomType<DateTimeValue>> = Arg::method_self(),
        year: RequiredArg<ExactIntType<i16>> = Arg::positional("year")
    )

    -> CustomType<DateTimeValue>
);

fn run() -> Runner {
    Runner::new(|_, Args { moment, year }, args_at, ctx| {
        let moment = Date::new(year, moment.month(), moment.day())
            .and_then(|date| {
                date.to_datetime(moment.time())
                    .to_zoned(moment.time_zone().clone())
            })
            .map_err(|err| {
                ctx.hard_error(
                    args_at.year,
                    format!(
                        "Failed to set year to {year} in {}: {err}",
                        moment.display(&(), PrettyPrintOptions::inline())
                    ),
                )
            })?;

        Ok(Some(RuntimeValue::Custom(Arc::new(DateTimeValue(moment)))))
    })
}
