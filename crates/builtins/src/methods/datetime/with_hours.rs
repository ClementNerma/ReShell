use jiff::civil::{DateTime, Time};
use reshell_prettify::{PrettyPrintOptions, PrettyPrintable};
use reshell_runtime::gc::GcReadOnlyCell;

use crate::types::DateTimeValue;

crate::define_internal_fn!(
    "withHours",

    (
        moment: RequiredArg<CustomType<DateTimeValue>> = Arg::method_self(),
        hours: RequiredArg<ExactIntType<i8>> = Arg::positional("hours")
    )

    -> CustomType<DateTimeValue>
);

fn run() -> Runner {
    Runner::new(|_, Args { moment, hours }, args_at, ctx| {
        let moment = Time::new(
            hours,
            moment.minute(),
            moment.second(),
            moment.subsec_nanosecond(),
        )
        .and_then(|time| {
            DateTime::from_parts(moment.date(), time).to_zoned(moment.time_zone().clone())
        })
        .map_err(|err| {
            ctx.error(
                args_at.hours,
                format!(
                    "Failed to set hours to {hours} in {}: {err}",
                    moment.display(&(), PrettyPrintOptions::inline())
                ),
            )
        })?;

        Ok(Some(RuntimeValue::Custom(GcReadOnlyCell::new(Box::new(
            DateTimeValue(moment),
        )))))
    })
}
