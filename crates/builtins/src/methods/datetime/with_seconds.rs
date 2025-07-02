use jiff::civil::{DateTime, Time};
use reshell_prettify::{PrettyPrintOptions, PrettyPrintable};
use reshell_runtime::gc::GcReadOnlyCell;

use crate::types::DateTimeValue;

crate::define_internal_fn!(
    "withSeconds",

    (
        moment: RequiredArg<CustomType<DateTimeValue>> = Arg::method_self(),
        seconds: RequiredArg<ExactIntType<i8>> = Arg::positional("seconds")
    )

    -> CustomType<DateTimeValue>
);

fn run() -> Runner {
    Runner::new(|_, Args { moment, seconds }, args_at, ctx| {
        let moment = Time::new(
            moment.hour(),
            moment.minute(),
            seconds,
            moment.subsec_nanosecond(),
        )
        .and_then(|time| {
            DateTime::from_parts(moment.date(), time).to_zoned(moment.time_zone().clone())
        })
        .map_err(|err| {
            ctx.error(
                args_at.seconds,
                format!(
                    "Failed to set seconds to {seconds} in {}: {err}",
                    moment.display(&(), PrettyPrintOptions::inline())
                ),
            )
        })?;

        Ok(Some(RuntimeValue::Custom(GcReadOnlyCell::new(Box::new(
            DateTimeValue(moment),
        )))))
    })
}
