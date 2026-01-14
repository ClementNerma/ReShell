use jiff::civil::{DateTime, Time};
use reshell_prettify::{PrettyPrintOptions, PrettyPrintable};
use reshell_runtime::gc::GcReadOnlyCell;

use crate::types::DateTimeValue;

crate::define_internal_fn!(
    "withMinutes",

    (
        moment: RequiredArg<CustomType<DateTimeValue>> = Arg::method_self(),
        minutes: RequiredArg<ExactIntType<i8>> = Arg::positional("minutes")
    )

    -> CustomType<DateTimeValue>
);

fn run() -> Runner {
    Runner::new(|_, Args { moment, minutes }, args_at, ctx| {
        let moment = Time::new(
            moment.hour(),
            minutes,
            moment.second(),
            moment.subsec_nanosecond(),
        )
        .and_then(|time| {
            DateTime::from_parts(moment.date(), time).to_zoned(moment.time_zone().clone())
        })
        .map_err(|err| {
            ctx.hard_error(
                args_at.minutes,
                format!(
                    "Failed to set minutes to {minutes} in {}: {err}",
                    moment.display(&(), PrettyPrintOptions::inline())
                ),
            )
        })?;

        Ok(Some(RuntimeValue::Custom(GcReadOnlyCell::new(Box::new(
            DateTimeValue(moment),
        )))))
    })
}
