use jiff::civil::Date;
use reshell_prettify::{PrettyPrintOptions, PrettyPrintable};
use reshell_runtime::gc::GcReadOnlyCell;

use crate::types::DateTimeValue;

crate::define_internal_fn!(
    "withDay",

    (
        moment: RequiredArg<CustomType<DateTimeValue>> = Arg::method_self(),
        day: RequiredArg<ExactIntType<i8>> = Arg::positional("day")
    )

    -> CustomType<DateTimeValue>
);

fn run() -> Runner {
    Runner::new(|_, Args { moment, day }, args_at, ctx| {
        let moment = Date::new(moment.year(), moment.month(), day)
            .and_then(|date| {
                date.to_datetime(moment.time())
                    .to_zoned(moment.time_zone().clone())
            })
            .map_err(|err| {
                ctx.error(
                    args_at.day,
                    format!(
                        "Failed to set day to {day} in {}: {err}",
                        moment.display(&(), PrettyPrintOptions::inline())
                    ),
                )
            })?;

        Ok(Some(RuntimeValue::Custom(GcReadOnlyCell::new(Box::new(
            DateTimeValue(moment),
        )))))
    })
}
