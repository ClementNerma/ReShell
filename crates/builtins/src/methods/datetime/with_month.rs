use jiff::civil::Date;
use reshell_prettify::{PrettyPrintOptions, PrettyPrintable};
use reshell_runtime::gc::GcReadOnlyCell;

use crate::types::DateTimeValue;

crate::define_internal_fn!(
    "withMonth",

    (
        moment: RequiredArg<CustomType<DateTimeValue>> = Arg::method_self(),
        month: RequiredArg<ExactIntType<i8>> = Arg::positional("month")
    )

    -> CustomType<DateTimeValue>
);

fn run() -> Runner {
    Runner::new(|_, Args { moment, month }, args_at, ctx| {
        let moment = Date::new(moment.year(), month, moment.day())
            .and_then(|date| {
                date.to_datetime(moment.time())
                    .to_zoned(moment.time_zone().clone())
            })
            .map_err(|err| {
                ctx.error(
                    args_at.month,
                    format!(
                        "Failed to set month to {month} in {}: {err}",
                        moment.display(&(), PrettyPrintOptions::inline())
                    ),
                )
            })?;

        Ok(Some(RuntimeValue::Custom(GcReadOnlyCell::new(Box::new(
            DateTimeValue(moment),
        )))))
    })
}
