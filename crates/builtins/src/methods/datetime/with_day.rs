use std::sync::Arc;

use jiff::civil::Date;
use reshell_prettify::{PrettyPrintOptions, PrettyPrintable};
use reshell_runtime::pretty_impl::pretty_printable_date_time;

crate::define_internal_fn!(
    "withDay",

    (
        moment: RequiredArg<DateTimeType> = Arg::method_self(),
        day: RequiredArg<ExactIntType<i8>> = Arg::positional("day")
    )

    -> DateTimeType
);

fn run() -> Runner {
    Runner::new(|_, Args { moment, day }, args_at, ctx| {
        let moment = Date::new(moment.year(), moment.month(), day)
            .and_then(|date| {
                date.to_datetime(moment.time())
                    .to_zoned(moment.time_zone().clone())
            })
            .map_err(|err| {
                ctx.hard_error(
                    args_at.day,
                    format!(
                        "Failed to set day to {day} in {}: {err}",
                        pretty_printable_date_time(&moment)
                            .display(&(), PrettyPrintOptions::inline()),
                    ),
                )
            })?;

        Ok(Some(RuntimeValue::DateTime(Arc::new(moment))))
    })
}
