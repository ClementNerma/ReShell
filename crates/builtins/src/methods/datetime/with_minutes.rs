use std::sync::Arc;

use jiff::civil::{DateTime, Time};
use reshell_prettify::PrettyPrintable;
use reshell_runtime::pretty_impl::pretty_printable_date_time;

crate::define_internal_fn!(
    "withMinutes",

    (
        moment: RequiredArg<DateTimeType> = Arg::method_self(),
        minutes: RequiredArg<ExactIntType<i8>> = Arg::positional("minutes")
    )

    -> DateTimeType
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
                    pretty_printable_date_time(&moment).display_inline()
                ),
            )
        })?;

        Ok(Some(RuntimeValue::DateTime(Arc::new(moment))))
    })
}
