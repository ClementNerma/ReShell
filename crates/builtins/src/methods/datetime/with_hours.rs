use std::sync::Arc;

use jiff::civil::{DateTime, Time};
use reshell_prettify::{PrettyPrintOptions, PrettyPrintable};
use reshell_runtime::pretty_impl::pretty_printable_date_time;

crate::define_internal_fn!(
    "withHours",

    (
        moment: RequiredArg<DateTimeType> = Arg::method_self(),
        hours: RequiredArg<ExactIntType<i8>> = Arg::positional("hours")
    )

    -> DateTimeType
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
            ctx.hard_error(
                args_at.hours,
                format!(
                    "Failed to set hours to {hours} in {}: {err}",
                    pretty_printable_date_time(&moment).display(&(), PrettyPrintOptions::inline())
                ),
            )
        })?;

        Ok(Some(RuntimeValue::DateTime(Arc::new(moment))))
    })
}
