use std::sync::Arc;

use jiff::civil::{DateTime, Time};
use reshell_prettify::{PrettyPrintOptions, PrettyPrintable};
use reshell_runtime::pretty_impl::pretty_printable_date_time;

crate::define_internal_fn!(
    "withSeconds",

    (
        moment: RequiredArg<DateTimeType> = Arg::method_self(),
        seconds: RequiredArg<ExactIntType<i8>> = Arg::positional("seconds")
    )

    -> DateTimeType
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
            ctx.hard_error(
                args_at.seconds,
                format!(
                    "Failed to set seconds to {seconds} in {}: {err}",
                    pretty_printable_date_time(&moment).display(PrettyPrintOptions::inline())
                ),
            )
        })?;

        Ok(Some(RuntimeValue::DateTime(Arc::new(moment))))
    })
}
