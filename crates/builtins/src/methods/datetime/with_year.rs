use std::sync::Arc;

use jiff::civil::Date;
use reshell_prettify::{PrettyPrintOptions, PrettyPrintable};
use reshell_runtime::pretty_impl::pretty_printable_date_time;

crate::define_internal_fn!(
    "withYear",

    (
        moment: RequiredArg<DateTimeType> = Arg::method_self(),
        year: RequiredArg<ExactIntType<i16>> = Arg::positional("year")
    )

    -> DateTimeType
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
                        pretty_printable_date_time(&moment)
                            .display(&(), PrettyPrintOptions::inline())
                    ),
                )
            })?;

        Ok(Some(RuntimeValue::DateTime(Arc::new(moment))))
    })
}
