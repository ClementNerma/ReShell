use std::sync::Arc;

use jiff::civil::Date;
use reshell_prettify::PrettyPrintable;
use reshell_runtime::pretty_impl::pretty_printable_date_time;

crate::define_internal_fn!(
    "withMonth",

    (
        moment: RequiredArg<DateTimeType> = Arg::method_self(),
        month: RequiredArg<ExactIntType<i8>> = Arg::positional("month")
    )

    -> DateTimeType
);

fn run() -> Runner {
    Runner::new(|_, Args { moment, month }, args_at, ctx| {
        let moment = Date::new(moment.year(), month, moment.day())
            .and_then(|date| {
                date.to_datetime(moment.time())
                    .to_zoned(moment.time_zone().clone())
            })
            .map_err(|err| {
                ctx.hard_error(
                    args_at.month,
                    format!(
                        "Failed to set month to {month} in {}: {err}",
                        pretty_printable_date_time(&moment).display_inline()
                    ),
                )
            })?;

        Ok(Some(RuntimeValue::DateTime(Arc::new(moment))))
    })
}
