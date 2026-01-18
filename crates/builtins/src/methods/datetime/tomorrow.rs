use std::sync::Arc;

use reshell_prettify::PrettyPrintable;
use reshell_runtime::pretty_impl::pretty_printable_date_time;

crate::define_internal_fn!(
    "tomorrow",

    (
        moment: RequiredArg<DateTimeType> = Arg::method_self()
    )

    -> DateTimeType
);

fn run() -> Runner {
    Runner::new(|_, Args { moment }, args_at, ctx| {
        let moment = moment.tomorrow().map_err(|err| {
            ctx.hard_error(
                args_at.moment,
                format!(
                    "Failed to get tomorrow's date from {}: {err}",
                    pretty_printable_date_time(&moment).display_inline()
                ),
            )
        })?;

        Ok(Some(RuntimeValue::DateTime(Arc::new(moment))))
    })
}
