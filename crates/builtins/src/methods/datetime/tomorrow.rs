use reshell_prettify::{PrettyPrintOptions, PrettyPrintable};
use reshell_runtime::gc::GcReadOnlyCell;

use crate::types::DateTimeValue;

crate::define_internal_fn!(
    "tomorrow",

    (
        moment: RequiredArg<CustomType<DateTimeValue>> = Arg::method_self()
    )

    -> CustomType<DateTimeValue>
);

fn run() -> Runner {
    Runner::new(|_, Args { moment }, args_at, ctx| {
        let moment = moment.tomorrow().map_err(|err| {
            ctx.error(
                args_at.moment,
                format!(
                    "Failed to get tomorrow's date from {}: {err}",
                    moment.display(&(), PrettyPrintOptions::inline())
                ),
            )
        })?;

        Ok(Some(RuntimeValue::Custom(GcReadOnlyCell::new(Box::new(
            DateTimeValue(moment),
        )))))
    })
}
