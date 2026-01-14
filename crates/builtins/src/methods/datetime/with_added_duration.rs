use std::sync::Arc;

use jiff::ZonedArithmetic;
use reshell_prettify::{PrettyPrintOptions, PrettyPrintable};

use crate::types::{DateTimeValue, DurationValue};

crate::define_internal_fn!(
    "withAddedDuration",

    (
        moment: RequiredArg<CustomType<DateTimeValue>> = Arg::method_self(),
        duration: RequiredArg<CustomType<DurationValue>> = Arg::positional("duration")
    )

    -> CustomType<DateTimeValue>
);

fn run() -> Runner {
    Runner::new(|_, Args { moment, duration }, args_at, ctx| {
        let moment = moment
            .checked_add(ZonedArithmetic::from(*duration))
            .map_err(|err| {
                ctx.hard_error(
                    args_at.duration,
                    format!(
                        "Failed to add duration {} to {}: {err}",
                        duration.display(&(), PrettyPrintOptions::inline()),
                        moment.display(&(), PrettyPrintOptions::inline())
                    ),
                )
            })?;

        Ok(Some(RuntimeValue::Custom(Arc::new(DateTimeValue(moment)))))
    })
}
