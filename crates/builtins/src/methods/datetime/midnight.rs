use std::sync::Arc;

use jiff::fmt::rfc2822;

use crate::{define_internal_fn, types::DateTimeValue};

define_internal_fn!(
    // return the provided datetime with midnight time

    "midnight",

    (
        datetime: RequiredArg<CustomType<DateTimeValue>> = Arg::method_self()
    )

    -> CustomType<DateTimeValue>
);

fn run() -> Runner {
    Runner::new(|_, Args { datetime }, args_at, ctx| {
        let datetime_at_midnight = datetime
            .date()
            .at(0, 0, 0, 0)
            .to_zoned(datetime.time_zone().clone())
            .map_err(|err| {
                ctx.throw(
                    args_at.datetime,
                    format!(
                        "cannot get midnight for provided datetime '{}': {err}",
                        rfc2822::to_string(&datetime).unwrap(),
                    ),
                )
            })?;

        Ok(Some(RuntimeValue::Custom(Arc::new(DateTimeValue(
            datetime_at_midnight,
        )))))
    })
}
