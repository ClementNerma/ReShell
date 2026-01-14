use std::sync::Arc;

use jiff::{Timestamp, Zoned, tz::TimeZone};

use crate::define_internal_fn;

define_internal_fn!(
    "datetime",

    (
        secs_since_epoch: OptionalArg<ExactIntType<i64>> = Arg::positional("from-secs-since-unix-epoch")
    )

    -> DateTimeType
);

fn run() -> Runner {
    Runner::new(|_, Args { secs_since_epoch }, args_at, ctx| {
        let moment = match secs_since_epoch {
            None => Zoned::now(),

            Some(secs) => {
                let ts = Timestamp::new(secs, 0).map_err(|err| {
                    ctx.hard_error(
                        args_at.secs_since_epoch.unwrap(),
                        format!("Failed to create date time from provided seconds: {err}"),
                    )
                })?;

                Zoned::new(ts, TimeZone::system())
            }
        };

        Ok(Some(RuntimeValue::DateTime(Arc::new(moment))))
    })
}
