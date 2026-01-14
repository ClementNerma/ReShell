use std::{path::Path, sync::Arc};

use jiff::{Timestamp, Zoned, tz::TimeZone};

use crate::types::DateTimeValue;

crate::define_internal_fn!(
    "ctime",

    (
        path: RequiredArg<StringType> = Arg::positional("path")
    )

    -> CustomType<DateTimeValue>
);

fn run() -> Runner {
    Runner::new(|_, Args { path }, args_at, ctx| {
        let path = Path::new(&path);

        if !path.exists() {
            return Err(ctx.throw(args_at.path, "Provided path does not exist"));
        }

        let stat = path.metadata().map_err(|err| {
            ctx.throw(
                args_at.path,
                format!("Failed to get metadata of file: {err}"),
            )
        })?;

        let ctime = stat
            .created()
            .map_err(|_| ctx.throw(args_at.path, "Failed to get creation time"))?;

        let mtime = Timestamp::try_from(ctime)
            .map_err(|err| ctx.throw(args_at.path, format!("Failed to decode timestamp: {err}")))?;

        let mtime = Zoned::new(mtime, TimeZone::system());

        Ok(Some(RuntimeValue::Custom(Arc::new(DateTimeValue(mtime)))))
    })
}
