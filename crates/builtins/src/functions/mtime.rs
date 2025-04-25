use std::path::Path;

use jiff::{Timestamp, Zoned, tz::TimeZone};
use reshell_runtime::gc::GcReadOnlyCell;

use crate::types::DateTimeValue;

crate::define_internal_fn!(
    "mtime",

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

        let mtime = stat
            .modified()
            .map_err(|_| ctx.throw(args_at.path, "Failed to get modification time"))?;

        let mtime = Timestamp::try_from(mtime)
            .map_err(|err| ctx.throw(args_at.path, format!("Failed to decode timestamp: {err}")))?;

        let mtime = Zoned::new(mtime, TimeZone::system());

        Ok(Some(RuntimeValue::Custom(GcReadOnlyCell::new(Box::new(
            DateTimeValue(mtime),
        )))))
    })
}
