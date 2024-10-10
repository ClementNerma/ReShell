use std::path::Path;

use reshell_runtime::gc::GcReadOnlyCell;
use time::OffsetDateTime;

use super::DateTimeValue;

crate::define_internal_fn!(
    "mtime",

    (
        path: RequiredArg<StringType> = Arg::positional("path")
    )

    -> Some(CustomType::<DateTimeValue>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { path }, args_at, ctx| {
        let path = Path::new(&path);

        if !path.exists() {
            return Err(ctx.error(args_at.path, "Provided path does not exist"));
        }

        let stat = path.metadata().map_err(|err| {
            ctx.throw(
                args_at.path,
                format!("Failed to get metadata of file: {err}"),
            )
        })?;

        let mtime = stat.modified().map_err(|_| {
            ctx.throw(
                args_at.path,
                format!(
                    "File size is too big to fit into an integer ({} bytes)",
                    stat.len()
                ),
            )
        })?;

        Ok(Some(RuntimeValue::Custom(GcReadOnlyCell::new(Box::new(
            DateTimeValue::new(OffsetDateTime::from(mtime)),
        )))))
    })
}
