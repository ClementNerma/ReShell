use std::{fs, path::Path};

use crate::errors::FallibleAtRuntime;

crate::define_internal_fn!(
    //
    // Delete a file
    //

    "rm",

    (
        path: RequiredArg<StringType> = Arg::positional("path"),
        recursive: PresenceFlag = Arg::long_and_short_flag("recursive", 'r')
    )

    -> None
);

fn run() -> Runner {
    Runner::new(
        |at, Args { path, recursive }, ArgsAt { path: path_at, .. }, ctx| {
            let path = Path::new(&path);

            let file_type = path.metadata().map_err(|_| {
                ctx.throw(
                    path_at,
                    format!("provided path '{}' does not exist", path.display()),
                )
            })?;

            let result = if file_type.is_dir() {
                if !recursive {
                    return Err(ctx.throw(path_at, "provided path is a directory ; to remove it, use the '--recursive' / '-r' flag."));
                }

                fs::remove_dir_all(path)
            } else {
                fs::remove_file(path)
            };

            result.context("failed to remove item", at, ctx)?;

            Ok(None)
        },
    )
}
