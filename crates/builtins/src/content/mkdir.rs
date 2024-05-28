use std::{fs, path::Path};

use crate::errors::FallibleAtRuntime;

crate::define_internal_fn!(
    //
    // Create a directory
    //

    "mkdir",

    (
        path: RequiredArg<StringType> = Arg::positional("path"),
        parents: OptionalArg<BoolType> = Arg::long_and_short_flag("parents", 'p')
    )

    -> None
);

fn run() -> Runner {
    Runner::new(
        |at, Args { path, parents }, ArgsAt { path: path_at, .. }, ctx| {
            let path = Path::new(&path);

            if path.exists() {
                return Err(ctx.error(
                    path_at,
                    format!("provided path '{}' already exists", path.display()),
                ));
            }

            let result = if parents == Some(true) {
                fs::create_dir_all(path)
            } else {
                fs::create_dir(path)
            };

            result.context("failed to create directory", at, ctx)?;

            Ok(None)
        },
    )
}
