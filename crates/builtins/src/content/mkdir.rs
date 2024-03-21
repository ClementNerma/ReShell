use std::{fs, path::Path};

use crate::errors::FallibleAtRuntime;

crate::define_internal_fn!(
    //
    // Create a directory
    //

    "mkdir",

    (
        path: RequiredArg<StringType> = Arg::positional("path"),
        parents: OptionalArg<BoolType> = Arg::long_and_short_flag("parents", 'p'),
        ignore_if_exists: OptionalArg<BoolType> = Arg::long_and_short_flag("ignore-if-exists", 'e')
    )

    -> None
);

fn run() -> Runner {
    Runner::new(
        |at,
         Args {
             path,
             parents,
             ignore_if_exists,
         },
         ArgsAt { path: path_at, .. },
         ctx| {
            let path = Path::new(&path);

            if path.is_dir() {
                return if ignore_if_exists == Some(true) {
                    Ok(None)
                } else {
                    Err(ctx.error(
                        path_at,
                        format!("a directory already exists at path '{}'", path.display()),
                    ))
                };
            }

            if path.exists() {
                return Err(ctx.error(
                    path_at,
                    format!(
                        "a non-directory item already exists at path '{}'",
                        path.display()
                    ),
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
