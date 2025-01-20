use std::{fs, path::Path};

crate::define_internal_fn!(
    //
    // Create a directory
    //

    "mkdir",

    (
        path: RequiredArg<StringType> = Arg::positional("path"),
        parents: PresenceFlag = Arg::long_and_short_flag("parents", 'p'),
        ignore_if_exists: PresenceFlag = Arg::long_and_short_flag("ignore-if-exists", 'i')
    )

    -> VoidType
);

fn run() -> Runner {
    Runner::new(
        |at,
         Args {
             path,
             parents,
             ignore_if_exists,
         },
         args_at,
         ctx| {
            let path = Path::new(&path);

            if path.is_dir() {
                return if ignore_if_exists || parents {
                    Ok(None)
                } else {
                    Err(ctx.throw(
                        args_at.path,
                        format!("a directory already exists at path '{}'", path.display()),
                    ))
                };
            }

            if path.exists() {
                return Err(ctx.throw(
                    args_at.path,
                    format!(
                        "a non-directory item already exists at path '{}'",
                        path.display()
                    ),
                ));
            }

            let result = if parents {
                fs::create_dir_all(path)
            } else {
                fs::create_dir(path)
            };

            result.map_err(|err| {
                ctx.throw(
                    at,
                    format!(
                        "failed to create directory at path '{}': {err}",
                        path.display()
                    ),
                )
            })?;

            Ok(None)
        },
    )
}
