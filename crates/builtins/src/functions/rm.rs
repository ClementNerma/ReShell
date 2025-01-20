use std::{fs, path::Path};

crate::define_internal_fn!(
    //
    // Delete a file
    //

    "rm",

    (
        path: RequiredArg<StringType> = Arg::positional("path"),
        recursive: PresenceFlag = Arg::long_and_short_flag("recursive", 'r')
    )

    -> VoidType
);

fn run() -> Runner {
    Runner::new(|at, Args { path, recursive }, args_at, ctx| {
        let path = Path::new(&path);

        let file_type = path.metadata().map_err(|_| {
            ctx.throw(
                args_at.path,
                format!("provided path '{}' does not exist", path.display()),
            )
        })?;

        let result = if file_type.is_dir() {
            if !recursive {
                return Err(ctx.throw(args_at.path, "provided path is a directory ; to remove it, use the '--recursive' / '-r' flag."));
            }

            fs::remove_dir_all(path)
        } else {
            fs::remove_file(path)
        };

        result.map_err(|err| {
            ctx.throw(
                at,
                format!("failed to remove item at path '{}': {err}", path.display()),
            )
        })?;

        Ok(None)
    })
}
