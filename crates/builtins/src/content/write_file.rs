use std::{fs, path::Path};

crate::define_internal_fn!(
    //
    // Write to a file
    //

    "writeFile",

    (
        path: RequiredArg<StringType> = Arg::positional("path"),
        content: RequiredArg<StringType> = Arg::positional("content")
    )

    -> None
);

fn run() -> Runner {
    Runner::new(|at, Args { path, content }, _, ctx| {
        let path = Path::new(&path);

        fs::write(path, content).map_err(|err| {
            ctx.error(
                at,
                format!("failed to write file '{}': {err}", path.display()),
            )
        })?;

        Ok(None)
    })
}
