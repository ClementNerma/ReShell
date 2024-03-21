use std::{fs, path::Path};

use crate::errors::FallibleAtRuntime;

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

        fs::write(path, content).with_context(
            || format!("failed to write file '{}'", path.display()),
            at,
            ctx,
        )?;

        Ok(None)
    })
}
