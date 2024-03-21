use std::{fs, path::Path};

use crate::errors::FallibleAtRuntime;

crate::define_internal_fn!(
    //
    // Read a file
    //

    "readFile",

    (
        path: RequiredArg<StringType> = Arg::positional("path")
    )

    -> Some(StringType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|at, Args { path }, ArgsAt { path: path_at }, ctx| {
        let path = Path::new(&path);

        if !path.is_file() {
            return Err(ctx.error(
                path_at,
                format!("no file exists at path '{}'", path.display()),
            ));
        }

        let content = fs::read_to_string(path).with_context(
            || format!("failed to read file '{}'", path.display()),
            at,
            ctx,
        )?;

        Ok(Some(RuntimeValue::String(content)))
    })
}
