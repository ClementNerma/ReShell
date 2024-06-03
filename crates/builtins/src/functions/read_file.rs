use std::{fs, path::Path};

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
            return Err(ctx.throw(
                path_at,
                format!("no file exists at path '{}'", path.display()),
            ));
        }

        let content = fs::read_to_string(path).map_err(|err| {
            ctx.error(
                at,
                format!("failed to read file '{}': {err}", path.display()),
            )
        })?;

        Ok(Some(RuntimeValue::String(content)))
    })
}
