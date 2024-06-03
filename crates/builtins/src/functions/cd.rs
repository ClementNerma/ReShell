use std::path::Path;

crate::define_internal_fn!(
    //
    // Change the current directory
    //

    "cd",

    (
        path: RequiredArg<StringType> = Arg::positional("path")
    )

    -> None
);

fn run() -> Runner {
    Runner::new(|at, Args { path }, ArgsAt { path: path_at }, ctx| {
        let trimmed_path = path.trim_end_matches(['/', '\\']);

        let path = Path::new(if trimmed_path.is_empty() {
            path.as_str()
        } else {
            trimmed_path
        });

        if !path.is_dir() {
            return Err(ctx.throw(
                path_at,
                format!("directory '{}' was not found", path.display()),
            ));
        }

        std::env::set_current_dir(path)
            .map_err(|err| ctx.error(at, format!("failed to change current directory: {err}")))?;

        ctx.trigger_directory_jump_event(at)?;

        Ok(None)
    })
}
