use std::path::Path;

crate::define_internal_fn!(
    //
    // Change the current directory
    //

    "cd",

    (
        path: RequiredArg<StringType> = Arg::positional("path")
    )

    -> VoidType
);

fn run() -> Runner {
    Runner::new(|at, Args { path }, args_at, ctx| {
        let trimmed_path = path.trim_end_matches(['/', '\\']);

        let path = Path::new(if trimmed_path.is_empty() {
            path.as_str()
        } else {
            trimmed_path
        });

        if !path.is_dir() {
            return Err(ctx.throw(
                args_at.path,
                format!("directory '{}' was not found", path.display()),
            ));
        }

        change_current_dir(path, at, ctx)?;

        Ok(None)
    })
}

pub fn change_current_dir(
    path: impl AsRef<Path>,
    at: RuntimeCodeRange,
    ctx: &mut Context,
) -> ExecResult<()> {
    let path = path.as_ref();

    std::env::set_current_dir(path).map_err(|err| {
        ctx.throw(
            at,
            format!(
                "failed to change current directory to '{}': {err}",
                path.display()
            ),
        )
    })?;

    ctx.trigger_directory_jump_event(at)?;

    Ok(())
}
