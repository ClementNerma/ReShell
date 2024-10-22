use std::path::Path;

use colored::Colorize;
use reshell_runtime::errors::ExecInfoType;

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
        ctx.throw(at, format!("failed to change current directory: {err}"))
            .with_info(
                ExecInfoType::Note,
                format!("tried path: {}", path.to_string_lossy().bright_blue()),
            )
    })?;

    ctx.trigger_directory_jump_event(at)?;

    Ok(())
}
