use std::path::Path;

use crate::errors::FallibleAtRuntime;

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
            return Err(ctx.error(
                path_at,
                format!("directory '{}' does not exist", path.display()),
            ));
        }

        std::env::set_current_dir(path).context("failed to change current directory", at, ctx)?;

        Ok(None)
    })
}
