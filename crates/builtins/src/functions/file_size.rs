use std::path::Path;

crate::define_internal_fn!(
    "fileSize",

    (
        path: RequiredArg<StringType> = Arg::positional("path")
    )

    -> Some(ExactIntType::<usize>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { path }, args_at, ctx| {
        let path = Path::new(&path);

        if path.is_dir() {
            return Err(ctx.error(args_at.path, "Provided path is a directory"));
        }

        if !path.is_file() {
            return Err(ctx.error(args_at.path, "Provided path is not a file"));
        }

        let stat = path.metadata().map_err(|err| {
            ctx.error(
                args_at.path,
                format!("Failed to get metadata of file: {err}"),
            )
        })?;

        let size = i64::try_from(stat.len()).map_err(|_| {
            ctx.error(
                args_at.path,
                format!(
                    "File size is too big to fit into an integer ({} bytes)",
                    stat.len()
                ),
            )
        })?;

        Ok(Some(RuntimeValue::Int(size)))
    })
}
