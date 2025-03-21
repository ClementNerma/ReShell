use std::{
    fs::{self, OpenOptions},
    io::Write,
    path::Path,
};

crate::define_internal_fn!(
    //
    // Write to a file
    //

    "writeFile",

    (
        path: RequiredArg<StringType> = Arg::positional("path"),
        content: RequiredArg<StringType> = Arg::positional("content"),
        append: PresenceFlag = Arg::long_and_short_flag("append", 'a')
    )

    -> VoidType
);

fn run() -> Runner {
    Runner::new(
        |at,
         Args {
             path,
             content,
             append,
         },
         _,
         ctx| {
            let () = run_write_file(Path::new(&path), &content, append, at, ctx)?;
            Ok(None)
        },
    )
}

pub fn run_write_file(
    path: &Path,
    content: &str,
    append: bool,
    at: RuntimeCodeRange,
    ctx: &mut Context,
) -> ExecResult<()> {
    if !append {
        fs::write(path, content).map_err(|err| {
            ctx.throw(
                at,
                format!("failed to write file '{}': {err}", path.display()),
            )
        })?;
    } else {
        OpenOptions::new()
            .create(true)
            .truncate(false)
            .append(true)
            .open(path)
            .map_err(|err| {
                ctx.throw(
                    at,
                    format!("failed to open file '{}': {err}", path.display()),
                )
            })?
            .write_all(format!("{content}\n").as_bytes())
            .map_err(|err| {
                ctx.throw(
                    at,
                    format!("failed to append to file '{}': {err}", path.display()),
                )
            })?
    }

    Ok(())
}
