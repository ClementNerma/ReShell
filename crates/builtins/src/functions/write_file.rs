use std::{
    fs::{self, OpenOptions},
    io::Write,
    path::Path,
};

use crate::errors::FallibleAtRuntime;

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

    -> None
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
            let path = Path::new(&path);

            if !append {
                fs::write(path, content).with_context(
                    || format!("failed to write file '{}'", path.display()),
                    at,
                    ctx,
                )?;
            } else {
                OpenOptions::new()
                    .create(true)
                    .truncate(false)
                    .append(true)
                    .open(path)
                    .with_context(
                        || format!("failed to open file '{}'", path.display()),
                        at,
                        ctx,
                    )?
                    .write_all(content.as_bytes())
                    .with_context(
                        || format!("failed to append to file '{}'", path.display()),
                        at,
                        ctx,
                    )?
            }

            Ok(None)
        },
    )
}
