use std::path::Path;

crate::define_internal_fn!(
    //
    // Write to a file
    //

    "writeToFile",

    (
        content: RequiredArg<StringType> = Arg::positional("content"),
        path: RequiredArg<StringType> = Arg::positional("path"),
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
            let () =
                super::write_file::run_write_file(Path::new(&path), &content, append, at, ctx)?;
            Ok(None)
        },
    )
}
