use std::{
    fs::File,
    io::{BufWriter, Write},
};

crate::define_internal_fn!(
    //
    // Download a remote resource (URL)
    //

    "fetchFile",

    (
        url: RequiredArg<StringType> = Arg::positional("url"),
        path: RequiredArg<StringType> = Arg::positional("path")
    )

    -> VoidType
);

fn run() -> Runner {
    Runner::new(|_, Args { url, path }, args_at, ctx| {
        let mut res = reqwest::blocking::get(&url)
            .and_then(|res| res.error_for_status())
            .map_err(|err| {
                ctx.throw(args_at.url, format!("Failed to fetch URL {url:?}: {err:?}"))
            })?;

        let file = File::create(path)
            .map_err(|err| ctx.throw(args_at.path, format!("Failed to create file: {err:?}")))?;

        let mut writer = BufWriter::new(file);

        res.copy_to(&mut writer)
            .map_err(|err| ctx.throw(args_at.url, format!("Failed to write to file: {err:?}")))?;

        writer
            .flush()
            .map_err(|err| ctx.throw(args_at.path, format!("Failed to flush file: {err:?}")))?;

        Ok(None)
    })
}
