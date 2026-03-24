crate::define_internal_fn!(
    //
    // Fetch a web page's text
    //

    "fetchText",

    (
        url: RequiredArg<StringType> = Arg::positional("url")
    )

    -> StringType
);

fn run() -> Runner {
    Runner::new(|_, Args { url }, args_at, ctx| {
        let text = reqwest::blocking::get(&url)
            .and_then(|res| res.error_for_status())
            .and_then(|res| res.text())
            .map_err(|err| {
                ctx.throw(args_at.url, format!("Failed to fetch URL {url:?}: {err:?}"))
            })?;

        Ok(Some(RuntimeValue::String(text)))
    })
}
