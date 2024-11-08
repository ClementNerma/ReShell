use crate::define_internal_fn;

define_internal_fn!(
    "canonicalize",

    (
        path: RequiredArg<StringType> = Arg::positional("path"),
        // simplified: PresenceFlag = Arg::long_and_short_flag("simplified", 's'),
        lossy: PresenceFlag = Arg::long_flag("lossy")
    )

    -> Some(StringType::value_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { path, lossy }, args_at, ctx| {
        let canon = dunce::canonicalize(path).map_err(|err| {
            ctx.throw(args_at.path, format!("failed to canonicalize path: {err}"))
        })?;

        let Some(canon) = canon.to_str() else {
            return if lossy {
                Ok(Some(RuntimeValue::String(
                    canon.to_string_lossy().into_owned(),
                )))
            } else {
                Err(ctx.throw(
                    args_at.path,
                    format!(
                        "canonicalized path contains invalid UTF-8 characters: {}",
                        canon.display()
                    ),
                ))
            };
        };

        Ok(Some(RuntimeValue::String(canon.to_owned())))
    })
}
