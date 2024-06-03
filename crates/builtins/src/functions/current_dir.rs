crate::define_internal_fn!(
    //
    // Get the current directory
    //

    "currentDir",

    (
        lossy: PresenceFlag = Arg::long_flag("lossy")
    )

    -> Some(StringType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|at, Args { lossy }, _, ctx| {
        let current_dir = std::env::current_dir()
            .map_err(|err| ctx.error(at, format!("failed to get current directory: {err}")))?;

        let current_dir = if lossy {
            current_dir.to_string_lossy().to_string()
        } else {
            let Some(current_dir) = current_dir.to_str() else {
                return Err(ctx.error(
                    at,
                    format!(
                        "current directoy contains invalid UTF-8 characters: '{}'",
                        current_dir.display()
                    ),
                ));
            };

            current_dir.to_string()
        };

        Ok(Some(RuntimeValue::String(current_dir)))
    })
}
