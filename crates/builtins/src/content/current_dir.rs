use crate::errors::FallibleAtRuntime;

crate::define_internal_fn!(
    //
    // Get the current directory
    //

    "current_dir",

    (
        lossy: OptionalArg<BoolType> = Arg::long_flag("lossy")
    )

    -> Some(StringType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|at, Args { lossy }, _, ctx| {
        let current_dir =
            std::env::current_dir().context("failed to get current directory", at, ctx)?;

        let current_dir = if lossy != Some(true) {
            current_dir
                .to_str()
                .with_context(
                    || {
                        format!(
                            "current directoy contains invalid UTF-8 characters: '{}'",
                            current_dir.display()
                        )
                    },
                    at,
                    ctx,
                )?
                .to_string()
        } else {
            current_dir.to_string_lossy().to_string()
        };

        Ok(Some(RuntimeValue::String(current_dir)))
    })
}
