crate::define_internal_fn!(
    //
    // Get the value of an environment variable
    //

    "env",

    (
        var_name: RequiredArg<StringType> = Arg::positional("var_name"),
        lossy: PresenceFlag = Arg::long_flag("lossy")
    )

    -> Some(StringType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(
        |_,
         Args { var_name, lossy },
         ArgsAt {
             var_name: var_name_at,
             ..
         },
         ctx| {
            let var_value = std::env::var_os(&var_name).ok_or_else(|| {
                ctx.error(
                    var_name_at,
                    format!("environment variable '{var_name}' is not set"),
                )
            })?;

            let var_value = if lossy {
                var_value.to_string_lossy().into_owned()
            } else {
                var_value
                .to_str()
                .ok_or_else(|| ctx.error(var_name_at, format!("environment variable '{var_name}' contains invalid UTF-8 characters: '{}'", var_value.to_string_lossy())))?
                .to_owned()
            };

            Ok(Some(RuntimeValue::String(var_value)))
        },
    )
}
