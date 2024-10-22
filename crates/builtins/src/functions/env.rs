crate::define_internal_fn!(
    //
    // Get the value of an environment variable
    //

    "env",

    (
        var_name: RequiredArg<StringType> = Arg::positional("var_name"),
        lossy: PresenceFlag = Arg::long_flag("lossy")
    )

    -> Some(NullableType::<StringType>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { var_name, lossy }, args_at, ctx| {
        let Some(var_value) = std::env::var_os(&var_name) else {
            return Ok(Some(RuntimeValue::Null));
        };

        let var_value = if lossy {
            var_value.to_string_lossy().into_owned()
        } else {
            var_value
                .to_str()
                .ok_or_else(|| ctx.throw(args_at.var_name, format!("environment variable '{var_name}' contains invalid UTF-8 characters: '{}'", var_value.to_string_lossy())))?
                .to_owned()
        };

        Ok(Some(RuntimeValue::String(var_value)))
    })
}
