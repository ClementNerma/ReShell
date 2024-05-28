crate::define_internal_fn!(
    //
    // Set an environment variable's value
    //

    "set_env",

    (
        var_name: RequiredArg<StringType> = Arg::positional("var_name"),
        value: RequiredArg<StringType> = Arg::positional("value")
    )

    -> None
);

fn run() -> Runner {
    Runner::new(|_, Args { var_name, value }, _, _| {
        std::env::set_var(var_name, value);
        Ok(None)
    })
}
