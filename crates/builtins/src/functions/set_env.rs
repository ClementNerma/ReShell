crate::define_internal_fn!(
    //
    // Set an environment variable's value
    //

    "setEnv",

    (
        var_name: RequiredArg<StringType> = Arg::positional("var_name"),
        value: RequiredArg<StringType> = Arg::positional("value")
    )

    -> VoidType
);

fn run() -> Runner {
    Runner::new(|at, Args { var_name, value }, _, ctx| {
        std::env::set_var(&var_name, value);

        if var_name == "PATH" {
            ctx.binaries_resolver()
                .refresh()
                .map_err(|err| ctx.throw(at, err.to_string()))?;
        }

        Ok(None)
    })
}
