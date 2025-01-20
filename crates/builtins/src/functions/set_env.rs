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
        let is_path_var = var_name.eq_ignore_ascii_case("PATH");

        std::env::set_var(var_name, value);

        if is_path_var {
            ctx.binaries_resolver()
                .refresh()
                .map_err(|err| ctx.throw(at, err.to_string()))?;
        }

        Ok(None)
    })
}
