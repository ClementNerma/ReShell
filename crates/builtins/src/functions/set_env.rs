use std::sync::Mutex;

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

static SETTING_ENV_VAR: Mutex<()> = Mutex::new(());

fn run() -> Runner {
    Runner::new(|at, Args { var_name, value }, _, ctx| {
        // Ensure a single thread is running at this point
        let _unused_guard = SETTING_ENV_VAR.lock().unwrap();

        #[allow(unsafe_code)]
        unsafe {
            // This is now safe as we ensured a single thread was setting the variable
            std::env::set_var(&var_name, value);
        }

        // Note that we don't drop the guard yet as we can refresh the binaries resolver first

        if var_name == "PATH" {
            ctx.binaries_resolver()
                .refresh()
                .map_err(|err| ctx.throw(at, err.to_string()))?;
        }

        Ok(None)
    })
}
