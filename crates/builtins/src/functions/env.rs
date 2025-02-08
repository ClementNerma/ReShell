use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // Get the value of all environment variables
    //

    "env",

    ()

    -> DetachedMapType<StringType>
);

fn run() -> Runner {
    Runner::new(|_, Args {}, _, _| {
        let vars = std::env::vars_os()
            .map(|(name, value)| {
                (
                    name.to_string_lossy().into_owned(),
                    RuntimeValue::String(value.to_string_lossy().into_owned()),
                )
            })
            .collect();

        Ok(Some(RuntimeValue::Map(GcCell::new(vars))))
    })
}
