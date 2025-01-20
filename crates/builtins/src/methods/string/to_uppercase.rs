crate::define_internal_fn!(
    "toUppercase",

    (
        string: RequiredArg<StringType> = Arg::method_self()
    )

    -> StringType
);

fn run() -> Runner {
    Runner::new(|_, Args { string }, _, _| Ok(Some(RuntimeValue::String(string.to_uppercase()))))
}
