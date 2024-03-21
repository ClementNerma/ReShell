crate::define_internal_fn!(
    "toLowercase",

    (
        string: RequiredArg<StringType> = Arg::method_self()
    )

    -> Some(NullableType::<StringType>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { string }, _, _| Ok(Some(RuntimeValue::String(string.to_lowercase()))))
}
