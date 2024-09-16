crate::define_internal_fn!(
    "afterLast",

    (
        string: RequiredArg<StringType> = Arg::method_self(),
        pattern: RequiredArg<StringType> = Arg::positional("pattern")
    )

    -> Some(NullableType::<StringType>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { string, pattern }, _, _| {
        Ok(Some(match string.rfind(&pattern) {
            Some(pos) => RuntimeValue::String(string[pos + pattern.len()..].to_owned()),
            None => RuntimeValue::Null,
        }))
    })
}
