crate::define_internal_fn!(
    "stringPos",

    (
        string: RequiredArg<StringType> = Arg::positional("string"),
        pattern: RequiredArg<StringType> = Arg::positional("pattern")
    )

    -> Some(NullableType::<IntType>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { string, pattern }, _, _| {
        Ok(Some(
            match string.find(&pattern) {
                Some(pos) => RuntimeValue::Int(pos.try_into().unwrap()),
                None => RuntimeValue::Null,
            }
        ))
    })
}
