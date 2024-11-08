crate::define_internal_fn!(
    "findStr",

    (
        string: RequiredArg<StringType> = Arg::method_self(),
        pattern: RequiredArg<StringType> = Arg::positional("pattern")
    )

    -> Some(NullableType::<IntType>::value_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { string, pattern }, _, _| {
        Ok(Some(match string.find(&pattern) {
            Some(pos) => RuntimeValue::Int(pos.try_into().unwrap()),
            None => RuntimeValue::Null,
        }))
    })
}
