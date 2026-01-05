crate::define_internal_fn!(
    "findStr",

    (
        string: RequiredArg<StringType> = Arg::method_self(),
        pattern: RequiredArg<StringType> = Arg::positional("pattern")
    )

    -> NullableType<IntType>
);

fn run() -> Runner {
    Runner::new(|_, Args { string, pattern }, _, _| {
        Ok(Some(match string.find(&pattern) {
            Some(pos) => {
                let chars = string[..pos].chars().count();
                RuntimeValue::Int(chars.try_into().unwrap())
            }

            None => RuntimeValue::Null,
        }))
    })
}
