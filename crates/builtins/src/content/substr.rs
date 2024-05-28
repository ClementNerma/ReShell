crate::define_internal_fn!(
    "substr",

    (
        string: RequiredArg<StringType> = Arg::positional("string"),
        from: RequiredArg<ExactIntType<usize>> = Arg::positional("from"),
        to: OptionalArg<ExactIntType<usize>> = Arg::positional("to")
    )

    -> Some(StringType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { string, from, to }, _, _| {
        let from = string.chars().skip(from);

        let result = match to {
            Some(to) => from.take(to).collect(),
            None => from.collect(),
        };

        Ok(Some(RuntimeValue::String(result)))
    })
}
