crate::define_internal_fn!(
    "repeat",

    (
        string: RequiredArg<StringType> = Arg::method_self(),
        count: RequiredArg<ExactIntType<usize>> = Arg::positional("count")
    )

    -> StringType
);

fn run() -> Runner {
    Runner::new(|_, Args { string, count }, _, _| {
        Ok(Some(RuntimeValue::String(string.repeat(count))))
    })
}
