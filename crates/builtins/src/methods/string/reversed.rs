crate::define_internal_fn!(
    //
    // reverse a list's order
    //

    "reversed",

    (
        string: RequiredArg<StringType> = Arg::method_self()
    )

    -> Some(StringType::value_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { string }, _, _| {
        Ok(Some(RuntimeValue::String(string.chars().rev().collect())))
    })
}
