crate::define_internal_fn!(
    //
    // Display a message
    //

    "echo",

    (
        message: RequiredArg<Union3Type<StringType, IntType, FloatType>> = Arg::positional("message")
    )

    -> None
);

fn run() -> Runner {
    Runner::new(|_, Args { message }, _, _| {
        println!(
            "{}",
            match message {
                Union3Result::A(string) => string,
                Union3Result::B(int) => int.to_string(),
                Union3Result::C(float) => float.to_string(),
            }
        );

        Ok(None)
    })
}
