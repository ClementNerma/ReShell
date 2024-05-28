crate::define_internal_fn!(
    //
    // Display a message
    //

    "take",

    (
        value: RequiredArg<AnyType> = Arg::positional("value")
    )

    -> None
);

fn run() -> Runner {
    Runner::new(|_, Args { value }, _, _| Ok(Some(value)))
}
