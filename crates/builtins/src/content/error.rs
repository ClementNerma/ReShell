crate::define_internal_fn!(
    //
    // Create a range value
    //

    "error",

    (
        content: RequiredArg<StringType> = Arg::positional("content")
    )

    -> Some(RangeType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|at, Args { content }, _, ctx| match at {
        RuntimeCodeRange::Parsed(at) => Ok(Some(RuntimeValue::Error { at, msg: content })),
        RuntimeCodeRange::Internal => {
            Err(ctx.error(at, "cannot generate an error from an internal location"))
        }
    })
}
