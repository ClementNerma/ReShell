use reshell_runtime::values::ErrorValueContent;

crate::define_internal_fn!(
    //
    // Create an error value
    //

    "error",

    (
        data: RequiredArg<AnyType> = Arg::positional("data")
    )

    -> Some(ErrorType::value_type())
);

fn run() -> Runner {
    Runner::new(|at, Args { data }, _, ctx| match at {
        RuntimeCodeRange::Parsed(at) => {
            Ok(Some(RuntimeValue::Error(Box::new(ErrorValueContent {
                at,
                data,
            }))))
        }

        RuntimeCodeRange::Internal(_) => {
            Err(ctx.throw(at, "cannot generate an error from an internal location"))
        }
    })
}
