use reshell_runtime::{pretty_impl::pretty_printable_code_range, values::ErrorValueContent};

crate::define_internal_fn!(
    //
    // Create an error value
    //

    "error",

    (
        data: RequiredArg<AnyType> = Arg::positional("data")
    )

    -> ErrorType
);

fn run() -> Runner {
    Runner::new(|at, Args { data }, _, ctx| match at {
        RuntimeCodeRange::Parsed(at) => {
            Ok(Some(RuntimeValue::Error(Box::new(ErrorValueContent {
                at,
                data,
                pretty_at: pretty_printable_code_range(at, ctx.files_map()),
            }))))
        }

        RuntimeCodeRange::Internal(_) => {
            Err(ctx.throw(at, "cannot generate an error from an internal location"))
        }
    })
}
