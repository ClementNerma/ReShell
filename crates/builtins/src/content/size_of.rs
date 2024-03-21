use reshell_runtime::size::ComputableSize;

crate::define_internal_fn!(
    //
    // Display a message
    //

    "sizeOf",

    (
        value: RequiredArg<AnyType> = Arg::positional("value")
    )

    -> Some(IntType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|at, Args { value }, _, ctx| {
        let size = value.compute_heap_size();

        i64::try_from(size)
            .map(|value| Some(RuntimeValue::Int(value)))
            .map_err(|_| {
                ctx.error(
                    at,
                    format!(
                        "value size is too big to be represented: {}",
                        value.compute_heap_size()
                    ),
                )
            })
    })
}
