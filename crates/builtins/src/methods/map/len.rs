crate::define_internal_fn!(
    //
    // Get the length of a string or the number of entries in a map / list
    //

    "len",

    (
        content: RequiredArg<UntypedMapType> = Arg::method_self()
    )

    -> Some(ExactIntType::<usize>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|at, Args { content }, args_at, ctx| {
        let len = content.read(args_at.content).len();

        let len = i64::try_from(len).map_err(|_| {
            ctx.throw(
                at,
                format!("length is too big to fit in the integer type ({len})"),
            )
        })?;

        Ok(Some(RuntimeValue::Int(len)))
    })
}
