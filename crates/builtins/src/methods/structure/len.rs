crate::define_internal_fn!(
    //
    // Get the number of entries in a structure
    //

    "len",

    (
        obj: RequiredArg<UntypedStructType> = Arg::method_self()
    )

    -> Some(ExactIntType::<usize>::value_type())
);

fn run() -> Runner {
    Runner::new(|at, Args { obj }, args_at, ctx| {
        let len = obj.read(args_at.obj).len();

        let len = i64::try_from(len).map_err(|_| {
            ctx.throw(
                at,
                format!("length is too big to fit in the integer type ({len})"),
            )
        })?;

        Ok(Some(RuntimeValue::Int(len)))
    })
}
