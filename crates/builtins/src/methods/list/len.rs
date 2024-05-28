crate::define_internal_fn!(
    //
    // Get the length of a string or the number of entries in a map / list
    //

    "len",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self()
    )

    -> Some(ExactIntType::<usize>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { list }, args_at, ctx| {
        let len = list.read(args_at.list).len();

        let len = i64::try_from(len).map_err(|_| {
            ctx.throw(
                args_at.list,
                format!("length is too big to fit in the integer type ({len})"),
            )
        })?;

        Ok(Some(RuntimeValue::Int(len)))
    })
}
