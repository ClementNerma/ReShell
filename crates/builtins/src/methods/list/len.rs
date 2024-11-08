crate::define_internal_fn!(
    //
    // Get the length of a string or the number of entries in a map / list
    //

    "len",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self()
    )

    -> Some(ExactIntType::<usize>::value_type())
);

fn run() -> Runner {
    Runner::new(|at, Args { list }, _, ctx| {
        let len = list.read_promise_no_write().len();

        let len = i64::try_from(len).map_err(|_| {
            ctx.throw(
                at,
                format!("length is too big to fit in the integer type ({len})"),
            )
        })?;

        Ok(Some(RuntimeValue::Int(len)))
    })
}
