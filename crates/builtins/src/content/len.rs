crate::define_internal_fn!(
    //
    // Get the length of a string or the number of entries in a map / list
    //

    "len",

    (
        content: RequiredArg<Union3Type<StringType, UntypedListType, UntypedMapType>> = Arg::method_self()
    )

    -> Some(ExactIntType::<usize>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { content }, args_at, ctx| {
        let len = match content {
            Union3Result::A(str) => str.len(),
            Union3Result::B(list) => list.read(args_at.content).len(),
            Union3Result::C(map) => map.read(args_at.content).len(),
        };

        let len = i64::try_from(len).map_err(|_| {
            ctx.error(
                args_at.content,
                format!("length is too big to fit in the integer type ({len})"),
            )
        })?;

        Ok(Some(RuntimeValue::Int(len)))
    })
}
