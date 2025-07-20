crate::define_internal_fn!(
    //
    // compute the sum of all integer values in a list
    //

    "sum",

    (
        list: RequiredArg<DetachedListType<IntType>> = Arg::method_self()

    )

    -> IntType
);

fn run() -> Runner {
    Runner::new(|_, Args { list }, args_at, ctx| {
        let mut sum = 0i64;

        for (i, num) in list.into_iter().enumerate() {
            sum = sum
                .checked_add(num)
                .ok_or_else(|| ctx.error(args_at.list, format!("Overflow during sum on item {} (sum at that point: {sum}, failed to add: {num}", i + 1)))?;
        }

        Ok(Some(RuntimeValue::Int(sum)))
    })
}
