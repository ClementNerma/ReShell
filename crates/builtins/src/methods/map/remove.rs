crate::define_internal_fn!(
    "remove",

    (
        map: RequiredArg<UntypedMapType> = Arg::method_self(),
        key: RequiredArg<StringType> = Arg::positional("key")
    )

    -> AnyType
);

fn run() -> Runner {
    Runner::new(|at, Args { map, key }, args_at, ctx| {
        let mut map = map.write(args_at.map, ctx)?;

        let removed = map
            .shift_remove(&key)
            .ok_or_else(|| ctx.throw(at, format!("key {key:?} was not found in the map")))?;

        Ok(Some(removed))
    })
}
