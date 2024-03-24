crate::define_internal_fn!(
    //
    // check if a key exists in a map
    //

    "has",

    (
        map: RequiredArg<UntypedMapType> = Arg::method_self(),
        key: RequiredArg<StringType> = Arg::positional("key")
    )

    -> Some(BoolType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { map, key }, _, _| {
        Ok(Some(RuntimeValue::Bool(
            map.read_promise_no_write().contains_key(&key),
        )))
    })
}
