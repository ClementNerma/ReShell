use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // list keys from a map
    //

    "keys",

    (
        map: RequiredArg<UntypedMapType> = Arg::method_self()
    )

    -> Some(DetachedListType::<StringType>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { map }, _, _| {
        let keys = map
            .read_promise_no_write()
            .keys()
            .map(|key| RuntimeValue::String(key.clone()))
            .collect();

        Ok(Some(RuntimeValue::List(GcCell::new(keys))))
    })
}
