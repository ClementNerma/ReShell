use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // list keys from a map
    //

    "values",

    (
        map: RequiredArg<UntypedMapType> = Arg::method_self()
    )

    -> Some(UntypedListType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { map }, _, _| {
        let values = map.read_promise_no_write().values().cloned().collect();

        Ok(Some(RuntimeValue::List(GcCell::new(values))))
    })
}
