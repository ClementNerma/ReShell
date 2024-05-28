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
    Runner::new(|_, Args { map }, ArgsAt { map: map_at }, _| {
        let values = map
            .read(map_at)
            .values()
            .map(|value| value.clone())
            .collect();

        Ok(Some(RuntimeValue::List(GcCell::new(values))))
    })
}
