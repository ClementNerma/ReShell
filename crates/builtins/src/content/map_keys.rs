use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // list keys from a map
    //

    "mapKeys",

    (
        map: RequiredArg<UntypedMapType> = Arg::positional("map")
    )

    -> Some(DetachedListType::<StringType>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { map }, ArgsAt { map: map_at }, _| {
        let keys = map
            .read(map_at)
            .keys()
            .map(|key| RuntimeValue::String(key.clone()))
            .collect();

        Ok(Some(RuntimeValue::List(GcCell::new(keys))))
    })
}
