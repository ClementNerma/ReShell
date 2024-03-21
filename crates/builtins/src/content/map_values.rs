use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // list keys from a map
    //

    "mapValues",

    (
        map: RequiredArg<UntypedMapType> = Arg::positional("map")
    )

    -> Some(DetachedListType::<AnyType>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { map }, ArgsAt { map: map_at }, _| {
        let values = map.read(map_at).values().cloned().collect();

        Ok(Some(RuntimeValue::List(GcCell::new(values))))
    })
}
