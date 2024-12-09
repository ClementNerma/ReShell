use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // convert a struct to a map
    //

    "toStruct",

    (
        map: RequiredArg<UntypedMapType> = Arg::method_self(),
        linked: PresenceFlag = Arg::long_flag("linked")
    )

    -> Some(UntypedStructType::value_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { map, linked }, _, _| {
        Ok(Some(RuntimeValue::Struct(if linked {
            map
        } else {
            GcCell::new(map.read_promise_no_write().clone())
        })))
    })
}
