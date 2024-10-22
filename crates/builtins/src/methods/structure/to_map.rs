use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // convert a struct to a map
    //

    "structToMap",

    (
        obj: RequiredArg<UntypedStructType> = Arg::method_self()
    )

    -> Some(UntypedMapType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { obj }, _, _| {
        let entries = obj.read_promise_no_write().clone();

        Ok(Some(RuntimeValue::Map(GcCell::new(entries))))
    })
}
