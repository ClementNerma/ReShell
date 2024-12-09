use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // convert a struct to a map
    //

    "toStruct",

    (
        obj: RequiredArg<UntypedMapType> = Arg::method_self(),
        linked: PresenceFlag = Arg::long_flag("linked")
    )

    -> Some(UntypedStructType::value_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { obj, linked }, _, _| {
        Ok(Some(RuntimeValue::Struct(if linked {
            obj
        } else {
            GcCell::new(obj.read_promise_no_write().clone())
        })))
    })
}
