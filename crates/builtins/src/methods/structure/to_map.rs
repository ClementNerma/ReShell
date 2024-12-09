use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // convert a struct to a map
    //

    "toMap",

    (
        obj: RequiredArg<UntypedStructType> = Arg::method_self(),
        linked: PresenceFlag = Arg::long_flag("linked")
    )

    -> Some(UntypedMapType::value_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { obj, linked }, _, _| {
        Ok(Some(RuntimeValue::Map(if linked {
            obj
        } else {
            GcCell::new(obj.read_promise_no_write().clone())
        })))
    })
}
