use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // list keys from a map
    //

    "values",

    (
        obj: RequiredArg<UntypedStructType> = Arg::method_self()
    )

    -> Some(UntypedListType::value_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { obj }, _, _| {
        let values = obj.read_promise_no_write().values().cloned().collect();

        Ok(Some(RuntimeValue::List(GcCell::new(values))))
    })
}
