use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // list keys from a structure
    //

    "fields",

    (
        obj: RequiredArg<UntypedStructType> = Arg::method_self()
    )

    -> Some(DetachedListType::<StringType>::value_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { obj }, _, _| {
        let keys = obj
            .read_promise_no_write()
            .keys()
            .map(|key| RuntimeValue::String(key.clone()))
            .collect();

        Ok(Some(RuntimeValue::List(GcCell::new(keys))))
    })
}
