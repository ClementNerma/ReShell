crate::define_internal_fn!(
    //
    // check if a key exists in a struct
    //

    "has",

    (
        obj: RequiredArg<UntypedStructType> = Arg::method_self(),
        field: RequiredArg<StringType> = Arg::positional("field")
    )

    -> BoolType
);

fn run() -> Runner {
    Runner::new(|_, Args { obj, field }, _, _| {
        Ok(Some(RuntimeValue::Bool(
            obj.read_promise_no_write().contains_key(&field),
        )))
    })
}
