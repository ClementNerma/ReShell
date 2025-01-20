use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    "flattened",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self()
    )

    -> UntypedListType
);

fn run() -> Runner {
    Runner::new(|_, Args { list }, _, _| {
        let mut flattened = vec![];

        for value in list.read_promise_no_write().iter() {
            match value {
                RuntimeValue::List(list) => {
                    flattened.extend(list.read_promise_no_write().iter().cloned())
                }
                _ => flattened.push(value.clone()),
            }
        }

        Ok(Some(RuntimeValue::List(GcCell::new(flattened))))
    })
}
