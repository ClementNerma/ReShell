use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // reverse a list's items' order into a new list
    //

    "enumerate",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self()
    )

    -> UntypedListType
);

fn run() -> Runner {
    Runner::new(|_, Args { list }, _, _| {
        let items = list.read_promise_no_write();

        let items = items
            .iter()
            .enumerate()
            .map(|(i, item)| {
                RuntimeValue::List(GcCell::new(vec![
                    RuntimeValue::Int(i64::try_from(i).unwrap()),
                    item.clone(),
                ]))
            })
            .collect();

        Ok(Some(RuntimeValue::List(GcCell::new(items))))
    })
}
