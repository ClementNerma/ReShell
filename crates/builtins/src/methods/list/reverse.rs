use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // reverse a list's order
    //

    "reverse",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self()
    )

    -> Some(UntypedListType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { list }, ArgsAt { list: list_at }, _| {
        let mut items = list.read(list_at).clone();

        items.reverse();

        Ok(Some(RuntimeValue::List(GcCell::new(items))))
    })
}
