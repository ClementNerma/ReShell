use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // slice a list
    //

    "slice",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        from: RequiredArg<ExactIntType<usize>> = Arg::positional("from"),
        length: OptionalArg<ExactIntType<usize>> = Arg::positional("length")
    )

    -> Some(UntypedListType::value_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { list, from, length }, _, _| {
        let sliced = list
            .read_promise_no_write()
            .iter()
            .skip(from)
            .take(length.unwrap_or(usize::MAX))
            .cloned()
            .collect::<Vec<_>>();

        Ok(Some(RuntimeValue::List(GcCell::new(sliced))))
    })
}
