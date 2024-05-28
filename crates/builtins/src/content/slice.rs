use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // slice a list
    //

    "slice",

    (
        list: RequiredArg<UntypedListType> = Arg::positional("list"),
        from: RequiredArg<ExactIntType<usize>> = Arg::positional("from"),
        length: OptionalArg<ExactIntType<usize>> = Arg::positional("length")
    )

    -> Some(UntypedListType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(
        |_, Args { list, from, length }, ArgsAt { list: list_at, .. }, _| {
            let sliced = list
                .read(list_at)
                .iter()
                .skip(from)
                .take(length.unwrap_or(usize::MAX))
                .cloned()
                .collect::<Vec<_>>();

            Ok(Some(RuntimeValue::List(GcCell::new(sliced))))
        },
    )
}
