use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    "concat",

    (
        list: RequiredArg<UntypedListType> = RequiredArg::method_self(),
        concat: RequiredArg<UntypedListType> = RequiredArg::positional("concat")
    )

    -> Some(UntypedListType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { list, concat }, _, _| {
        let mut list = list.read_promise_no_write().clone();

        list.extend(concat.read_promise_no_write().iter().cloned());

        Ok(Some(RuntimeValue::List(GcCell::new(list))))
    })
}
