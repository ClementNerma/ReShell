use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    "sort",

    (
        list: RequiredArg<Union3Type<
            DetachedListType<IntType>,
            DetachedListType<FloatType>,
            DetachedListType<StringType>
        >> = Arg::method_self()
    )

     -> Some(UntypedListType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { list }, _, _| {
        let sorted_list = match list {
            Union3Result::A(mut list) => {
                list.sort();
                list.into_iter().map(RuntimeValue::Int).collect()
            }

            Union3Result::B(mut list) => {
                list.sort_by(|a, b| a.partial_cmp(b).unwrap());
                list.into_iter().map(RuntimeValue::Float).collect()
            }

            Union3Result::C(mut list) => {
                list.sort();
                list.into_iter().map(RuntimeValue::String).collect()
            }
        };

        Ok(Some(RuntimeValue::List(GcCell::new(sorted_list))))
    })
}
