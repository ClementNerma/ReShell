use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // Create a map (optionally from a list of entries)
    //

    "make_map",

    (
        entries: OptionalArg<
            Union2Type<
                UntypedStructType,
                DetachedListType<Tuple2Type<StringType, AnyType>>
            >
        > = Arg::positional("entries")
    )

    -> Some(UntypedMapType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { entries }, args_at, _| {
        let map = match entries {
            None => HashMap::new(),
            Some(entries) => match entries {
                Union2Result::A(obj) => obj.read(args_at.entries.unwrap()).clone(),
                Union2Result::B(tuples) => tuples.into_iter().collect(),
            },
        };

        Ok(Some(RuntimeValue::Map(GcCell::new(map))))
    })
}
