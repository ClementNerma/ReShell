crate::define_internal_fn!(
    "isEmpty",

    (
        value: RequiredArg<Union4Type<StringType, UntypedListType, UntypedMapType, SpreadType>> = Arg::method_self()
    )

    -> Some(BoolType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { value }, _, _| {
        let is_empty = match value {
            Union4Result::A(string) => string.is_empty(),
            Union4Result::B(list) => list.read_promise_no_write().is_empty(),
            Union4Result::C(map) => map.read_promise_no_write().is_empty(),
            Union4Result::D(spread) => spread.is_empty(),
        };

        Ok(Some(RuntimeValue::Bool(is_empty)))
    })
}
