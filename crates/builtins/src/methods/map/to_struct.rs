crate::define_internal_fn!(
    "toStruct",

    (
        map: RequiredArg<UntypedMapType> = RequiredArg::method_self()
    )

    -> Some(UntypedStructType::value_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { map }, _, _| Ok(Some(RuntimeValue::Struct(map))))
}
