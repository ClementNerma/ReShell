crate::define_internal_fn!(
    //
    // clone a value
    //

    "clone",

    (
        value: RequiredArg<AnyType> = Arg::method_self()
    )

    -> Some(AnyType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { value }, _, _| Ok(Some(value.clone())))
}
