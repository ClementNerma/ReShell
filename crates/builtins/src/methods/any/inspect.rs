use crate::utils::{call_fn_checked, forge_basic_fn_signature};

crate::define_internal_fn!(
    //
    // map over a list
    //

    "inspect",

    (
        value: RequiredArg<AnyType> = Arg::method_self(),
        inspect_fn: RequiredArg<TypedFunctionType> = inspect_fn_type()
    )

    -> Some(AnyType::direct_underlying_type())
);

fn inspect_fn_type() -> RequiredArg<TypedFunctionType> {
    RequiredArg::new(
        ArgNames::Positional("inspect_fn"),
        TypedFunctionType::new(forge_basic_fn_signature(
            vec![("value", AnyType::direct_underlying_type())],
            None,
        )),
    )
}

fn run() -> Runner {
    Runner::new(|_, Args { value, inspect_fn }, args_at, ctx| {
        let inspect_fn = LocatedValue::new(args_at.inspect_fn, RuntimeValue::Function(inspect_fn));

        call_fn_checked(
            &inspect_fn,
            inspect_fn_type().base_typing().signature(),
            vec![value.clone()],
            ctx,
        )?;

        Ok(Some(value))
    })
}
