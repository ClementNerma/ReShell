use crate::utils::{call_fn_checked, expect_returned_value, forge_basic_fn_signature};

crate::define_internal_fn!(
    //
    // map over a list
    //

    "pipe",

    (
        value: RequiredArg<AnyType> = Arg::positional("value"),
        pipe: RequiredArg<TypedFunctionType> = pipe_type()
    )

    -> Some(UntypedListType::direct_underlying_type())
);

fn pipe_type() -> RequiredArg<TypedFunctionType> {
    RequiredArg::new(
        ArgNames::Positional("pipe"),
        TypedFunctionType::new(forge_basic_fn_signature(
            vec![("value", AnyType::direct_underlying_type())],
            Some(AnyType::direct_underlying_type()),
        )),
    )
}

fn run() -> Runner {
    Runner::new(
        |_, Args { value, pipe }, ArgsAt { pipe: pipe_at, .. }, ctx| {
            let pipe = LocatedValue::new(RuntimeValue::Function(pipe), pipe_at);

            let value = call_fn_checked(
                &pipe,
                pipe_type().base_typing().signature(),
                vec![value],
                ctx,
            )
            .and_then(|ret| expect_returned_value(ret, pipe_at, AnyType::new_direct(), ctx))?;

            Ok(Some(value))
        },
    )
}
