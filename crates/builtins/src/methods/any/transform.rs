use crate::utils::{call_fn_checked, expect_returned_value, forge_basic_fn_signature};

crate::define_internal_fn!(
    //
    // map over a list
    //

    "transform",

    (
        value: RequiredArg<AnyType> = Arg::method_self(),
        transform_fn: RequiredArg<SignatureBasedFunctionType> = transform_fn_type()
    )

    -> Some(AnyType::direct_underlying_type())
);

fn transform_fn_type() -> RequiredArg<SignatureBasedFunctionType> {
    RequiredArg::new(
        ArgNames::Positional("transform_fn"),
        SignatureBasedFunctionType::new(forge_basic_fn_signature(
            vec![("value", AnyType::direct_underlying_type())],
            Some(AnyType::direct_underlying_type()),
        )),
    )
}

fn run() -> Runner {
    Runner::new(
        |_,
         Args {
             value,
             transform_fn,
         },
         args_at,
         ctx| {
            let transform_fn =
                LocatedValue::new(args_at.transform_fn, RuntimeValue::Function(transform_fn));

            let value = call_fn_checked(
                &transform_fn,
                transform_fn_type().base_typing().signature(),
                vec![value],
                ctx,
            )
            .and_then(|ret| {
                expect_returned_value(ret, args_at.transform_fn, AnyType::new_direct(), ctx)
            })?;

            Ok(Some(value))
        },
    )
}
