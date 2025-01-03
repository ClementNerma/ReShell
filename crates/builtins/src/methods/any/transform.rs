use crate::{
    declare_typed_fn_handler,
    utils::{call_fn_checked, expect_returned_value, forge_basic_fn_signature},
};

crate::define_internal_fn!(
    //
    // map over a list
    //

    "transform",

    (
        value: RequiredArg<AnyType> = Arg::method_self(),
        transform_fn: RequiredArg<TransformFn> = Arg::positional("transformFn")
    )

    -> Some(AnyType::value_type())
);

declare_typed_fn_handler!(TransformFn => forge_basic_fn_signature(
    vec![("value", AnyType::value_type())],
    Some(AnyType::value_type()),
));

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

            let value = call_fn_checked(&transform_fn, &TransformFn::signature(), vec![value], ctx)
                .and_then(|ret| {
                    expect_returned_value::<_, AnyType>(ret, args_at.transform_fn, ctx)
                })?;

            Ok(Some(value))
        },
    )
}
