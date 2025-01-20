use crate::{declare_typed_fn_handler, utils::call_fn_checked};

crate::define_internal_fn!(
    //
    // map over a list
    //

    "inspect",

    (
        value: RequiredArg<AnyType> = Arg::method_self(),
        inspect_fn: RequiredArg<InspectFn> = Arg::positional("inspect_fn")
    )

    -> AnyType
);

declare_typed_fn_handler!(InspectFn(value: AnyType) -> VoidType);

fn run() -> Runner {
    Runner::new(|_, Args { value, inspect_fn }, args_at, ctx| {
        let inspect_fn = LocatedValue::new(args_at.inspect_fn, RuntimeValue::Function(inspect_fn));

        call_fn_checked(
            &inspect_fn,
            &InspectFn::signature(),
            vec![value.clone()],
            ctx,
        )?;

        Ok(Some(value))
    })
}
