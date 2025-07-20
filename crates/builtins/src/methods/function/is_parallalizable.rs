use crate::declare_typed_fn_handler;

crate::define_internal_fn!(
    //
    // Check if a function can be run in parallel
    //

    "isParallelizable",

    (
        func: RequiredArg<BasicFunc> = Arg::method_self()
    )

    -> BoolType
);

declare_typed_fn_handler!(BasicFunc() -> VoidType);

fn run() -> Runner {
    Runner::new(|_, Args { func }, _, _| {
        let deps = func.captured_deps.get();
        Ok(Some(RuntimeValue::Bool(deps.as_ref().unwrap().is_empty())))
    })
}
