use crate::{
    declare_typed_fn_handler,
    utils::{INTERNAL_CODE_RANGE, call_fn_checked},
};

crate::define_internal_fn!(
    //
    // Run a function in parallel
    //

    "parallel",

    (
        func: RequiredArg<BasicFunc> = Arg::method_self()
    )

    -> VoidType
);

declare_typed_fn_handler!(BasicFunc() -> VoidType);

fn run() -> Runner {
    Runner::new(|_, Args { func }, _, ctx| {
        let mut ctx_clone = ctx.clone();

        std::thread::spawn(move || {
            call_fn_checked(
                &LocatedValue::new(INTERNAL_CODE_RANGE, RuntimeValue::Function(func)),
                &BasicFunc::signature(),
                vec![],
                &mut ctx_clone,
            )
        });

        Ok(None)
    })
}
