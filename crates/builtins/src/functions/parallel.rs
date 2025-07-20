use reshell_runtime::gc::GcCell;

use crate::{
    declare_typed_fn_handler,
    utils::{INTERNAL_CODE_RANGE, call_fn_checked},
};

crate::define_internal_fn!(
    //
    // Run a set of functions in parallel
    //

    "parallel",

    (
        func: RequiredArg<DetachedListType<BasicFunc>> = Arg::method_self()
    )

    -> DetachedListType<AnyType>
);

declare_typed_fn_handler!(BasicFunc() -> AnyType);

fn run() -> Runner {
    Runner::new(|_, Args { func }, args_at, ctx| {
        let count = func.len();

        let tasks = func
            .into_iter()
            .map(|func| {
                let mut ctx_clone = ctx.clone();

                std::thread::spawn(move || {
                    call_fn_checked(
                        &LocatedValue::new(INTERNAL_CODE_RANGE, RuntimeValue::Function(func)),
                        &BasicFunc::signature(),
                        vec![],
                        &mut ctx_clone,
                    )
                })
            })
            .collect::<Vec<_>>();

        let mut results = Vec::<RuntimeValue>::with_capacity(tasks.len());
        let mut error = None;

        for task in tasks {
            let result = task.join().unwrap_or_else(|err| {
                let panic_msg = if let Some(str) = err.downcast_ref::<&'static str>() {
                    str
                } else if let Some(string) = err.downcast_ref::<String>() {
                    string.as_str()
                } else {
                    "Thread panicked with invalid message"
                };

                ctx.panic(args_at.func, panic_msg)
            });

            match result {
                Ok(result) => results.push(match result {
                    Some(loc_val) => loc_val.value,
                    None => RuntimeValue::Null,
                }),

                Err(err) => {
                    error = Some(err);
                    break;
                }
            }
        }

        if let Some(err) = error {
            return Err(err);
        }

        assert_eq!(count, results.len());

        Ok(Some(RuntimeValue::List(GcCell::new(results))))
    })
}
