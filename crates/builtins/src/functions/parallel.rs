use rayon::iter::{IntoParallelIterator, ParallelIterator};
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
    Runner::new(|_, Args { func }, _, ctx| {
        let count = func.len();

        let tasks = func
            .into_par_iter()
            .map(|func| {
                let mut ctx_clone = ctx.clone();

                call_fn_checked(
                    &LocatedValue::new(INTERNAL_CODE_RANGE, RuntimeValue::Function(func)),
                    &BasicFunc::signature(),
                    vec![],
                    &mut ctx_clone,
                )
            })
            .collect::<Vec<_>>();

        let mut results = Vec::<RuntimeValue>::with_capacity(tasks.len());
        let mut error = None;

        for task in tasks {
            match task {
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
