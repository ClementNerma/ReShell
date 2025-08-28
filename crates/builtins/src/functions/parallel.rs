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
        func: RequiredArg<DetachedListType<BasicFunc>> = Arg::positional("func")
    )

    -> DetachedListType<AnyType>
);

declare_typed_fn_handler!(BasicFunc() -> AnyType);

fn run() -> Runner {
    Runner::new(|_, Args { func }, _, ctx| {
        let count = func.len();

        let results = func
            .into_par_iter()
            .map(|func| {
                let mut ctx_clone = ctx.clone();

                call_fn_checked(
                    &LocatedValue::new(INTERNAL_CODE_RANGE, RuntimeValue::Function(func)),
                    &BasicFunc::signature(),
                    vec![],
                    &mut ctx_clone,
                )
                .map(|ret_val| ret_val.map_or(RuntimeValue::Null, |loc_val| loc_val.value))
            })
            .collect::<Result<Vec<_>, _>>()?;

        assert_eq!(count, results.len());

        Ok(Some(RuntimeValue::List(GcCell::new(results))))
    })
}
