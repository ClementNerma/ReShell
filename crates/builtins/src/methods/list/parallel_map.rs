use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use reshell_runtime::gc::GcCell;

use crate::{
    declare_typed_fn_handler,
    helpers::runner::run_parallel,
    utils::{call_fn_checked, expect_returned_value},
};

crate::define_internal_fn!(
    //
    // map over a list running the provided function in parallel
    //

    "parallelMap",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        mapper_fn: RequiredArg<MapperFn> = Arg::positional("mapper_fn"),
        max_threads: OptionalArg<ExactIntType<usize>> = Arg::long_and_short_flag("max-threads", 't')
    )

    -> UntypedListType
);

declare_typed_fn_handler!(MapperFn(value: AnyType) -> AnyType);

fn run() -> Runner {
    Runner::new(
        |_,
         Args {
             list,
             mapper_fn,
             max_threads,
         },
         args_at,
         ctx| {
            let for_each_fn =
                LocatedValue::new(args_at.mapper_fn, RuntimeValue::Function(mapper_fn));

            let results = run_parallel(
                || {
                    list.read(args_at.list)
                        .par_iter()
                        .map(|value| {
                            let mut ctx = ctx.clone();

                            call_fn_checked(
                                &for_each_fn,
                                &MapperFn::signature(),
                                vec![value.clone()],
                                &mut ctx,
                            )
                            .and_then(|ret| {
                                expect_returned_value::<_, AnyType>(ret, args_at.mapper_fn, &ctx)
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()
                },
                max_threads,
            )?;

            Ok(Some(RuntimeValue::List(GcCell::new(results))))
        },
    )
}
