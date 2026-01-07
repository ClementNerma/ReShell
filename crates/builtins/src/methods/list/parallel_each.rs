use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

use crate::{declare_typed_fn_handler, helpers::runner::run_parallel, utils::call_fn_checked};

crate::define_internal_fn!(
    //
    // iterate over a list running the provided function in parallel
    //

    "parallelEach",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        for_each_fn: RequiredArg<ForEachFn> = Arg::positional("for_each_fn"),
        max_threads: OptionalArg<ExactIntType<usize>> = Arg::long_and_short_flag("max_threads", 't')
    )

    -> VoidType
);

declare_typed_fn_handler!(ForEachFn(value: AnyType) -> AnyType);

fn run() -> Runner {
    Runner::new(
        |_,
         Args {
             list,
             for_each_fn,
             max_threads,
         },
         args_at,
         ctx| {
            let for_each_fn =
                LocatedValue::new(args_at.for_each_fn, RuntimeValue::Function(for_each_fn));

            run_parallel(
                || {
                    list.read(args_at.list)
                        .par_iter()
                        .map(|value| {
                            let mut ctx = ctx.clone();

                            call_fn_checked(
                                &for_each_fn,
                                &ForEachFn::signature(),
                                vec![value.clone()],
                                &mut ctx,
                            )
                        })
                        .collect::<Result<Vec<_>, _>>()
                },
                max_threads,
            )?;

            Ok(None)
        },
    )
}
