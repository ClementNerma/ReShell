use crate::{declare_typed_fn_handler, helpers::runner::parallel_map, utils::call_fn_checked};

crate::define_internal_fn!(
    //
    // iterate over a list running the provided function in parallel
    //

    "parallelEach",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        for_each_fn: RequiredArg<ForEachFn> = Arg::positional("for_each_fn"),
        max_threads: OptionalArg<ExactIntType<usize>> = Arg::long_and_short_flag("max-threads", 't'),
        best_ordered: PresenceFlag = Arg::long_and_short_flag("best-ordered", 'b')
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
             best_ordered,
         },
         args_at,
         ctx| {
            let for_each_fn =
                LocatedValue::new(args_at.for_each_fn, RuntimeValue::Function(for_each_fn));

            parallel_map(
                list.read(args_at.list).as_slice(),
                |value| {
                    let mut ctx = ctx.clone();

                    call_fn_checked(
                        &for_each_fn,
                        &ForEachFn::signature(),
                        vec![value.clone()],
                        &mut ctx,
                    )
                },
                max_threads,
                best_ordered,
            )?;

            Ok(None)
        },
    )
}
