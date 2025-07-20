use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

use crate::{declare_typed_fn_handler, utils::call_fn_checked};

crate::define_internal_fn!(
    //
    // map over a list running the provided function in parallel
    //

    "parallelEach",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        for_each_fn: RequiredArg<ForEachFn> = Arg::positional("for_each_fn")
    )

    -> VoidType
);

declare_typed_fn_handler!(ForEachFn(value: AnyType) -> AnyType);

fn run() -> Runner {
    Runner::new(|_, Args { list, for_each_fn }, args_at, ctx| {
        let for_each_fn =
            LocatedValue::new(args_at.for_each_fn, RuntimeValue::Function(for_each_fn));

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
            .collect::<Result<Vec<_>, _>>()?;

        Ok(None)
    })
}
