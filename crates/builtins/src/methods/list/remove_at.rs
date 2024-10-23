crate::define_internal_fn!(
    "removeAt",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        index: RequiredArg<ExactIntType<usize>> = Arg::positional("index")
    )

    -> Some(AnyType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|at, Args { list, index }, args_at, ctx| {
        let mut list = list.write(args_at.list, ctx)?;

        if index > list.len() {
            return Err(ctx.throw(
                at,
                format!(
                    "cannot remove index {index} from a list with {} elements",
                    list.len()
                ),
            ));
        }

        let removed = list.remove(index);

        Ok(Some(removed))
    })
}
