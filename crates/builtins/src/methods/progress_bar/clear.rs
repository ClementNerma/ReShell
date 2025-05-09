use crate::types::ProgressBarValue;

crate::define_internal_fn!(
    "clear",

    (
        pb: RequiredArg<CustomType<ProgressBarValue>> = Arg::method_self()
    )

    -> VoidType
);

fn run() -> Runner {
    Runner::new(|_, Args { pb }, _, _| {
        pb.finish_and_clear();

        Ok(None)
    })
}
