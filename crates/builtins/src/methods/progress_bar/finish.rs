use crate::types::ProgressBarValue;

crate::define_internal_fn!(
    "finish",

    (
        pb: RequiredArg<CustomType<ProgressBarValue>> = Arg::method_self()
    )

    -> VoidType
);

fn run() -> Runner {
    Runner::new(|_, Args { pb }, _, _| {
        pb.finish();
        pb.disable_steady_tick();

        Ok(None)
    })
}
