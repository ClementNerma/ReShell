use crate::functions::ProgressBarValue;

crate::define_internal_fn!(
    "clear",

    (
        pb: RequiredArg<CustomType<ProgressBarValue>> = Arg::method_self()
    )

    -> None
);

fn run() -> Runner {
    Runner::new(|_, Args { pb }, _, _| {
        pb.finish_and_clear();

        Ok(None)
    })
}
