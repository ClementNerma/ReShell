use super::progress_bar::ProgressBarValue;

crate::define_internal_fn!(
    "clearProgressBar",

    (
        pb: RequiredArg<CustomType<ProgressBarValue>> = Arg::method_self()
    )

    -> None
);

fn run() -> Runner {
    Runner::new(|_, Args { pb }, _, _| {
        pb.inner.finish_and_clear();

        Ok(None)
    })
}
