use crate::functions::ProgressBarValue;

crate::define_internal_fn!(
    "finish",

    (
        pb: RequiredArg<CustomType<ProgressBarValue>> = Arg::method_self(),
        clear: PresenceFlag = Arg::long_flag("clear")
    )

    -> None
);

fn run() -> Runner {
    Runner::new(|_, Args { pb, clear }, _, _| {
        if clear {
            pb.inner.finish_and_clear();
        } else {
            pb.inner.finish();
        }

        Ok(None)
    })
}
