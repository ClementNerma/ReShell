use crate::functions::ProgressBarValue;

crate::define_internal_fn!(
    "finish",

    (
        pb: RequiredArg<CustomType<ProgressBarValue>> = Arg::method_self(),
        clear: PresenceFlag = Arg::long_flag("clear")
    )

    -> VoidType
);

fn run() -> Runner {
    Runner::new(|_, Args { pb, clear }, _, _| {
        if clear {
            pb.finish_and_clear();
        } else {
            pb.abandon();
        }

        Ok(None)
    })
}
