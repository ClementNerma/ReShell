use crate::functions::ProgressBarValue;

crate::define_internal_fn!(
    "inc",

    (
        pb: RequiredArg<CustomType<ProgressBarValue>> = Arg::method_self(),
        amount: OptionalArg<ExactIntType<u64>> = Arg::positional("amount"),
        disable_auto_finish: PresenceFlag = Arg::long_and_short_flag("disable-auto-finish", 'd')
    )

    -> VoidType
);

fn run() -> Runner {
    Runner::new(
        |_,
         Args {
             pb,
             amount,
             disable_auto_finish,
         },
         _,
         _| {
            pb.inc(amount.unwrap_or(1));

            if !disable_auto_finish && pb.length() == Some(pb.position()) {
                pb.disable_steady_tick();
                pb.finish();
            }

            Ok(None)
        },
    )
}
