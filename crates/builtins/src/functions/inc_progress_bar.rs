use super::progress_bar::ProgressBarValue;

crate::define_internal_fn!(
    "incProgressBar",

    (
        pb: RequiredArg<CustomType<ProgressBarValue>> = Arg::method_self(),
        amount: OptionalArg<ExactIntType<u64>> = Arg::positional("amount")
    )

    -> None
);

fn run() -> Runner {
    Runner::new(|_, Args { pb, amount }, _, _| {
        pb.inner.inc(amount.unwrap_or(1));

        Ok(None)
    })
}
