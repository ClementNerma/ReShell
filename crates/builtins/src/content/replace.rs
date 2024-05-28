crate::define_internal_fn!(
    //
    // replace substrings
    //

    "replace",

    (
        source: RequiredArg<StringType> = Arg::method_self(),
        lookfor: RequiredArg<StringType> = Arg::positional("lookfor"),
        replacement: RequiredArg<StringType> = Arg::positional("replacement")
    )

    -> Some(StringType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(
        |_,
         Args {
             source,
             lookfor,
             replacement,
         },
         _,
         _| {
            Ok(Some(RuntimeValue::String(
                source.replace(&lookfor, &replacement),
            )))
        },
    )
}
