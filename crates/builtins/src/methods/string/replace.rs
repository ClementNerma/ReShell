crate::define_internal_fn!(
    //
    // replace substrings
    //

    "replace",

    (
        source: RequiredArg<StringType> = Arg::method_self(),
        replacer: RequiredArg<StringType> = Arg::positional("replacer"),
        replacement: RequiredArg<StringType> = Arg::positional("replacement")
    )

    -> StringType
);

fn run() -> Runner {
    Runner::new(
        |_,
         Args {
             source,
             replacer,
             replacement,
         },
         _,
         _| {
            let replaced = source.replace(&replacer, &replacement);
            Ok(Some(RuntimeValue::String(replaced)))
        },
    )
}
