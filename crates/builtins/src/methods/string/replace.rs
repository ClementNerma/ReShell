use crate::types::RegexValue;

crate::define_internal_fn!(
    //
    // replace substrings
    //

    "replace",

    (
        source: RequiredArg<StringType> = Arg::method_self(),
        replacer: RequiredArg<Union2Type<StringType, CustomType<RegexValue>>> = Arg::positional("replacer"),
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
            let replaced = match replacer {
                Union2Result::A(string) => source.replace(&string, &replacement),
                Union2Result::B(regex) => regex.replace(&source, &replacement).into_owned(),
            };

            Ok(Some(RuntimeValue::String(replaced)))
        },
    )
}
