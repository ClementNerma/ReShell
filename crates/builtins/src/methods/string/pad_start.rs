crate::define_internal_fn!(
    "padStart",

    (
        string: RequiredArg<StringType> = RequiredArg::method_self(),
        min_len: RequiredArg<ExactIntType<usize>> = RequiredArg::positional("minLen"),
        char: RequiredArg<StringType> = RequiredArg::positional("char")
    )

    -> Some(StringType::value_type())
);

fn run() -> Runner {
    Runner::new(
        |_,
         Args {
             mut string,
             min_len,
             char,
         },
         args_at,
         ctx| {
            if char.len() != 1 {
                return Err(ctx.throw(
                    args_at.char,
                    format!("Expected a single character, got '{char:?}'"),
                ));
            }

            let missing_chars = min_len.saturating_sub(string.len());

            if missing_chars > 0 {
                string = char.repeat(missing_chars) + &string;
            }

            Ok(Some(RuntimeValue::String(string)))
        },
    )
}
