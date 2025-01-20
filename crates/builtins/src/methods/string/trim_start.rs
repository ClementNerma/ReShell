use crate::define_internal_fn;

define_internal_fn!(
    "trimStart",

    (
        string: RequiredArg<StringType> = Arg::method_self(),
        pattern: OptionalArg<StringType> = Arg::positional("pattern")
    )

    -> StringType
);

fn run() -> Runner {
    Runner::new(|_, Args { string, pattern }, args_at, ctx| {
        let trimmed = match pattern {
            Some(pattern) => {
                if pattern.len() != 1 {
                    return Err(ctx.throw(
                        args_at.pattern.unwrap(),
                        format!("expected a single-character pattern, got: '{pattern:?}'"),
                    ));
                }

                string.trim_start_matches(pattern.chars().next().unwrap())
            }

            None => string.trim_start(),
        };

        Ok(Some(RuntimeValue::String(trimmed.to_owned())))
    })
}
