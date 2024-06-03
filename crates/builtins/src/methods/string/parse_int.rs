crate::define_internal_fn!(
    "parseInt",

    (
        string: RequiredArg<StringType> = Arg::method_self()
    )

    -> Some(ExactIntType::<i64>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { string }, args_at, ctx| {
        let int = string.parse::<i64>().map_err(|err| {
            ctx.throw(
                args_at.string,
                format!("failed to parse string as integer: {err}"),
            )
        })?;

        Ok(Some(RuntimeValue::Int(int)))
    })
}
