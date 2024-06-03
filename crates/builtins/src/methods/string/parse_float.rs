crate::define_internal_fn!(
    "parseFloat",

    (
        string: RequiredArg<StringType> = Arg::method_self()
    )

    -> Some(FloatType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { string }, args_at, ctx| {
        let int = string.parse::<f64>().map_err(|err| {
            ctx.throw(
                args_at.string,
                format!("failed to parse string as float: {err}"),
            )
        })?;

        Ok(Some(RuntimeValue::Float(int)))
    })
}
