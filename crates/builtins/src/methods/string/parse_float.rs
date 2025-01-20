crate::define_internal_fn!(
    "parseFloat",

    (
        string: RequiredArg<StringType> = Arg::method_self()
    )

    -> FloatType
);

fn run() -> Runner {
    Runner::new(|at, Args { string }, _, ctx| {
        let int = string
            .parse::<f64>()
            .map_err(|err| ctx.throw(at, format!("failed to parse string as float: {err}")))?;

        Ok(Some(RuntimeValue::Float(int)))
    })
}
