crate::define_internal_fn!(
    "parseInt",

    (
        string: RequiredArg<StringType> = Arg::method_self()
    )

    -> ExactIntType<i64>
);

fn run() -> Runner {
    Runner::new(|at, Args { string }, _, ctx| {
        let int = string
            .parse::<i64>()
            .map_err(|err| ctx.throw(at, format!("failed to parse string as integer: {err}")))?;

        Ok(Some(RuntimeValue::Int(int)))
    })
}
