use console::Term;

crate::define_internal_fn!(
    "ask",

    (
        message: RequiredArg<StringType> = Arg::positional("message")
    )

    -> StringType
);

fn run() -> Runner {
    Runner::new(|at, Args { message }, _, ctx| {
        print!("{message}");

        // TODO: try to not rely on an external crate (`console`)
        let term = Term::stdout();

        let input = term
            .read_line()
            .map_err(|err| ctx.throw(at, format!("Failed to read input: {err}")))?;

        Ok(Some(RuntimeValue::String(input)))
    })
}
