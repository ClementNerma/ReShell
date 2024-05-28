use std::io;

crate::define_internal_fn!(
    "ask",

    (
        message: RequiredArg<StringType> = Arg::positional("message")
    )

    -> Some(StringType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|at, Args { message }, _, ctx| {
        print!("{message}");

        let mut buffer = String::new();

        io::stdin()
            .read_line(&mut buffer)
            .map_err(|err| ctx.error(at, format!("failed to read line: {err}")))?;

        Ok(Some(RuntimeValue::String(buffer)))
    })
}
