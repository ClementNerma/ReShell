use std::io::{self, Write};

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

        let mut buffer = String::new();

        io::stdout()
            .flush()
            .map_err(|err| ctx.throw(at, format!("failed to flush stdout: {err}")))?;

        io::stderr()
            .flush()
            .map_err(|err| ctx.throw(at, format!("failed to flush stdout: {err}")))?;

        io::stdin()
            .read_line(&mut buffer)
            .map_err(|err| ctx.throw(at, format!("failed to read line: {err}")))?;

        if buffer.ends_with('\n') {
            buffer.pop();
        }

        Ok(Some(RuntimeValue::String(buffer)))
    })
}
