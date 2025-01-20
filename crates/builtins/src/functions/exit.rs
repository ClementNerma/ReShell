use std::num::NonZero;

crate::define_internal_fn!(
    //
    // Exit the program (optionally with an error code)
    //

    "exit",

    (
        code: OptionalArg<ExactIntType<u8>> = Arg::positional("code")
    )

    -> VoidType
);

fn run() -> Runner {
    Runner::new(|at, Args { code }, _, ctx| {
        Err(match code.and_then(|code| NonZero::try_from(code).ok()) {
            Some(code) => ctx.failure_exit(at, code),
            None => ctx.successful_exit(at),
        })
    })
}
