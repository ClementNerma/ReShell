crate::define_internal_fn!(
    //
    // Exit the program (optionally with an error code)
    //

    "exit",

    (
        code: OptionalArg<ExactIntType<u8>> = Arg::positional("code")
    )

    -> None
);

fn run() -> Runner {
    Runner::new(|at, Args { code }, _, ctx| Err(ctx.exit(at, code)))
}
