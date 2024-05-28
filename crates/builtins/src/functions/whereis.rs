crate::define_internal_fn!(
    "whereis",

    (
        command: RequiredArg<StringType> = Arg::positional("command")
    )

    -> Some(StringType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(
        |_,
         Args { command },
         ArgsAt {
             command: command_at,
         },
         ctx| {
            let path = ctx
                .binaries_resolver()
                .resolve_binary_path(&command)
                .map_err(|err| ctx.error(command_at, format!("command not found: {err}")))?;

            let path = path.to_str().ok_or_else(|| {
                ctx.error(
                    command_at,
                    format!(
                        "command path contains invalid UTF-8 characters: {}",
                        path.display()
                    ),
                )
            })?;

            Ok(Some(RuntimeValue::String(path.to_owned())))
        },
    )
}
