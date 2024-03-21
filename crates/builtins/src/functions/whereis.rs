crate::define_internal_fn!(
    "whereis",

    (
        command: RequiredArg<StringType> = Arg::positional("command"),
        lossy: PresenceFlag = Arg::long_flag("lossy")
    )

    -> Some(StringType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(
        |_,
         Args { command, lossy },
         ArgsAt {
             command: command_at,
             ..
         },
         ctx| {
            let path = ctx
                .binaries_resolver()
                .resolve_binary_path(&command)
                .map_err(|err| ctx.error(command_at, err))?;

            let path = if lossy {
                path.to_string_lossy().into_owned()
            } else {
                path.to_str()
                    .ok_or_else(|| {
                        ctx.error(
                            command_at,
                            format!(
                                "command path contains invalid UTF-8 characters: {}",
                                path.display()
                            ),
                        )
                    })?
                    .to_owned()
            };

            Ok(Some(RuntimeValue::String(path)))
        },
    )
}
