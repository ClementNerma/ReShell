crate::define_internal_fn!(
    "whereis",

    (
        command: RequiredArg<StringType> = Arg::positional("command"),
        lossy: PresenceFlag = Arg::long_flag("lossy")
    )

    -> Some(NullableType::<StringType>::direct_underlying_type())
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
            let Ok(path) = ctx.binaries_resolver().resolve_binary_path(&command) else {
                return Ok(Some(RuntimeValue::Null));
            };

            let path = if lossy {
                path.to_string_lossy().into_owned()
            } else {
                path.to_str()
                    .ok_or_else(|| {
                        ctx.throw(
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
