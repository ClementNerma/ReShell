use crate::define_internal_fn;

define_internal_fn!(
    "get",

    (
        map: RequiredArg<UntypedMapType> = Arg::positional("map"),
        key: RequiredArg<StringType> = Arg::positional("key"),
        or_else: OptionalArg<AnyType> = Arg::long_flag("or-else")
    )

    -> Some(AnyType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(
        |_, Args { map, key, or_else }, ArgsAt { key: key_at, .. }, ctx| match map
            .read_promise_no_write()
            .get(&key)
        {
            Some(value) => Ok(Some(value.clone())),

            None => match or_else {
                Some(value) => Ok(Some(value.clone())),
                None => Err(ctx.error(key_at, format!("key '{key}' was not found"))),
            },
        },
    )
}
