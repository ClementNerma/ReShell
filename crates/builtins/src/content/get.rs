use crate::define_internal_fn;

define_internal_fn!(
    "get",

    (
        map: RequiredArg<UntypedMapType> = Arg::positional("map"),
        key: RequiredArg<StringType> = Arg::positional("key")
    )

    -> Some(AnyType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { map, key }, ArgsAt { key: key_at, .. }, ctx| {
        match map.with_ref(|map| map.get(&key).cloned()) {
            Some(value) => Ok(Some(value.clone())),

            None => Err(ctx.error(key_at, format!("key '{key}' was not found"))),
        }
    })
}