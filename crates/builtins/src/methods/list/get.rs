use crate::define_internal_fn;

define_internal_fn!(
    "get",

    (
        map: RequiredArg<UntypedMapType> = Arg::method_self(),
        key: RequiredArg<StringType> = Arg::positional("key"),
        or_else: OptionalArg<AnyType> = Arg::long_flag("or-else")
    )

    -> Some(AnyType::value_type())
);

fn run() -> Runner {
    Runner::new(|at, Args { map, key, or_else }, _, ctx| {
        match map.read_promise_no_write().get(&key) {
            Some(value) => Ok(Some(value.clone())),

            None => match or_else {
                Some(value) => Ok(Some(value.clone())),
                None => Err(ctx.throw(at, format!("key '{key}' was not found"))),
            },
        }
    })
}
