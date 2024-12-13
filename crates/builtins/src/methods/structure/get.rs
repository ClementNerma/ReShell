use crate::define_internal_fn;

define_internal_fn!(
    "get",

    (
        obj: RequiredArg<UntypedStructType> = Arg::method_self(),
        field: RequiredArg<StringType> = Arg::positional("field"),
        or_else: OptionalArg<AnyType> = Arg::long_flag("or-else")
    )

    -> Some(AnyType::value_type())
);

fn run() -> Runner {
    Runner::new(
        |at,
         Args {
             obj,
             field,
             or_else,
         },
         _,
         ctx| {
            match obj.read_promise_no_write().get(&field) {
                Some(value) => Ok(Some(value.clone())),

                None => match or_else {
                    Some(value) => Ok(Some(value.clone())),
                    None => Err(ctx.throw(at, format!("field '{field}' was not found"))),
                },
            }
        },
    )
}
