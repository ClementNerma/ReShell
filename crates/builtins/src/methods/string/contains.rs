use crate::define_internal_fn;

define_internal_fn!(
    "contains",

    (
        string: RequiredArg<StringType> = Arg::method_self(),
        sub: RequiredArg<StringType> = Arg::positional("sub")
    )

    -> BoolType
);

fn run() -> Runner {
    Runner::new(|_, Args { string, sub }, _, _| Ok(Some(RuntimeValue::Bool(string.contains(&sub)))))
}
