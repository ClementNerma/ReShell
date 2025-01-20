crate::define_internal_fn!(
    //
    // map over a list
    //

    "typename",

    (
        value: RequiredArg<AnyType> = Arg::method_self()
    )

    -> AnyType
);

fn run() -> Runner {
    Runner::new(|_, Args { value }, _, _| {
        let typename = match value {
            RuntimeValue::Void => "void",
            RuntimeValue::Null => "null",
            RuntimeValue::Bool(_) => "bool",
            RuntimeValue::Int(_) => "int",
            RuntimeValue::Float(_) => "float",
            RuntimeValue::String(_) => "string",
            RuntimeValue::Error(_) => "error",
            RuntimeValue::CmdCall { content_at: _ } => "cmdcall",
            RuntimeValue::CmdArg(_) => "cmdarg",
            RuntimeValue::List(_) => "list",
            RuntimeValue::Map(_) => "map",
            RuntimeValue::Struct(_) => "struct",
            RuntimeValue::Function(_) => "fn",
            RuntimeValue::Custom(name) => name.typename(),
        };

        Ok(Some(RuntimeValue::String(typename.to_owned())))
    })
}
