use reshell_runtime::gc::GcCell;
use toml::{Table, Value};

use crate::define_internal_fn;

define_internal_fn!(
    "parseToml",

    (
        string: RequiredArg<StringType> = Arg::positional("string")
    )

    -> Some(AnyType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { string }, ArgsAt { string: string_at }, ctx| {
        let toml = string.parse::<Table>().map_err(|err| {
            ctx.throw(
                string_at,
                format!("Failed to parse input string as JSON: {err:?}"),
            )
        })?;

        let json = toml_to_value(Value::Table(toml)).map_err(|err| {
            ctx.throw(
                string_at,
                format!("Failed to parse input string as JSON: {err}",),
            )
        })?;

        Ok(Some(json))
    })
}

fn toml_to_value(value: Value) -> Result<RuntimeValue, String> {
    match value {
        Value::Boolean(bool) => Ok(RuntimeValue::Bool(bool)),

        Value::Integer(int) => Ok(RuntimeValue::Int(int)),

        Value::Float(float) => Ok(RuntimeValue::Float(float)),

        Value::String(string) => Ok(RuntimeValue::String(string)),

        Value::Datetime(_) => {
            // TODO
            Err("Date time not supported in TOML parser".into())
        }

        Value::Array(array) => Ok(RuntimeValue::List(GcCell::new(
            array
                .into_iter()
                .map(toml_to_value)
                .collect::<Result<_, _>>()?,
        ))),

        Value::Table(object) => Ok(RuntimeValue::Map(GcCell::new(
            object
                .into_iter()
                .map(|(key, value)| toml_to_value(value).map(|value| (key, value)))
                .collect::<Result<_, _>>()?,
        ))),
    }
}
