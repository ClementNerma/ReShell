use serde_json::{Number, Value};

use crate::define_internal_fn;

define_internal_fn!(
    "toJson",

    (
        value: RequiredArg<AnyType> = Arg::positional("value")
    )

    -> Some(StringType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { value }, ArgsAt { value: value_at }, ctx| {
        let value = value_to_serde_json(value).map_err(|err| {
            ctx.error(
                value_at,
                format!("failed to stringify value to JSON: {err}"),
            )
        })?;

        Ok(Some(RuntimeValue::String(value.to_string())))
    })
}

fn value_to_serde_json(value: RuntimeValue) -> Result<Value, &'static str> {
    match value {
        RuntimeValue::Null => Ok(Value::Null),
        RuntimeValue::Bool(bool) => Ok(Value::Bool(bool)),
        RuntimeValue::Int(int) => Ok(Value::Number(int.into())),
        RuntimeValue::Float(float) => Ok(Value::Number(
            Number::from_f64(float).ok_or("cannot convert NaN or Infinity numbers to JSON")?,
        )),
        RuntimeValue::String(string) => Ok(Value::String(string)),
        RuntimeValue::Range { from: _, to: _ } => Err("cannot convert ranges to JSON"),
        RuntimeValue::Error(_) => Err("cannot convert an error to JSON"),
        RuntimeValue::CmdCall { content_at: _ } => Err("cannot convert a command call to JSON"),
        RuntimeValue::List(list) => list
            .read_promise_no_write()
            .iter()
            .cloned()
            .map(value_to_serde_json)
            .collect(),
        RuntimeValue::Map(map) => map
            .read_promise_no_write()
            .iter()
            .map(|(key, value)| {
                value_to_serde_json(value.clone()).map(|value| (key.clone(), value))
            })
            .collect(),
        RuntimeValue::Struct(obj) => obj
            .read_promise_no_write()
            .iter()
            .map(|(key, value)| {
                value_to_serde_json(value.clone()).map(|value| (key.clone(), value))
            })
            .collect(),
        RuntimeValue::Function(_) => Err("cannot convert a function to JSON"),
        RuntimeValue::ArgSpread(_) => Err("cannot convert a spread to JSON"),
    }
}
