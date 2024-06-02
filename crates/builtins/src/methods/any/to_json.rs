use serde_json::{Number, Value};

use crate::define_internal_fn;

define_internal_fn!(
    "toJson",

    (
        value: RequiredArg<AnyType> = Arg::method_self(),
        pretty: PresenceFlag = Arg::long_and_short_flag("pretty", 'p')
    )

    -> Some(StringType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { value, pretty }, args_at, ctx| {
        let value = value_to_serde_json(value).map_err(|err| {
            ctx.throw(
                args_at.value,
                format!("failed to stringify value to JSON: {err}"),
            )
        })?;

        Ok(Some(RuntimeValue::String(if pretty {
            serde_json::to_string_pretty(&value).unwrap()
        } else {
            serde_json::to_string(&value).unwrap()
        })))
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
        RuntimeValue::CmdArg(_) => Err("cannot convert a command argument to JSON"),
        RuntimeValue::Custom(_) => Err("cannot convert a custom type to JSON"),
    }
}
