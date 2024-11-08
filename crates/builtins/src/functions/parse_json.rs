use reshell_runtime::gc::GcCell;
use serde_json::Value;

use crate::define_internal_fn;

define_internal_fn!(
    "parseJson",

    (
        string: RequiredArg<StringType> = Arg::positional("string")
    )

    -> Some(AnyType::value_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { string }, args_at, ctx| {
        let json = serde_json::from_str(&string).map_err(|err| {
            ctx.throw(
                args_at.string,
                format!("Failed to parse input string as JSON: {err:?}"),
            )
        })?;

        let json = serde_json_to_value(json).map_err(|err| {
            ctx.throw(
                args_at.string,
                format!("Failed to parse input string as JSON: {err}"),
            )
        })?;

        Ok(Some(json))
    })
}

fn serde_json_to_value(value: Value) -> Result<RuntimeValue, String> {
    match value {
        Value::Null => Ok(RuntimeValue::Null),

        Value::Bool(bool) => Ok(RuntimeValue::Bool(bool)),

        Value::Number(number) => {
            if let Some(int) = number.as_i64() {
                Ok(RuntimeValue::Int(int))
            } else if let Some(float) = number.as_f64() {
                Ok(RuntimeValue::Float(float))
            } else if let Some(uint) = number.as_u64() {
                Err(format!("found too big integer: {uint}"))
            } else {
                unreachable!()
            }
        }

        Value::String(string) => Ok(RuntimeValue::String(string)),

        Value::Array(array) => Ok(RuntimeValue::List(GcCell::new(
            array
                .into_iter()
                .map(serde_json_to_value)
                .collect::<Result<_, _>>()?,
        ))),

        Value::Object(object) => Ok(RuntimeValue::Map(GcCell::new(
            object
                .into_iter()
                .map(|(key, value)| serde_json_to_value(value).map(|value| (key, value)))
                .collect::<Result<_, _>>()?,
        ))),
    }
}
