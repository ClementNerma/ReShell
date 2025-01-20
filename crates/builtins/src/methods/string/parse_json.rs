use reshell_runtime::gc::GcCell;
use serde_json::Value;

use crate::define_internal_fn;

define_internal_fn!(
    "parseJson",

    (
        string: RequiredArg<StringType> = Arg::method_self(),
        use_maps: PresenceFlag = Arg::long_and_short_flag("use-maps", 'm')
    )

    -> AnyType
);

fn run() -> Runner {
    Runner::new(|_, Args { string, use_maps }, args_at, ctx| {
        let json = serde_json::from_str(&string).map_err(|err| {
            ctx.throw(
                args_at.string,
                format!("Failed to parse input string as JSON: {err:?}"),
            )
        })?;

        let json = serde_json_to_value(json, use_maps).map_err(|err| {
            ctx.throw(
                args_at.string,
                format!("Failed to parse input string as JSON: {err}"),
            )
        })?;

        Ok(Some(json))
    })
}

fn serde_json_to_value(value: Value, use_maps: bool) -> Result<RuntimeValue, String> {
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
                .map(|value| serde_json_to_value(value, use_maps))
                .collect::<Result<_, _>>()?,
        ))),

        Value::Object(object) => {
            let gc_cell = GcCell::new(
                object
                    .into_iter()
                    .map(|(key, value)| {
                        serde_json_to_value(value, use_maps).map(|value| (key, value))
                    })
                    .collect::<Result<_, _>>()?,
            );

            Ok(if use_maps {
                RuntimeValue::Map(gc_cell)
            } else {
                RuntimeValue::Struct(gc_cell)
            })
        }
    }
}
