use jiff::{
    civil::{Date, Time},
    tz::{Offset, TimeZone},
};
use reshell_runtime::gc::{GcCell, GcReadOnlyCell};
use toml::{
    value::{Date as TomlDate, Datetime as TomlDatetime, Offset as TomlOffset, Time as TomlTime},
    Table, Value,
};

use crate::{define_internal_fn, functions::DateTimeValue};

define_internal_fn!(
    "parseToml",

    (
        string: RequiredArg<StringType> = Arg::method_self(),
        use_maps: PresenceFlag = Arg::long_and_short_flag("use-maps", 'm')
    )

    -> Some(AnyType::value_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { string, use_maps }, args_at, ctx| {
        let toml = string.parse::<Table>().map_err(|err| {
            ctx.throw(
                args_at.string,
                format!("Failed to parse input string as JSON: {err:?}"),
            )
        })?;

        let json = toml_to_value(Value::Table(toml), use_maps).map_err(|err| {
            ctx.throw(
                args_at.string,
                format!("Failed to parse input string as JSON: {err}",),
            )
        })?;

        Ok(Some(json))
    })
}

fn toml_to_value(value: Value, use_maps: bool) -> Result<RuntimeValue, String> {
    match value {
        Value::Boolean(bool) => Ok(RuntimeValue::Bool(bool)),

        Value::Integer(int) => Ok(RuntimeValue::Int(int)),

        Value::Float(float) => Ok(RuntimeValue::Float(float)),

        Value::String(string) => Ok(RuntimeValue::String(string)),

        Value::Datetime(datetime) => {
            let TomlDatetime { date, time, offset } = datetime;

            let (date_time, timezone) = match (date, time, offset) {
                (Some(date), Some(time), offset) => {
                    let date = try_convert_date(date)?;
                    let time = try_convert_time(time)?;

                    let date_time = date.to_datetime(time);

                    let timezone = offset.map(|offset| -> Result<TimeZone, String> {
                        match offset {
                            TomlOffset::Z => Ok(TimeZone::UTC),
                            TomlOffset::Custom { minutes } => {
                                let offset = Offset::from_seconds(i32::from(minutes) * 60)
                                    .map_err(|err| {
                                        format!("Invalid offset of '{minutes}' minutes: {err}")
                                    })?;

                                Ok(TimeZone::fixed(offset))
                            }
                        }
                    });

                    (date_time, timezone.transpose()?)
                }

                (Some(date), None, None) => {
                    (try_convert_date(date)?.to_datetime(Time::midnight()), None)
                }

                (None, Some(time), None) => (try_convert_time(time)?.to_datetime(Date::ZERO), None),

                (_, _, Some(_)) | (None, None, None) => unreachable!(),
            };

            let date_time = date_time
                .to_zoned(timezone.unwrap_or(TimeZone::UTC))
                .map_err(|err| format!("Invalid date time {datetime}: {err}"))?;

            Ok(RuntimeValue::Custom(GcReadOnlyCell::new(Box::new(
                DateTimeValue::new(date_time),
            ))))
        }

        Value::Array(array) => Ok(RuntimeValue::List(GcCell::new(
            array
                .into_iter()
                .map(|value| toml_to_value(value, use_maps))
                .collect::<Result<_, _>>()?,
        ))),

        Value::Table(object) => {
            let gc_cell = GcCell::new(
                object
                    .into_iter()
                    .map(|(key, value)| toml_to_value(value, use_maps).map(|value| (key, value)))
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

fn try_convert_date(date: TomlDate) -> Result<Date, String> {
    let TomlDate { year, month, day } = date;

    let year = i16::try_from(year).map_err(|_| format!("Invalid year '{year}'"))?;
    let month = i8::try_from(month).map_err(|_| format!("Invalid month '{month}'"))?;
    let day = i8::try_from(day).map_err(|_| format!("Invalid day '{day}'"))?;

    let converted = Date::new(year, month, day);

    converted.map_err(|err| format!("Invalid date '{date}': {err}"))
}

fn try_convert_time(time: TomlTime) -> Result<Time, String> {
    let TomlTime {
        hour,
        minute,
        second,
        nanosecond,
    } = time;

    let hour = i8::try_from(hour).map_err(|_| format!("Invalid hour '{hour}'"))?;
    let minute = i8::try_from(minute).map_err(|_| format!("Invalid minute '{minute}'"))?;
    let second = i8::try_from(second).map_err(|_| format!("Invalid second '{second}'"))?;

    let subsec_nanosecond = i32::try_from(nanosecond)
        .map_err(|_| format!("Invalid nanoseconds count '{nanosecond}'"))?;

    let converted = Time::new(hour, minute, second, subsec_nanosecond);

    converted.map_err(|err| format!("Invalid time '{time}': {err}"))
}
