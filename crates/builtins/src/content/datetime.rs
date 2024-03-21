use time::{
    format_description,
    util::local_offset::{set_soundness, Soundness},
    OffsetDateTime, UtcOffset,
};

use crate::define_internal_fn;

define_internal_fn!(
    "datetime",

    (
        format: RequiredArg<StringType> = Arg::positional("format")
    )

    -> Some(StringType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|at, Args { format }, ArgsAt { format: format_at }, ctx| {
        let offset = get_utc_offset();

        let now = OffsetDateTime::now_utc().to_offset(offset);

        let format = format_description::parse(&format).map_err(|err| {
            ctx.error(
                format_at,
                format!("Failed to parse date/time formatting: {err}"),
            )
        })?;

        let formatted = now
            .format(&format)
            .map_err(|err| ctx.error(at, format!("Failed to format date/time: {err}")))?;

        Ok(Some(RuntimeValue::String(formatted)))
    })
}

// TODO: comment on this unsafe portion (and the fact that ::set_env_var is "unsafe" anyway)
pub fn get_utc_offset() -> UtcOffset {
    unsafe {
        set_soundness(Soundness::Unsound);
    }

    // TODO: explain why we can unwrap here
    let offset = UtcOffset::current_local_offset().unwrap();

    unsafe {
        set_soundness(Soundness::Sound);
    }

    offset
}
