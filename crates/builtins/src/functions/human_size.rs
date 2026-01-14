use super::approx_int_div;

crate::define_internal_fn!(
    //
    // Display a message
    //

    "humanSize",

    (
        size: RequiredArg<ExactIntType<u64>> = Arg::positional("size"),
        precision: OptionalArg<ExactIntType<u8>> = Arg::long_and_short_flag("precision", 'p')
    )

    -> StringType
);

fn run() -> Runner {
    Runner::new(|at, Args { size, precision }, _, ctx| {
        let str = human_size(size, precision).map_err(|err| ctx.throw(at, err))?;
        Ok(Some(RuntimeValue::String(str)))
    })
}

pub fn human_size(size: u64, precision: Option<u8>) -> Result<String, &'static str> {
    let units = ["B", "KiB", "MiB", "GiB", "TiB"];

    let (unit, unit_base) = units
        .iter()
        .enumerate()
        .rev()
        .find_map(|(i, unit)| {
            let base = 1024_u64.pow(i.try_into().unwrap());

            if size >= base || base == 1 {
                Some((unit, base))
            } else {
                None
            }
        })
        .unwrap();

    let str = approx_int_div::approx_int_div(size, unit_base, precision.unwrap_or(2))?;

    Ok(format!("{str} {unit}"))
}
