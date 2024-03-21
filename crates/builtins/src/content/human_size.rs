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

    -> None
);

fn run() -> Runner {
    Runner::new(|_, Args { size, precision }, _, _| {
        let units = ["B", "KiB", "MiB", "GiB", "TiB"];

        let (unit, unit_base) = units
            .iter()
            .enumerate()
            .rev()
            .find_map(|(i, unit)| {
                let base = 1024_u64.pow(i.try_into().unwrap());

                if size >= base {
                    Some((unit, base))
                } else {
                    None
                }
            })
            .unwrap();

        Ok(Some(RuntimeValue::String(format!(
            "{} {unit}",
            approx_int_div::approx_int_div(size, unit_base, precision.unwrap_or(2))
        ))))
    })
}
