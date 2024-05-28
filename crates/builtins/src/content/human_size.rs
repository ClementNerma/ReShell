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

        let precision = precision.unwrap_or(2);

        let max_prec = 10_u128.pow(u32::from(precision));

        let div = u128::from(size) * max_prec / u128::from(unit_base);

        let mut int_part = div / max_prec;
        let mut frac_part = div % max_prec;

        let last_digit = (u128::from(size) * max_prec * 10 / u128::from(unit_base)) % 10;

        if last_digit > 5 {
            if precision == 0 {
                int_part += 1;
                frac_part = 0;
            } else {
                frac_part += 1;

                if frac_part >= max_prec {
                    int_part += 1;
                    frac_part = 0;
                }
            }
        }

        Ok(Some(RuntimeValue::String(format!(
            "{int_part}{} {unit}",
            if frac_part > 0 && precision > 0 {
                format!(
                    ".{:#0precision$}",
                    frac_part,
                    precision = usize::from(precision)
                )
            } else {
                String::new()
            }
        ))))
    })
}
