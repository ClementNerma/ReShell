use reshell_runtime::gc::GcCell;

use crate::{
    declare_typed_fn_handler,
    types::RegexValue,
    utils::{call_fn_checked, expect_returned_value},
};

crate::define_internal_fn!(
    "replace",

    (
        regex: RequiredArg<CustomType<RegexValue>> = Arg::method_self(),
        subject: RequiredArg<StringType> = Arg::positional("subject"),
        replacer: RequiredArg<ReplacerFn> = Arg::positional("replacer")
    )

    -> StringType
);

declare_typed_fn_handler!(ReplacerFn(matched: DetachedMapType<StringType>) -> StringType);

fn run() -> Runner {
    Runner::new(
        |_,
         Args {
             regex,
             subject,
             replacer,
         },
         args_at,
         ctx| {
            let replacer = LocatedValue::new(args_at.replacer, RuntimeValue::Function(replacer));

            let mut new = String::with_capacity(subject.len());

            let mut last_match = 0;

            for captured in regex.captures_iter(&subject) {
                let mut counter = 0;
                let mut capture_names = regex.capture_names();

                let map = captured
                    .iter()
                    .flatten()
                    .map(|matched| {
                        let name = match capture_names.next().unwrap() {
                            Some(name) => name.to_owned(),

                            None => {
                                counter += 1;
                                (counter - 1).to_string()
                            }
                        };

                        (name, RuntimeValue::String(matched.as_str().to_owned()))
                    })
                    .collect();

                let replacement = call_fn_checked(
                    &replacer,
                    &ReplacerFn::signature(),
                    vec![RuntimeValue::Map(GcCell::new(map))],
                    ctx,
                )
                .and_then(|ret| {
                    expect_returned_value::<_, StringType>(ret, args_at.replacer, ctx)
                })?;

                let m = captured.get(0).unwrap();
                new.push_str(&subject[last_match..m.start()]);
                new.push_str(&replacement);
                last_match = m.end();
            }

            new.push_str(&subject[last_match..]);
            new.shrink_to_fit();

            Ok(Some(RuntimeValue::String(new)))
        },
    )
}
