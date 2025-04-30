use std::sync::LazyLock;

use regex::{Captures, Regex};
use reshell_runtime::{
    gc::{GcCell, GcReadOnlyCell},
    values::RuntimeFnValue,
};

use crate::{
    declare_typed_fn_handler, declare_typed_union_handler,
    types::RegexValue,
    utils::{call_fn_checked, expect_returned_value},
};

crate::define_internal_fn!(
    "replace",

    (
        regex: RequiredArg<CustomType<RegexValue>> = Arg::method_self(),
        subject: RequiredArg<StringType> = Arg::positional("subject"),
        replacer: RequiredArg<ReplacerType> = Arg::positional("replacer")
    )

    -> StringType
);

declare_typed_union_handler!(ReplacerType => enum Replacer {
    Pattern(StringType),
    Function(ReplacerFn)
});

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
            let replaced = match replacer {
                // Replace using a string
                Replacer::Pattern(pattern) => {
                    replace_regex_with_pattern(&regex, &subject, &pattern, args_at.replacer, ctx)?
                }

                // Replace using a mapper function
                Replacer::Function(replacer_fn) => {
                    replace_regex_with_fn(&regex, &subject, replacer_fn, args_at.replacer, ctx)?
                }
            };

            Ok(Some(RuntimeValue::String(replaced)))
        },
    )
}

/// Replace all matches of a regex with a provided function
///
/// The callback takes the captured content of the regex and returns the replacement string
fn regex_replace(
    regex: &Regex,
    subject: &str,
    mut replace: impl FnMut(&Captures, &Regex) -> ExecResult<String>,
) -> ExecResult<String> {
    let mut new = String::with_capacity(subject.len());

    let mut last_match = 0;

    for captured in regex.captures_iter(subject) {
        let replacement = replace(&captured, regex)?;

        let m = captured.get(0).unwrap();
        new.push_str(&subject[last_match..m.start()]);
        new.push_str(&replacement);
        last_match = m.end();
    }

    new.push_str(&subject[last_match..]);
    new.shrink_to_fit();

    Ok(new)
}

/// Replace all matches of a regex with a pattern
///
/// The pattern may contain group names prefixed by a '$' symbol
fn replace_regex_with_pattern(
    regex: &Regex,
    subject: &str,
    pattern: &str,
    pattern_at: RuntimeCodeRange,
    ctx: &mut Context,
) -> ExecResult<String> {
    regex_replace(regex, subject, move |captured, _| {
        // Replace all occurences of '$<group name>' occurrences in the pattern
        // with the actual groups' content
        regex_replace(&CAPTURE_NAME_REGEX, pattern, |pattern_captured, _| {
            // Get the group name (which was prefixed by a '$')
            let group_name = pattern_captured.get(1).unwrap().as_str();

            // If it's a number...
            let captured = match group_name.parse::<usize>() {
                // Get the numbered capture group
                Ok(index) => captured.get(index),

                // Otherwise get the named capture group
                Err(_) => captured.name(group_name),
            };

            // If the group doesn't exist, return an error
            let Some(captured) = captured else {
                return Err(ctx.error(
                    pattern_at,
                    format!("No capture group named '{group_name}' in matched content"),
                ));
            };

            // Success!
            Ok(captured.as_str().to_owned())
        })
    })
}

static CAPTURE_NAME_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new("\\$(\\d+|\\{[a-zA-Z_]+})").unwrap());

/// Replace all matches of a regex with a shell function
fn replace_regex_with_fn(
    regex: &Regex,
    subject: &str,
    replacer_fn: GcReadOnlyCell<RuntimeFnValue>,
    replacer_at: RuntimeCodeRange,
    ctx: &mut Context,
) -> ExecResult<String> {
    let replacer_fn = LocatedValue::new(replacer_at, RuntimeValue::Function(replacer_fn));

    regex_replace(regex, subject, move |captured, regex| {
        // Build a map with the captured groups
        let map = captured
            .iter()
            .flatten()
            .map(|matched| {
                let mut counter = 0;
                let mut capture_names = regex.capture_names();

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

        // Call the shell function with the map
        let ret = call_fn_checked(
            &replacer_fn,
            &ReplacerFn::signature(),
            vec![RuntimeValue::Map(GcCell::new(map))],
            ctx,
        )?;

        // Ensure the return value is a string
        expect_returned_value::<_, StringType>(ret, replacer_at, ctx)
    })
}
