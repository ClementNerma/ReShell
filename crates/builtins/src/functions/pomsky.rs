use std::sync::Arc;

use colored::Colorize;
use pomsky::{Expr, diagnose::Severity, options::CompileOptions};
use regex::Regex;
use reshell_prettify::PrettyPrintable;
use reshell_runtime::pretty_impl::pretty_printable_runtime_input_range;

use crate::define_internal_fn;

define_internal_fn!(
    "pomsky",

    (
        pattern: RequiredArg<StringType> = Arg::positional("pattern")
    )

    -> RegexType
);

fn run() -> Runner {
    Runner::new(|_, Args { pattern }, args_at, ctx| {
        let (parsed, diag, _) = Expr::parse_and_compile(&pattern, CompileOptions::default());

        for diag in diag {
            println!(
                "{} for regex at {}: {}",
                match diag.severity {
                    Severity::Warning => "WARNING".bright_yellow(),
                    Severity::Error => "ERROR".bright_red(),
                },
                pretty_printable_runtime_input_range(args_at.pattern, ctx.files_map())
                    .display_inline(),
                diag.msg
            );
        }

        let regex = match parsed {
            Some(parsed) => Regex::new(&parsed).unwrap(),
            None => {
                return Err(ctx.throw(args_at.pattern, "failed to parse Pomsky regex"));
            }
        };

        Ok(Some(RuntimeValue::Regex(Arc::new(regex))))
    })
}
