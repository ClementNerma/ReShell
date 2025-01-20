use std::sync::Arc;

use colored::Colorize;
use pomsky::{diagnose::Severity, options::CompileOptions, Expr};
use regex::Regex;
use reshell_runtime::gc::GcReadOnlyCell;
use reshell_shared::pretty::{PrettyPrintOptions, PrettyPrintable};

use crate::define_internal_fn;

use super::RegexValue;

define_internal_fn!(
    "pomsky",

    (
        pattern: RequiredArg<StringType> = Arg::positional("pattern")
    )

    -> CustomType<RegexValue>
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
                args_at
                    .pattern
                    .render_colored(ctx.files_map(), PrettyPrintOptions::inline()),
                diag.msg
            );
        }

        let regex = match parsed {
            Some(parsed) => Regex::new(&parsed).unwrap(),
            None => {
                return Err(ctx.throw(args_at.pattern, "failed to parse Pomsky regex"));
            }
        };

        let value =
            RuntimeValue::Custom(GcReadOnlyCell::new(Box::new(RegexValue(Arc::new(regex)))));

        Ok(Some(value))
    })
}
