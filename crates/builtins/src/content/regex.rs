use colored::{Color, Colorize};
use pomsky::{diagnose::Severity, options::CompileOptions, Expr};
use regex::Regex;
use reshell_runtime::{
    display::{dbg_loc, pretty_print_string},
    errors::ExecErrorInfoType,
    gc_cell,
    pretty::{PrettyPrintable, PrettyPrintablePiece},
    size::ComputableSize,
    values::CustomValueType,
};

use crate::define_internal_fn;

define_internal_fn!(
    "regex",

    (
        pattern: RequiredArg<StringType> = Arg::positional("pattern"),
        classic: PresenceFlag = Arg::long_and_short_flag("classic", 'c')
    )

    -> None // TODO: new type handler for custom
);

fn run() -> Runner {
    Runner::new(
        |_,
         Args { pattern, classic },
         ArgsAt {
             pattern: pattern_at,
             ..
         },
         ctx| {
            let regex = if classic {
                Regex::new(&pattern)
                    .map_err(|err| ctx.error(pattern_at, format!("Failed to parse regex: {err}")))?
            } else {
                let (parsed, diag, _) =
                    Expr::parse_and_compile(&pattern, CompileOptions::default());

                for diag in diag {
                    println!(
                        "{} for regex at {}: {}",
                        dbg_loc(pattern_at, ctx.files_map()),
                        match diag.severity {
                            Severity::Warning => "WARNING".bright_yellow(),
                            Severity::Error => "ERROR".bright_red(),
                        },
                        diag.msg
                    );
                }

                match parsed {
                    Some(parsed) => Regex::new(&parsed).unwrap(),
                    None => {
                        return Err(ctx
                            .error(pattern_at, "failed to parse Pomsky regex")
                            .with_info(
                                ExecErrorInfoType::Tip,
                                "if you wrote a PCRE regex, use the '--classic' flag",
                            ))
                    }
                }
            };

            Ok(Some(RuntimeValue::Custom(
                gc_cell!([readonly] RegexValue::new(regex)),
            )))
        },
    )
}

#[derive(Debug)]
pub struct RegexValue {
    inner: Regex,
}

impl RegexValue {
    fn new(inner: Regex) -> Self {
        Self { inner }
    }

    fn inner(&self) -> &Regex {
        &self.inner
    }
}

impl CustomValueType for RegexValue {
    fn typename(&self) -> &'static str {
        "regex"
    }
}

impl ComputableSize for RegexValue {
    fn compute_heap_size(&self) -> usize {
        // NOTE: Low estimate
        self.inner.as_str().len()
    }
}

impl PrettyPrintable for RegexValue {
    fn generate_pretty_data(&self, _: &Context) -> PrettyPrintablePiece {
        PrettyPrintablePiece::Join(vec![
            PrettyPrintablePiece::colored_atomic("regex(", Color::Magenta),
            pretty_print_string(self.inner.as_str()),
            PrettyPrintablePiece::colored_atomic(")", Color::Magenta),
        ])
    }
}
