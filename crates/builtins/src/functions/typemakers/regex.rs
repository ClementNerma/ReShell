use std::rc::Rc;

use colored::{Color, Colorize};
use pomsky::{diagnose::Severity, options::CompileOptions, Expr};
use regex::Regex;
use reshell_runtime::{
    display::{dbg_loc, pretty_print_string},
    gc::GcReadOnlyCell,
    pretty::{PrettyPrintable, PrettyPrintablePiece},
    values::CustomValueType,
};

use crate::define_internal_fn;

define_internal_fn!(
    "regex",

    (
        pattern: RequiredArg<StringType> = Arg::positional("pattern"),
        pomsky: PresenceFlag = Arg::long_and_short_flag("pomsky", 'p')
    )

    -> Some(CustomType::<RegexValue>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(
        |_,
         Args { pattern, pomsky },
         ArgsAt {
             pattern: pattern_at,
             ..
         },
         ctx| {
            let regex = if !pomsky {
                Regex::new(&pattern).map_err(|err| ctx.throw(pattern_at, format!("{err}")))?
            } else {
                let (parsed, diag, _) =
                    Expr::parse_and_compile(&pattern, CompileOptions::default());

                for diag in diag {
                    println!(
                        "{} for regex at {}: {}",
                        match diag.severity {
                            Severity::Warning => "WARNING".bright_yellow(),
                            Severity::Error => "ERROR".bright_red(),
                        },
                        dbg_loc(pattern_at, ctx.files_map()),
                        diag.msg
                    );
                }

                match parsed {
                    Some(parsed) => Regex::new(&parsed).unwrap(),
                    None => {
                        return Err(ctx.error(pattern_at, "failed to parse Pomsky regex"));
                    }
                }
            };

            Ok(Some(RuntimeValue::Custom(GcReadOnlyCell::new(Box::new(
                RegexValue {
                    inner: Rc::new(regex),
                },
            )))))
        },
    )
}

#[derive(Debug, Clone)]
pub struct RegexValue {
    inner: Rc<Regex>,
}

impl RegexValue {
    pub fn inner(&self) -> &Regex {
        &self.inner
    }
}

impl CustomValueType for RegexValue {
    fn typename(&self) -> &'static str {
        "regex"
    }

    fn typename_static() -> &'static str
    where
        Self: Sized,
    {
        "regex"
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