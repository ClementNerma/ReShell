use std::{ops::Deref, sync::Arc};

use colored::{Color, Colorize};
use pomsky::{diagnose::Severity, options::CompileOptions, Expr};
use regex::Regex;
use reshell_runtime::{
    gc::GcReadOnlyCell, pretty_impl::pretty_printable_string, values::CustomValueType,
};
use reshell_shared::pretty::{PrettyPrintOptions, PrettyPrintable, PrettyPrintablePiece};

use crate::define_internal_fn;

define_internal_fn!(
    "regex",

    (
        pattern: RequiredArg<StringType> = Arg::positional("pattern"),
        pomsky: PresenceFlag = Arg::long_and_short_flag("pomsky", 'p')
    )

    -> Some(CustomType::<RegexValue>::value_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { pattern, pomsky }, args_at, ctx| {
        let regex = if !pomsky {
            Regex::new(&pattern).map_err(|err| ctx.throw(args_at.pattern, format!("{err}")))?
        } else {
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

            match parsed {
                Some(parsed) => Regex::new(&parsed).unwrap(),
                None => {
                    return Err(ctx.throw(args_at.pattern, "failed to parse Pomsky regex"));
                }
            }
        };

        Ok(Some(RuntimeValue::Custom(GcReadOnlyCell::new(Box::new(
            RegexValue(Arc::new(regex)),
        )))))
    })
}

/// Regular expression
///
/// Backed with a thread-shared [`Regex`]
#[derive(Debug, Clone)]
pub struct RegexValue(Arc<Regex>);

// impl RegexValue {
//     pub fn new(dur: Arc<Regex>) -> Self {
//         Self(dur)
//     }
// }

impl Deref for RegexValue {
    type Target = Regex;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl RegexValue {
    pub fn inner(&self) -> &Regex {
        &self.0
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
    type Context = ();

    fn generate_pretty_data(&self, _: &()) -> PrettyPrintablePiece {
        PrettyPrintablePiece::Join(vec![
            PrettyPrintablePiece::colored_atomic("regex(", Color::Magenta),
            pretty_printable_string(self.0.as_str()),
            PrettyPrintablePiece::colored_atomic(")", Color::Magenta),
        ])
    }
}
