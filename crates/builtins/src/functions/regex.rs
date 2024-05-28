use colored::{Color, Colorize};
use pomsky::{diagnose::Severity, options::CompileOptions, Expr};
use regex::Regex;
use reshell_runtime::{
    display::{dbg_loc, pretty_print_string},
    errors::ExecInfoType,
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

    -> None // TODO: new type handler for custom
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
                Regex::new(&pattern)
                    .map_err(|err| ctx.throw(pattern_at, format!("Failed to parse regex: {err}")))?
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
                        return Err(ctx
                            .error(pattern_at, "failed to parse Pomsky regex")
                            .with_info(
                                ExecInfoType::Tip,
                                // TODO: try to parse the regex as Pomsky to make the tip more precise
                                "if you wrote a Pomsky regex, use the '--pomsky' flag",
                            ));
                    }
                }
            };

            Ok(Some(RuntimeValue::Custom(GcReadOnlyCell::new(Box::new(
                RegexValue::new(regex),
            )))))
        },
    )
}

#[derive(Debug, Clone)]
pub struct RegexValue {
    inner: Regex, // TODO: Rc
}

impl RegexValue {
    fn new(inner: Regex) -> Self {
        Self { inner }
    }

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
