use std::{ops::Deref, sync::Arc};

use colored::Color;
use regex::Regex;
use reshell_runtime::{pretty_impl::pretty_printable_string, values::CustomValueType};
use reshell_shared::pretty::{PrettyPrintable, PrettyPrintablePiece};

/// Regular expression
///
/// Backed with a thread-shared [`Regex`]
#[derive(Debug, Clone)]
pub struct RegexValue(pub Arc<Regex>);

impl Deref for RegexValue {
    type Target = Regex;

    fn deref(&self) -> &Self::Target {
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
