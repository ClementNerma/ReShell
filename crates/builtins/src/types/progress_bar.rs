use std::{cmp::Ordering, ops::Deref};

use colored::Color;
use indicatif::ProgressBar;
use reshell_prettify::{PrettyPrintable, PrettyPrintablePiece};
use reshell_runtime::values::CustomValueType;

/// Progress bar displayer
#[derive(Debug, Clone)]
pub struct ProgressBarValue(pub ProgressBar);

impl Deref for ProgressBarValue {
    type Target = ProgressBar;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl CustomValueType for ProgressBarValue {
    fn typename(&self) -> &'static str {
        "progressbar"
    }

    fn typename_static() -> &'static str
    where
        Self: Sized,
    {
        "progressbar"
    }

    fn supports_ord(&self) -> bool {
        false
    }

    fn ord(&self, _: &dyn CustomValueType) -> Ordering {
        unreachable!()
    }

    fn supports_eq(&self) -> bool {
        false
    }

    fn eq(&self, _: &dyn CustomValueType) -> bool {
        unreachable!()
    }
}

impl PrettyPrintable for ProgressBarValue {
    type Context = ();

    fn generate_pretty_data(&self, _: &()) -> PrettyPrintablePiece {
        PrettyPrintablePiece::Join(vec![
            PrettyPrintablePiece::colored_atomic("progressBar(", Color::Magenta),
            PrettyPrintablePiece::colored_atomic("<internal>", Color::BrightBlack),
            PrettyPrintablePiece::colored_atomic(")", Color::Magenta),
        ])
    }
}
