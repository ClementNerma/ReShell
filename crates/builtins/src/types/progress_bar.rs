use std::ops::Deref;

use colored::Color;
use indicatif::ProgressBar;
use reshell_runtime::values::CustomValueType;
use reshell_shared::pretty::{PrettyPrintable, PrettyPrintablePiece};

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
