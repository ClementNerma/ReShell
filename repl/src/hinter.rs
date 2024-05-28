use nu_ansi_term::{Color, Style};
use reedline::{DefaultHinter, Hinter};

pub fn create_hinter() -> Box<dyn Hinter> {
    Box::new(DefaultHinter::default().with_style(Style::new().fg(Color::DarkGray)))
}
