use nu_ansi_term::{Color, Style};
use once_cell::sync::Lazy;
use parsy::CodeRange;
use reedline::StyledText;

pub struct HighlightList<'a> {
    source: &'a str,
    rendered: StyledText,
    prev: usize,
}

impl<'a> HighlightList<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            rendered: StyledText::new(),
            prev: 0,
        }
    }

    pub fn into_rendered(mut self) -> StyledText {
        self.push_untreated_bit(self.source.as_bytes().len());
        self.rendered
    }
}

impl<'a> HighlightList<'a> {
    pub fn push(&mut self, style: &Style, at: CodeRange) {
        assert!(at.start.offset >= self.prev);

        let start = at.start.offset;
        let end = start + at.len;

        self.push_untreated_bit(start);

        self.rendered
            .push((style.clone(), self.source[start..end].to_string()));

        self.prev = start + at.len;
    }

    fn push_untreated_bit(&mut self, start: usize) {
        if start > self.prev {
            self.rendered
                .push((Style::default(), self.source[self.prev..start].to_string()));
        }
    }
}

pub static KEYWORD: Lazy<Style> = Lazy::new(|| Color::Cyan.into());
pub static COMMENT: Lazy<Style> = Lazy::new(|| Color::DarkGray.into());
pub static VAR_NAME: Lazy<Style> = Lazy::new(|| Color::LightPurple.into());
pub static ENV_VAR_NAME: Lazy<Style> = Lazy::new(|| Style::new().fg(Color::Blue).bold());
pub static FN_NAME: Lazy<Style> = Lazy::new(|| Style::new().fg(Color::LightPurple).bold());
pub static PROP_NAME: Lazy<Style> = Lazy::new(|| Color::LightPurple.into());
pub static FLAG: Lazy<Style> = Lazy::new(|| Style::new().fg(Color::LightMagenta).bold());
pub static TYPE_NAME: Lazy<Style> = Lazy::new(|| Style::new().fg(Color::LightYellow).bold());
pub static BOOL: Lazy<Style> = Lazy::new(|| Color::Yellow.into());
pub static INT: Lazy<Style> = Lazy::new(|| Color::Green.into());
pub static FLOAT: Lazy<Style> = Lazy::new(|| Color::Green.into());
pub static STRING: Lazy<Style> = Lazy::new(|| Color::LightGreen.into());
pub static ESCAPED_CHAR: Lazy<Style> = Lazy::new(|| Color::Cyan.into());
pub static RAW_STR: Lazy<Style> = Lazy::new(|| Style::new());
pub static PATH: Lazy<Style> = Lazy::new(|| Color::Blue.into());
