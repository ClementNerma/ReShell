use std::cmp::min;

use nu_ansi_term::{Color, Style};
use once_cell::sync::Lazy;
use parsy::{CodeRange, Location};
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
        self.push_untreated_bit(Style::default(), self.source.as_bytes().len());
        self.rendered
    }
}

impl<'a> HighlightList<'a> {
    pub fn push(&mut self, style: &Style, at: CodeRange) {
        assert!(at.start.offset >= self.prev);

        let start = at.start.offset;
        let end = start + at.len;

        self.push_untreated_bit(Style::default(), start);

        self.rendered
            .push((*style, self.source[start..end].to_string()));

        self.prev += at.len;
    }

    pub fn push_until(&mut self, style: &Style, at: CodeRange, until: CodeRange) {
        assert_eq!(at.start.file_id, until.start.file_id);
        assert!(until.start.offset >= at.start.offset);

        self.push(
            style,
            CodeRange {
                start: at.start,
                len: min(until.start.offset - at.start.offset, at.len),
            },
        );
    }

    pub fn push_remaining(&mut self, style: &Style, at: CodeRange) {
        assert!(at.start.offset <= self.prev);

        self.push(
            style,
            CodeRange {
                start: Location {
                    file_id: at.start.file_id,
                    offset: self.prev,
                },
                len: at.len - (self.prev - at.start.offset),
            },
        );
    }

    pub fn push_everything_until(&mut self, style: &Style, until: CodeRange) {
        self.push_untreated_bit(*style, until.start.offset);
    }

    fn push_untreated_bit(&mut self, style: Style, until: usize) {
        assert!(until >= self.prev);

        if until > self.prev {
            self.rendered
                .push((style, self.source[self.prev..until].to_string()));
        }

        self.prev = until;
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
pub static INT: Lazy<Style> = Lazy::new(|| Color::Yellow.into());
pub static FLOAT: Lazy<Style> = Lazy::new(|| Color::Yellow.into());
pub static STRING: Lazy<Style> = Lazy::new(|| Color::LightGreen.into());
pub static ESCAPED_CHAR: Lazy<Style> = Lazy::new(|| Color::LightGreen.into());
pub static RAW_STR: Lazy<Style> = Lazy::new(|| Color::LightGreen.into());
pub static PATH: Lazy<Style> = Lazy::new(|| Color::Blue.into());
