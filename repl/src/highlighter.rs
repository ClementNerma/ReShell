use nu_ansi_term::{Color, Style};
use reedline::{Highlighter as RlHighlighter, StyledText};
use regex::Regex;

use crate::highlighting::SyntaxHighlighter;

pub fn create_highlighter() -> Box<dyn RlHighlighter> {
    Box::new(Highlighter)
}

pub struct Highlighter;

impl RlHighlighter for Highlighter {
    fn highlight(&self, line: &str, _cursor: usize) -> StyledText {
        let mut h = SyntaxHighlighter::new(line);

        h.highlight(
            Regex::new(
                "\\b(let|mut|if|for|in|while|continue|break|fn|return|throw|alias|type|do)\\b",
            )
            .unwrap(),
            &[Style::new().fg(Color::Magenta).bold()],
        );

        h.highlight(
            Regex::new("\\b(any|null|bool|int|float|string|list|map|error|struct|fn)\\b").unwrap(),
            &[Style::new().fg(Color::Magenta).bold()],
        );

        h.highlight(
            Regex::new("(\\$[a-zA-Z_][a-zA-Z0-9_]*)\\b").unwrap(),
            &[Style::new().fg(Color::Red)],
        );

        h.highlight(
            Regex::new("(?:^|\\n|\\s)(?:for|let|mut)\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\b").unwrap(),
            &[Style::new().fg(Color::Red)],
        );

        h.highlight(
            Regex::new("\\b(\\d+(?:\\.\\d+)?)\\b").unwrap(),
            &[Style::new().fg(Color::LightYellow)],
        );

        h.highlight(
            Regex::new("(?:^|[\\(\\{;])\\s*([a-zA-Z0-9_/\\.]+)\\b").unwrap(),
            &[Style::new().fg(Color::LightBlue)],
        );

        h.highlight(
            Regex::new("\\b([a-zA-Z_][a-zA-Z0-9_]*)\\(").unwrap(),
            &[Style::new().fg(Color::LightBlue)],
        );

        // h.highlight(
        //     Regex::new("\\b([a-zA-Z_][a-zA-Z0-9_]*)\\b").unwrap(),
        //     &[Style::new().fg(Color::Green)],
        // );

        h.finalize(Style::new().fg(Color::Green))
    }
}
