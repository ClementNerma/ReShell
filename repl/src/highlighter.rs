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

        macro_rules! highlight {
            ($(($category: expr) => $regex: expr => $color: ident),+) => {
                $(
                    // $category
                    h.highlight(
                        Regex::new($regex).unwrap(),
                        &[Style::new().fg(Color::$color)],
                    );
                )+
            }
        }

        highlight!(
            ("keywords") => "\\b(let|mut|if|for|in|while|continue|break|fn|return|throw|alias|type|do)\\b" => Magenta,
            ("types") => "\\b(any|null|bool|int|float|string|list|map|error|struct|fn)\\b" => Magenta,
            ("variables") => "(\\$[a-zA-Z_][a-zA-Z0-9_]*)\\b" => Red,
            ("loop iterator") => "(?:^|\\n|\\s)(?:for|let|mut)\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\b" => Red,
            ("numbers") => "\\b(\\d+(?:\\.\\d+)?)\\b" => LightYellow,
            ("command") => "(?:^|[\\(\\{;])\\s*([a-zA-Z0-9_/\\.]+)\\b" => LightBlue,
            ("function calls") => "\\b([a-zA-Z_][a-zA-Z0-9_]*)\\(" => LightBlue,
            ("symbols") => "([\\(\\)\\{\\}\\[\\],;=!<>])" => DarkGray
        );

        h.finalize(Style::new().fg(Color::Green))
    }
}
