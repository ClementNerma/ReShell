use nu_ansi_term::{Color, Style};
use reedline::{Highlighter as RlHighlighter, StyledText};
use regex::Regex;

use crate::logic::{
    nesting::{handle_nesting, NestingCharAction},
    syntax_highlighter::SyntaxHighlighter,
};

pub fn create_highlighter() -> Box<dyn RlHighlighter> {
    Box::new(Highlighter)
}

pub struct Highlighter;

impl RlHighlighter for Highlighter {
    fn highlight(&self, line: &str, _cursor: usize) -> StyledText {
        highlight(line)
    }
}

fn highlight(input: &str) -> StyledText {
    let mut h = SyntaxHighlighter::new(input);

    for (c, offset, nesting_action) in handle_nesting(input) {
        let style = match nesting_action {
            NestingCharAction::OpeningPrefix
            | NestingCharAction::Opening
            | NestingCharAction::Closing => Style::new().fg(match c {
                '"' => Color::Green,
                _ => Color::LightBlue,
            }),

            NestingCharAction::Unclosed => Style::new().fg(Color::Red),

            NestingCharAction::ClosingWithoutOpening => {
                Style::new().fg(Color::Red).on(Color::White)
            }

            NestingCharAction::Escaping => Style::new().fg(Color::Cyan),

            NestingCharAction::InvalidEscape => Style::new().fg(Color::Red),
        };

        h.range(offset, c.len_utf8(), style);
    }

    macro_rules! highlight {
            ($(($category: expr) => $regex: expr => $color: ident),+) => {
                $(
                    // $category
                    h.regex(
                        Regex::new($regex).unwrap(),
                        &[Style::new().fg(Color::$color)],
                    );
                )+
            }
        }

    highlight!(
        ("comments") => "(#.*)$" => DarkGray,
        ("flags") => "\\s(\\-[\\-a-zA-Z0-9_]*)" => Yellow,
        ("keywords") => "(?:^|\\n|;|\\{)\\s*(let|mut|if|else|for|in|while|switch|case|continue|break|fn|return|throw|alias|type|do|try|catch)\\b" => Magenta,
        ("in") => "(?:\\bfor\\s+[a-zA-Z_][a-zA-Z0-9_]*\\s+)(in)\\b" => Magenta,
        ("types") => "\\b(any|bool|int|float|string|list|map|error|struct|fn)\\b" => Magenta,
        ("booleans") => "\\b(true|false)\\b" => LightYellow,
        ("null value") => "\\b(null)\\b" => LightYellow,
        ("variables") => "(\\$[a-zA-Z_][a-zA-Z0-9_]*)\\b" => Red,
        ("variables declaration") => "\\blet\\s+(?:mut\\s+)?([a-zA-Z_][a-zA-Z0-9_]*)\\b" => Red,
        ("loop iterator") => "(?:^|\\n|\\s)for\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\b" => Red,
        ("function arguments and struct fields") => "(?:\\b|[^\\|]\\|\\s*|,\\s*)([a-zA-Z_][a-zA-Z0-9_]*)\\s*(?:[:,]|\\|[^\\|])" => Red,
        ("untyped function arguments (1)") => "[^\\$]\\(\\s*([a-zA-Z_][a-zA-Z0-9_]*)" => Red,
        ("untyped function arguments (2)") => "([a-zA-Z_][a-zA-Z0-9_]*)\\s*[\\),]" => Red,
        ("numbers") => "\\b(\\d+(?:\\.\\d+)?)\\b" => LightYellow,
        ("raw command marker") => "(?:^|[\\{;]|\\$\\()\\s*(@raw)\\b" => Magenta,
        ("command") => "(?:^|[\\{;]|\\$\\(|@raw\\s+)\\s*([^\\(\\)\\[\\]\\{\\}<>=;\\!\\?&\\|'\"\\$]+)\\b" => LightBlue,
        ("function calls") => "\\b([a-zA-Z_][a-zA-Z0-9_]*)\\(" => LightBlue,
        ("command expression opening") => "\\s(\\$)\\(" => Red,
        ("symbols and operators") => "([&\\|,;=!<>\\?\\+\\-\\*\\/:]+)" => Blue,
        ("raw arguments") => "(.)" => Green
    );

    h.finalize(Style::default())
}
