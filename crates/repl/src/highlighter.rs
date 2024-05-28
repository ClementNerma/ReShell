use nu_ansi_term::{Color, Style};
use reedline::{Highlighter as RlHighlighter, StyledText};
use regex::Regex;

use crate::logic::{
    nesting::{detect_nesting_actions, NestingAction, NestingActionType},
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

    let nesting = detect_nesting_actions(input);

    highlight_inner(input, &nesting, &mut h);

    macro_rules! highlight {
        ($($regex: expr => $color: ident),+) => {
            $(
                h.regex(
                    Regex::new($regex).unwrap(),
                    &[Style::new().fg(Color::$color)],
                );
            )+
        }
    }

    highlight!(
        // comments
        "(#.*)$" => DarkGray,

        // flags
        "\\s(\\-[\\-a-zA-Z0-9_]*)" => Yellow,

        // keywords
        "(?:^|\\n|;|\\{)\\s*(let|if|else|for|in|while|switch|case|continue|break|fn|return|throw|alias|type|do|try|catch)\\b" => Magenta,

        // mut
        "(?:\\blet\\s+)(mut)\\b" => Magenta,

        // in
        "(?:\\bfor\\s+[a-zA-Z_][a-zA-Z0-9_]*\\s+)(in)\\b" => Magenta,

        // types
        "\\b(any|bool|int|float|string|list|map|error|struct|fn)\\b" => Magenta,

        // custom types
        "\\b([A-Z][a-zA-Z0-9_]*)\\b" => LightYellow,

        // booleans
        "\\b(true|false)\\b" => LightYellow,

        // null value
        "\\b(null)\\b" => LightYellow,

        // variables
        "(\\$[a-zA-Z_][a-zA-Z0-9_]*)\\b" => Red,

        // variables declaration
        "\\blet\\s+(?:mut\\s+)?([a-zA-Z_][a-zA-Z0-9_]*)\\b" => Red,

        // loop iterator
        "(?:^|\\n|\\s)for\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\b" => Red,

        // function arguments and struct fields
        "(?:\\b|[^\\|]\\|\\s*|,\\s*)([a-zA-Z_][a-zA-Z0-9_]*)\\s*(?:[:,]|\\|[^\\|])" => Red,

        // untyped function arguments (1)
        "[^\\$]\\(\\s*([a-zA-Z_][a-zA-Z0-9_]*)" => Red,

        // untyped function arguments (2)
        "(?:,|[a-zA-Z0-9_]\\s*\\()\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*[\\),]" => Red,

        // numbers
        "\\b(\\d+(?:\\.\\d+)?)\\b" => LightYellow,

        // command markers
        "(?:^|[\\{;]|\\$\\()\\s*(@raw|@var)\\b" => Magenta,

        // command call
        "(?:^|[\\{;]|\\$\\(|@raw|@var\\s+)\\s*([^\\(\\)\\[\\]\\{\\}<>=;\\!\\?&\\|'\"\\$\\s]+)(?:[\\s\\)]|\\n|$)" => LightBlue,

        // function call
        "\\b([a-zA-Z_][a-zA-Z0-9_]*)\\(" => LightBlue,

        // symbols and operators
        "([&\\|,;=!<>\\?\\+\\-\\*\\/:]+)" => Blue

        // // raw arguments
        // "(.)" => Green
    );

    h.finalize(Style::default())
}

fn highlight_inner(input: &str, nesting_actions: &[NestingAction], h: &mut SyntaxHighlighter) {
    // if let Some(begin) = nesting_actions.iter().find(|a| {
    //     matches!(a.action_type, NestingActionType::Opening)
    //         && &input[a.offset..a.offset + a.len] == "$("
    // }) {
    //     todo!()
    // }

    for action in nesting_actions {
        let NestingAction {
            offset,
            len,
            action_type,
        } = action;

        let style = match action_type {
            NestingActionType::Opening | NestingActionType::Closing { opening_offset: _ } => {
                Style::new().fg(if &input[*offset..*offset + len] == "\"" {
                    Color::Green
                } else {
                    Color::LightBlue
                })
            }

            NestingActionType::Unclosed => Style::new().fg(Color::Red),

            NestingActionType::ClosingWithoutOpening => {
                Style::new().fg(Color::Red).on(Color::White)
            }

            NestingActionType::Escaping => Style::new().fg(Color::Cyan),

            NestingActionType::InvalidEscape => Style::new().fg(Color::Red),

            NestingActionType::StringPiece => Style::new().fg(Color::Green),
        };

        h.range(*offset, *len, style);
    }
}
