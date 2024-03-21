use std::collections::HashMap;

use nu_ansi_term::{Color, Style};
use reedline::{Highlighter as RlHighlighter, StyledText};
use regex::Regex;

use crate::syntax::{HighlightPiece, Highlighter as SyntaxHighlighter, Rule, RuleSet, SimpleRule};

macro_rules! rule {
    (@match $matches: expr => [$($style: ident),+]) => {{
        Rule::Simple(SimpleRule {
            matches: Regex::new($matches).unwrap(),
            style: vec![$(Style::new().fg(Color::$style)),+],
        })
    }};

    (@match $matches: expr => $style: ident) => {{
        Rule::Simple(SimpleRule {
            matches: Regex::new($matches).unwrap(),
            style: vec![Style::new().fg(Color::$style)],
        })
    }};

    (@nested $begin: expr => $beginStyle: ident, $end: expr => $endStyle: ident, $innerRules: expr) => {
        Rule::Nested(NestingRule {
            begin: rule!(@match $begin => $beginStyle),
            end: rule!(@match $end => $endStyle),
            inner_rules: $innerRules
        })
    };

    (@group $name: expr) => {
        Rule::Group($name.to_owned())
    }
}

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
    // TODO: lazify this
    let rule_set = RuleSet {
        groups: [
            ("instructions", vec![
                // Comments
                rule!(@match "(#.*)$" => DarkGray),
                // Keywords
                rule!(@match "(?:^|\\n|;|\\{)\\s*(let|for|if|else|while|switch|case|continue|break|fn|return|throw|alias|type|do|try|catch)\\b" => Magenta),
                // Variable declaration
                rule!(@match "(?:\\blet\\s+)(mut\\s+)([a-zA-Z_][a-zA-Z0-9_]+)\\b" => [Magenta, Red]),
                // Constant declaration
                rule!(@match "(?:\\blet\\s+)([a-zA-Z_][a-zA-Z0-9_]+)\\b" => Red),
                // Loop
                rule!(@match "\\b(for)\\s+([a-zA-Z_][a-zA-Z0-9_]+)\\s+(in)\\s+" => [Magenta, Red, Magenta]),
                // [Pending] Loop
                rule!(@match "\\b(for)\\s+([a-zA-Z_][a-zA-Z0-9_]+)\\b" => [Magenta, Red]),
            ]),
            ("in-expressions", vec![
                // Types
                rule!(@match "\\b(any|bool|int|float|string|list|map|error|struct|fn)\\b" => Magenta),
                // Type aliases
                rule!(@match "\\b([A-Z][a-zA-Z0-9_]*)\\b" => LightYellow),
                // Booleans
                rule!(@match "\\b(true|false)\\b" => LightYellow),
                // The null value
                rule!(@match "\\b(null)\\b" => LightYellow),
                // Variables
                rule!(@match "(\\$[a-zA-Z_][a-zA-Z0-9_]*)\\b" => Red),
                // Flags
                rule!(@match "\\s(\\-[\\-a-zA-Z0-9_]*)" => LightYellow),
                // Number
                rule!(@match "\\b(\\d+(?:\\.\\d+)?)\\b" => LightYellow),
                // Symbols and operators
                rule!(@match "([&\\|,;=!<>\\?\\+\\-\\*\\/:]+)" => LightYellow),
            ],
        )]
        .into_iter().map(|(group, rules)| (group.to_owned(), rules)).collect(),
        rules: vec![rule!(@group "instructions")],
    };

    let pieces = SyntaxHighlighter::new(rule_set).highlight(input);

    let mut out = StyledText::new();

    for piece in pieces {
        let HighlightPiece { start, len, style } = piece;

        out.push((
            style.unwrap_or_else(Style::new),
            input[start..start + len].to_owned(),
        ));
    }

    out
}
