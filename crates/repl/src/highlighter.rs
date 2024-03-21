use nu_ansi_term::{Color, Style};
use reedline::{Highlighter as RlHighlighter, StyledText};
use regex::Regex;

use crate::syntax::{
    HighlightPiece, Highlighter as SyntaxHighlighter, NestingRule, Rule, RuleSet, SimpleRule,
};

macro_rules! rule {
    (_simple $matches: expr => [$($style: ident),+]) => {{
        SimpleRule {
            matches: Regex::new($matches).unwrap(),
            style: vec![$(Style::new().fg(Color::$style)),+],
        }
    }};

    (@simple $matches: expr => [$($style: ident),+]) => {{
        Rule::Simple(rule!(_simple $matches => [$($style),+]))
    }};

    (@frac $matches: expr => [$($style: ident),+], $($fol_matches: expr => [$($fol_style: ident),+]),+) => {{
        Rule::Progressive(
            rule!(_simple $matches => [$($style),+]),
            vec![
                $( rule!(_simple $fol_matches => [$($fol_style),+]) ),+
            ]
        )
    }};

    (@nested $begin: expr => $beginStyle: ident, $end: expr => $endStyle: ident, $innerRules: expr) => {
        Rule::Nested(NestingRule {
            begin: rule!(_simple $begin => [$beginStyle]),
            end: rule!(_simple $end => [$endStyle]),
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
                rule!(@simple "(#.*)$" => [DarkGray]),
                // Variable declaration
                rule!(@frac "\\b(let)\\b" => [Magenta],
                            "\\s+(mut)\\b" => [Magenta],
                            "\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\b" => [Red],
                            "\\s*(=)\\s*" => [LightYellow]
                ),
                // Variable assignment
                rule!(@frac "(\\$[a-zA-Z_][a-zA-Z0-9_]*)\\b" => [Red],
                            "\\s*(=)" => [LightYellow]
                ),
                // For loop
                rule!(@frac "\\b(for)\\b" => [Magenta],
                            "\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\b" => [Red],
                            "\\s+(in)\\b" => [Magenta]
                ),
                // Function declaration
                rule!(@frac "\\b(fn)\\b" => [Magenta],
                            "\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\b" => [Blue]
                ),
                // Command alias
                rule!(@frac "\\b(alias)\\b" => [Magenta],
                            "\\s+([a-zA-Z_][a-zA-Z0-9_-]*)\\b" => [Blue],
                            "\\s*(=)" => [LightYellow]
                ),
                // Keywords
                rule!(@simple "\\b(while|if|else|continue|break|throw|try|catch|do|return)\\b" => [Magenta]),
                // Expressions
                rule!(@group "in-expressions"),
                // Command names
                // TODO: make "^" actually work!
                rule!(@simple "(?:^|\\n)\\s*([^\\s\\(\\)\\[\\]\\{\\}<>\\=\\;\\!\\?\\&\\'\\\"\\$]+)" => [Blue]),
                // Raw arguments
                rule!(@simple "(?:^|\\n|\\s+)\\s*([^\\s\\(\\)\\[\\]\\{\\}<>\\=\\;\\!\\?\\&\\'\\\"\\$]+)" => [Green]),
            ]),
            ("in-expressions", vec![
                // Types
                rule!(@simple "\\b(any|bool|int|float|string|list|map|error|struct|fn)\\b" => [Magenta]),
                // Type aliases
                rule!(@simple "\\b([A-Z][a-zA-Z0-9_]*)\\b" => [LightYellow]),
                // Booleans
                rule!(@simple "\\b(true|false)\\b" => [LightYellow]),
                // The null value
                rule!(@simple "\\b(null)\\b" => [LightYellow]),
                // Variables
                rule!(@simple "(\\$[a-zA-Z_][a-zA-Z0-9_]*)\\b" => [Red]),
                // Flags
                rule!(@simple "\\s(\\-[\\-a-zA-Z0-9_]*)" => [LightYellow]),
                // Number
                rule!(@simple "(\\d+(?:\\.\\d+)?)" => [LightYellow]),
                // Symbols and operators
                rule!(@simple "([&\\|,;=!<>\\?\\+\\-\\*\\/:]+)" => [LightYellow]),
                // Strings
                rule!(@nested "(\")" => Green, "(\")" => Green, vec![
                    rule!(@simple "(.+)" => [LightGreen])
                ])
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
