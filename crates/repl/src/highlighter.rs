use std::{sync::Arc, collections::HashMap};

use nu_ansi_term::{Color, Style};
use once_cell::sync::Lazy;
use reedline::{Highlighter as RlHighlighter, StyledText};
use regex::Regex;

use crate::utils::{syntax::{
    HighlightPiece, Rule, RuleSet, SimpleRule,
    ValidatedRuleSet, compute_highlight_pieces, NestedContentRules,
}, nesting::NestingOpeningType};

pub fn create_highlighter() -> Box<dyn RlHighlighter> {
    Box::new(Highlighter)
}

pub struct Highlighter;

impl RlHighlighter for Highlighter {
    fn highlight(&self, line: &str, _cursor: usize) -> StyledText {
        highlight(line)
    }
}

static RULE_SET: Lazy<Arc<ValidatedRuleSet>> = Lazy::new(|| {
    /// Create a simple rule's inner content
    fn simple_rule(regex: &'static str, colors: impl AsRef<[Color]>) -> SimpleRule {
        SimpleRule { matches: Regex::new(regex).unwrap(), style: colors.as_ref().iter().map(|color| Style::new().fg(*color)).collect() }
    }

    /// Create a simple rule
    fn simple(regex: &'static str, colors: impl AsRef<[Color]>) -> Rule {
        Rule::Simple(simple_rule(regex, colors))
    }

    /// Create a progressive rule
    fn progressive<C: AsRef<[Color]>>(parts: impl AsRef<[(&'static str, C)]>) -> Rule {
        let mut parts = parts.as_ref().iter().map(|(regex, colors)| simple_rule(regex, colors)).collect::<Vec<_>>();
        Rule::Progressive(parts.remove(0), parts)
    }

    /// Create a group inclusion rule
    fn include_group(name: &'static str) -> Rule {
        Rule::Group(name.to_owned())
    }
    
    // Import all available colors for ease of use
    use Color::*;

    // Build the rule set
    let rule_set = RuleSet {
        groups: [
            ("instructions", vec![
                // Comments
                simple("(#.*)$", [DarkGray]),
                
                // Variable declaration
                progressive([
                    ("\\b(let)\\b", [Magenta]),
                    ("(\\s+mut\\b|)", [Magenta]),
                    ("\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\b", [Red]),
                    ("\\s*(=)\\s*", [LightYellow])
                ]),
                
                // For loop
                progressive([
                    ("\\b(for)\\b", [Magenta]),
                    ("\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\b", [Red]),
                    ("\\s+(in)\\b", [Magenta])
                ]),
                
                // Function declaration
                progressive([
                    ("\\b(fn)\\b", [Magenta]),
                    ("\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\b", [Blue]),
                    ("\\s*(\\()", [LightYellow])
                ]),

                // Command aliases
                progressive([
                    ("\\b(alias)\\b", [Magenta]),
                    ("\\s+([a-zA-Z_][a-zA-Z0-9_-]*)\\b", [Blue]),
                    ("\\s*(=)", [LightYellow])
                ]),
                
                // Keywords
                simple("\\b(while|if|else|continue|break|throw|try|catch|do|return)\\b", [Magenta]),

                // Argument names and structure keys
                simple("\\b([a-zA-Z_][a-zA-Z0-9_]*)\\s*([:=])[^/\\\\]", [Red, LightYellow]),
                
                // Commands
                include_group("commands")
            ]),
            ("commands", vec![
                // Command names
                simple("(?:^|[\\n\\{\\|]|\\$\\()\\s*([^\\s\\(\\)\\[\\]\\{\\}<>\\=\\;\\!\\?\\&\\|\\'\\\"\\$]+)", [Blue]),

                // Variables
                simple("(\\$[a-zA-Z_][a-zA-Z0-9_]*)\\b", [Red]),

                // Single variable marker
                simple("(\\$)", [Red]),

                // Flags
                simple("\\s(\\-[\\-a-zA-Z0-9_]*)", [LightYellow]),

                // Numbers
                simple("[\\s\\(\\[\\{<>=;\\|](\\d+(?:\\.\\d+)?)(?:[\\s\\(\\)\\[\\]\\{\\}<>=;\\&\\|]|$)", [LightYellow]),

                // Raw arguments
                simple("([^\\s\\(\\)\\[\\]\\{\\}<>=;\\!\\?\\&\\|'\"\\$]+)", [Green]),
            ]),
            ("strings", vec![
                // Escaped characters
                simple("(\\\\.)", [Cyan]),

                // Any other character
                simple("(.)", [Green]),
            ]),
            ("expressions", vec![
                // Function calls
                simple("\\b([a-zA-Z_][a-zA-Z0-9_]*)\\(", [Blue]),

                // Types
                simple("\\b(any|bool|int|float|string|list|map|error|struct|fn)\\b", [Magenta]),

                // Booleans
                simple("\\b(true|false)\\b", [LightYellow]),

                // The null value
                simple("\\b(null)\\b", [LightYellow]),

                // Flags
                simple("\\s(\\-[\\-a-zA-Z0-9_]*)", [LightYellow]),

                // Variables
                simple("(\\$[a-zA-Z_][a-zA-Z0-9_]*)\\b", [Red]),

                // Single variable marker
                simple("(\\$)", [Red]),

                // Numbers
                simple("(\\d+(?:\\.\\d+)?)", [LightYellow]),

                // Symbols and operators
                simple("([&\\|,;=!<>\\?\\+\\-\\*\\/:\\(\\)\\{\\}\\[\\]]+)", [LightYellow]),

                // Flags
                simple("(?:[\\(,\\s])(-[a-zA-Z0-9_-]*)(?:$|[,\\s\\)])", [LightYellow]),
            ])
        ]
        .into_iter().map(|(group, rules)| (group.to_owned(), rules)).collect(),

        nested_content_rules: HashMap::from([
            (NestingOpeningType::Block, NestedContentRules {
                opening_style: Style::new().fg(DarkGray),
                closing_style: Style::new().fg(DarkGray),
                rules: vec![
                    include_group("instructions")
                ]
            }),

            (NestingOpeningType::List, NestedContentRules {
                opening_style: Style::new().fg(Magenta),
                closing_style: Style::new().fg(Magenta),
                rules: vec![
                    include_group("expressions")
                ]
            }),

            (NestingOpeningType::ExprWithParen, NestedContentRules {
                opening_style: Style::new().fg(Blue),
                closing_style: Style::new().fg(Blue),
                rules: vec![
                    include_group("expressions")
                ]
            }),

            (NestingOpeningType::String, NestedContentRules {
                opening_style: Style::new().fg(Green),
                closing_style: Style::new().fg(Green),
                rules: vec![
                    include_group("strings")
                ]
            }),

            (NestingOpeningType::ExprInString, NestedContentRules {
                opening_style: Style::new().fg(Blue),
                closing_style: Style::new().fg(Blue),
                rules: vec![
                    include_group("expressions")
                ]
            }),

            (NestingOpeningType::CmdCallInString, NestedContentRules {
                opening_style: Style::new().fg(LightYellow),
                closing_style: Style::new().fg(LightYellow),
                rules: vec![
                    include_group("instructions")
                ]
            }),
        ]),

        non_nested_content_rules: vec![
            include_group("instructions")
        ],

        closing_without_opening_style: Style::new().fg(White).on(Red),

        unclosed_style: Style::new().fg(White).on(Red)
    };

    Arc::new(ValidatedRuleSet::validate(rule_set).unwrap())
});

fn highlight(input: &str) -> StyledText {
    let mut out = StyledText::new();

    for piece in compute_highlight_pieces(input, &RULE_SET) {
        let HighlightPiece { start, len, style } = piece;

        out.push((
            style.unwrap_or_default(),
            input[start..start + len].to_owned(),
        ));
    }

    out
}
