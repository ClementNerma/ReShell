use std::sync::Arc;

use nu_ansi_term::{Color, Style};
use once_cell::sync::Lazy;
use reedline::{Highlighter as RlHighlighter, StyledText};
use regex::Regex;

use crate::syntax::{
    HighlightPiece, Highlighter as SyntaxHighlighter, NestingRule, Rule, RuleSet, SimpleRule,
    ValidatedRuleSet,
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

    /// Create a nested rule
    fn nested(begin: SimpleRule, end: SimpleRule, inner_rules: Vec<Rule>) -> Rule {
        Rule::Nested(NestingRule { begin, end, inner_rules })
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

                //
                // This is the "less polished" part of the highlighter
                //

                // Variables
                simple("(\\$[a-zA-Z_][a-zA-Z0-9_]*)\\b", [Red]),

                // Single variable marker
                simple("(\\$)", [Red]),

                // Number
                simple("(?:\\s*)(\\d+(?:\\.\\d+)?)(?:[^\\d]|$)", [LightYellow]),

                // Expressions
                nested(
                    simple_rule("(\\()", [LightYellow]),
                    simple_rule("(\\))", [LightYellow]),
                    vec![include_group("in-expressions")]
                ),

                // Strings
                include_group("strings"),

                // Raw arguments
                simple("([^\\s\\(\\)\\[\\]\\{\\}<>=;\\!\\?\\&\\|'\"\\$]+)", [Green]),
            ]),
            ("strings", vec![
                // Strings
                nested(
                    simple_rule("(\")", [Green]),
                    simple_rule("(\")", [Green]),
                    vec![
                        // Commands
                        nested(
                            simple_rule("(?:^|[^\\\\])(\\$\\()", [Blue]),
                            simple_rule("(\\))", [Blue]),
                            vec![ include_group("commands") ]
                        ),

                        // Commands
                        nested(
                            simple_rule("(?:^|[^\\\\])(\\$\\{)", [Blue]),
                            simple_rule("(\\})", [Blue]),
                            vec![ include_group("in-expressions") ]
                        ),

                        // Escaped characters
                        simple("(\\\\.)", [Cyan]),

                        // Any other character
                        simple("(.)", [Green]),
                    ]
                )
            ]),
            ("in-expressions", vec![
                include_group("strings"),

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

                // Number
                simple("(\\d+(?:\\.\\d+)?)", [LightYellow]),

                // Symbols and operators
                simple("([&\\|,;=!<>\\?\\+\\-\\*\\/:]+)", [LightYellow]),
            ],
        )
        ]
        .into_iter().map(|(group, rules)| (group.to_owned(), rules)).collect(),
        rules: vec![
            include_group("instructions")
        ],
    };

    Arc::new(ValidatedRuleSet::validate(rule_set).unwrap())
});

fn highlight(input: &str) -> StyledText {
    let pieces = SyntaxHighlighter::new(&RULE_SET).highlight(input);

    // TODO: highlight unclosed (override existing highlight pieces)

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
