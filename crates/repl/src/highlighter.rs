use std::{sync::Arc, collections::{HashMap, HashSet}};

use nu_ansi_term::{Color, Style};
use reedline::{Highlighter as RlHighlighter, StyledText};
use regex::Regex;

use crate::utils::{
    syntax::{
        HighlightPiece, Rule, RuleSet, SimpleRule,
        ValidatedRuleSet, compute_highlight_pieces, NestedContentRules,
    },
    nesting::NestingOpeningType,
    lazy_cell::LazyCell
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

static RULE_SET: LazyCell<Arc<ValidatedRuleSet>> = LazyCell::new(|| {
    /// Create a simple rule's inner content
    fn simple_rule<S: Into<Style> + Copy>(regex: &'static str, colors: impl AsRef<[S]>) -> SimpleRule {
        SimpleRule {
            matches: Regex::new(regex).unwrap(),
            inside: None,
            preceded_by: None,
            followed_by: None,
            followed_by_nesting: None,
            style: colors.as_ref().iter().copied().map(S::into).collect()
        }
    }

    /// Create a simple rule
    fn simple<S: Into<Style> + Copy>(regex: &'static str, colors: impl AsRef<[S]>) -> Rule {
        Rule::Simple(simple_rule(regex, colors))
    }

    /// Create a simple rule that must be preceded by a given pattern
    fn simple_preceded_by<S: Into<Style> + Copy>(preceded_by: &'static str, regex: &'static str, colors: impl AsRef<[S]>) -> Rule {
        let mut rule = simple_rule(regex, colors);
        rule.preceded_by = Some(Regex::new(preceded_by).unwrap());
        Rule::Simple(rule)
    }

    /// Create a simple rule that must be followed by a given pattern
    fn simple_followed_by<S: Into<Style> + Copy>(regex: &'static str, colors: impl AsRef<[S]>, followed_by: &'static str) -> Rule {
        let mut rule = simple_rule(regex, colors);
        rule.followed_by = Some(Regex::new(followed_by).unwrap());
        Rule::Simple(rule)
    }

    /// Create a simple rule that must be followed by a specific nesting type
    fn simple_followed_by_nesting<S: Into<Style> + Copy>(regex: &'static str, colors: impl AsRef<[S]>, followed_by: impl Into<HashSet<NestingOpeningType>>) -> Rule {
        let mut rule = simple_rule(regex, colors);
        rule.followed_by_nesting = Some(followed_by.into());
        Rule::Simple(rule)
    }

    /// Create a group inclusion rule
    fn include_group(name: &'static str) -> Rule {
        Rule::Group(name.to_owned())
    }
    
    // Import all available colors for ease of use
    use Color::*;

    // Match remaining invalid characters
    let invalid_chars = simple("([^\\s])", [Style::new().fg(White).on(Red)]);

    // Build the rule set
    let rule_set = RuleSet {
        groups: [
            ("instructions", vec![
                // Comments
                simple("(#.*)$", [DarkGray]),
                
                // Variable declaration
                simple("\\b(let)\\s+(mut)\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*(=)", [Magenta, Magenta, Red, LightYellow]),
                simple("\\b(let)\\s+(mut)\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\b", [Magenta, Magenta, Red]),
                simple("\\b(let)\\s+(mut)\\b", [Magenta, Magenta]),
                
                // For loop
                simple("\\b(for)\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s+(in)\\b", [Magenta, Red, Magenta]),
                simple("\\b(for)\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\b", [Magenta, Red]),
                
                // Function declaration
                simple("\\b(fn)\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\b", [Magenta, Blue]),

                // Command aliases
                simple("\\b(alias)\\s+([a-zA-Z_][a-zA-Z0-9_-]*)\\s*(=)", [Magenta, Blue, LightYellow]),
                simple("\\b(alias)\\s+([a-zA-Z_][a-zA-Z0-9_-]*)\\b", [Magenta, Blue]),
                
                // // Argument names and structure keys
                // simple("\\b([a-zA-Z_][a-zA-Z0-9_]*)\\s*([:=])[^/\\\\]", [Red, LightYellow]),

                // Commands separator
                simple("(;)", [DarkGray]),

                // Commands (contains invalid characters as well)
                include_group("commands"),
            ]),
            ("commands", vec![
                // Pipes
                simple("(\\->|\\!?\\|)", [LightYellow]),

                // @direct marker
                simple("(@direct)\\b", [Magenta]),

                // Normalized flags
                simple_followed_by("\\s((?:\\-\\-[a-zA-Z0-9_-]+|\\-[a-zA-Z0-9_])[=]?|\\-?\\-)", [LightYellow], "[\\s\\)\\]}<>\\;\\?\\|\\'\\\"\\$]|$"),

                // Keywords
                simple("\\b(alias|fn|for|while|if|else|continue|break|throw|try|catch|do|return)\\b", [Magenta]),

                // Numbers
                simple("[\\s\\(\\[\\{<>=;\\|](\\d+(?:\\.\\d+)?)(?:[\\s\\(\\)\\[\\]\\{\\}<>=;\\&\\|]|$)", [LightYellow]),

                // Command names
                simple_preceded_by("(^\\s*|[\\|\\n;]\\s*|\\->\\s*|@direct\\s+)$", "([^\\s\\(\\)\\[\\]\\{}<>\\;\\?\\|\\'\\\"\\$]+)", [Blue]),

                // Variables
                simple("(\\$(?:[a-zA-Z_][a-zA-Z0-9_]*)?)\\b", [Red]),

                // Booleans
                simple("\\b(true|false)\\b", [LightYellow]),

                // Raw arguments
                simple("([^\\s\\(\\)\\[\\]\\{\\}<>;\\?\\|'\"\\$]+)", [Green]),

                // Invalid characters
                invalid_chars.clone()
            ]),
            ("literal-strings", vec![
                // Escaped characters
                simple("(\\\\.)", [Cyan]),

                // Any other character
                simple("(.)", [Green]),
            ]),
            ("computed-strings", vec![
                // Escaped characters
                simple("(\\\\.)", [Cyan]),

                // Variables
                simple("(\\$(?:[a-zA-Z_][a-zA-Z0-9_]*)?)", [Red]),

                // Any other character
                simple("(.)", [Green]),
            ]),
            ("expressions", vec![
                // Method calls
                simple_followed_by_nesting("((?:[\\?]|\\s*\\->\\s*)?\\.[a-zA-Z_][a-zA-Z0-9_]*)$", [Blue], [NestingOpeningType::ExprWithParen]),

                // Function calls
                simple_followed_by_nesting("(?:^\\s*|\\b)([a-zA-Z_][a-zA-Z0-9_]*)$", [Blue], [NestingOpeningType::ExprWithParen]),

                // Types
                simple("\\b(any|bool|int|float|string|list|map|error|struct|fn|cmdcall)\\b", [Magenta]),

                // Booleans
                simple("\\b(true|false)\\b", [LightYellow]),

                // The null value
                simple("\\b(null)\\b", [LightYellow]),

                // Variables
                simple("(\\$(?:[a-zA-Z_][a-zA-Z0-9_]*)?)", [Red]),

                // Struct member access
                simple("([\\?]?\\.[a-zA-Z_][a-zA-Z0-9_]*)", [Red]),

                // Functions as values
                simple("(\\@(?:[a-zA-Z_][a-zA-Z0-9_]*)?)", [Magenta]),

                // Numbers
                simple("(\\d+(?:\\.\\d+)?)", [LightYellow]),

                // // Flags
                // simple("(?:[\\|,]\\s*)(\\-[a-zA-Z0-9_-]*)(?:\\s*[:,\\|]|$)", [LightYellow]),

                // Argument names (by elimination we have reached them)
                // simple("(?:[\\|,]\\s*)([a-zA-Z_][a-zA-Z0-9_]*)(?:\\s*[,:\\?\\|]|$)", [Red]),

                // Symbols and operators
                simple("([&\\|,;=!<>\\?\\+\\-\\*\\/:\\(\\)\\{\\}\\[\\]])", [LightYellow]),

                // Invalid characters
                invalid_chars.clone()
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

            (NestingOpeningType::LiteralString, NestedContentRules {
                opening_style: Style::new().fg(Green),
                closing_style: Style::new().fg(Green),
                rules: vec![
                    include_group("literal-strings")
                ]
            }),

            (NestingOpeningType::ComputedString, NestedContentRules {
                opening_style: Style::new().fg(Green),
                closing_style: Style::new().fg(Green),
                rules: vec![
                    include_group("computed-strings")
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

        unclosed_style: Style::new().fg(White).on(Red),
    };

    Arc::new(ValidatedRuleSet::validate(rule_set).unwrap())
});

fn highlight(input: &str) -> StyledText {
    StyledText {
        buffer:
            compute_highlight_pieces(input, &RULE_SET)
                .into_iter()
                .map(|piece| {
                    let HighlightPiece { start, len, style } = piece;
        
                    (
                        style.unwrap_or_default(),
                        input[start..start + len].to_owned()
                    )
                })
                .collect()
    }
}
