//!
//! This module defines the rule set and systems used for highlighting ReShell programs
//! using the [`super::utils::syntax`] module.
//!

use std::{
    collections::{HashMap, HashSet},
    sync::{Arc, LazyLock},
};

use nu_ansi_term::{Color, Style};
use reedline::{Highlighter as RlHighlighter, StyledText};
use regex::Regex;

use crate::{
    repl::SHARED_CONTEXT,
    utils::{
        cmd_checker::{CheckCmdType, COMMANDS_CHECKER},
        nesting::NestingOpeningType,
        syntax::{
            compute_highlight_pieces, HighlightPiece, NestedContentRules, Rule, RuleSet,
            RuleStylization, SimpleRule, ValidatedRuleSet,
        },
    },
};

pub fn create_highlighter() -> Box<dyn RlHighlighter> {
    Box::new(Highlighter)
}

pub struct Highlighter;

impl RlHighlighter for Highlighter {
    fn highlight(&self, line: &str, _cursor: usize) -> StyledText {
        if line.is_empty() {
            COMMANDS_CHECKER
                .lock()
                .unwrap()
                .refresh(SHARED_CONTEXT.lock().unwrap().as_mut().unwrap());
        }

        highlight(line)
    }
}

static RULE_SET: LazyLock<Arc<ValidatedRuleSet>> = LazyLock::new(|| {
    /// Create a simple rule's inner content
    fn simple_rule<S: Into<Style> + Copy>(
        regex: &'static str,
        colors: impl AsRef<[S]>,
    ) -> SimpleRule {
        SimpleRule {
            matches: Regex::new(regex).unwrap(),
            inside: None,
            preceded_by: None,
            followed_by: None,
            followed_by_nesting: None,
            style: RuleStylization::Static(colors.as_ref().iter().copied().map(S::into).collect()),
        }
    }

    /// Create a simple rule
    fn simple<S: Into<Style> + Copy>(regex: &'static str, colors: impl AsRef<[S]>) -> Rule {
        Rule::Simple(simple_rule(regex, colors))
    }

    /// Create a simple rule that must be preceded by a given pattern
    fn simple_preceded_by<S: Into<Style> + Copy>(
        preceded_by: &'static str,
        regex: &'static str,
        colors: impl AsRef<[S]>,
    ) -> Rule {
        let mut rule = simple_rule(regex, colors);
        rule.preceded_by = Some(Regex::new(preceded_by).unwrap());
        Rule::Simple(rule)
    }

    /// Create a simple rule that must be followed by a given pattern
    fn simple_followed_by<S: Into<Style> + Copy>(
        regex: &'static str,
        colors: impl AsRef<[S]>,
        followed_by: &'static str,
    ) -> Rule {
        let mut rule = simple_rule(regex, colors);
        rule.followed_by = Some(Regex::new(followed_by).unwrap());
        Rule::Simple(rule)
    }

    /// Create a group inclusion rule
    fn include_group(name: &'static str) -> Rule {
        Rule::Group(name.to_owned())
    }

    // Import all available colors for ease of use
    use Color::*;

    // Match method calls
    let method_call = || {
        Rule::Simple(SimpleRule {
            matches: Regex::new("(\\.)([a-zA-Z_][a-zA-Z0-9_]*)$").unwrap(),
            inside: None,
            preceded_by: None,
            followed_by: None,
            followed_by_nesting: Some(HashSet::from([NestingOpeningType::ExprWithParen])),
            style: RuleStylization::Dynamic(Box::new(|ctx, matched| {
                let color = if COMMANDS_CHECKER.lock().unwrap().check(
                    ctx,
                    &matched[2],
                    CheckCmdType::Method,
                ) {
                    Color::Blue
                } else {
                    Color::Red
                };

                vec![Style::new().fg(Color::LightYellow), Style::new().fg(color)]
            })),
        })
    };

    // Build the rule set
    let rule_set = RuleSet {
        groups: [
            ("instructions", vec![
                // Comments
                simple("(#.*)$", [DarkGray]),
                
                // Mutable variable declaration
                simple("\\b(let)\\s+(mut)\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*(=)", [Magenta, Magenta, Red, LightYellow]),
                simple("\\b(let)\\s+(mut)\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\b", [Magenta, Magenta, Red]),
                simple("\\b(let)\\s+(mut)\\b", [Magenta, Magenta]),
                
                // Immutable variable declaration
                simple("\\b(let)\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*(=)", [Magenta, Red, LightYellow]),
                simple("\\b(let)\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\b", [Magenta, Red]),
                simple("\\b(let)\\b", [Magenta]),

                // For loop
                simple("\\b(for)\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s+(in)\\b", [Magenta, Red, Magenta]),
                simple("\\b(for)\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\b", [Magenta, Red]),
                
                // Function declaration
                simple("\\b(fn)\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\b", [Magenta, Blue]),

                // Command aliases
                simple("\\b(alias)\\s+([a-zA-Z_][a-zA-Z0-9_-]*)\\s*(=)", [Magenta, Blue, LightYellow]),
                simple("\\b(alias)\\s+([a-zA-Z_][a-zA-Z0-9_-]*)\\b", [Magenta, Blue]),

                // Commands
                include_group("commands"),
            ]),
            ("commands", vec![
                // Pipes
                simple("(\\->|\\!?\\|)", [LightYellow]),

                // Markers
                simple("^\\s*(include)(?:\\s|$)", [Magenta]),

                // Normalized flags
                simple_followed_by("\\s(\\-\\-?(?:\\p{Letter}|[_\\-\\+])*)", [LightYellow], "[\\s=\\)\\]}<>\\;\\?\\|\\'\\\"\\$]|$"),

                // Keywords
                simple("\\b(alias|fn|for|while|if|else|continue|typematch|match|break|throw|try|catch|return)(?:\\s|$)", [Magenta]),

                // 'self' keyword
                simple("\\b(self)(?:\\.|$)", [Magenta]),

                // Numbers
                simple("[\\s\\(\\[\\{<>=;\\|](\\d+(?:\\.\\d+)?)(?:[\\s\\(\\)\\[\\]\\{\\}<>=;\\&\\|]|$)", [LightYellow]),

                // Symbols and operators
                simple("\\s(\\!=|&&|\\|\\||[&\\|,;=!<>\\?\\+\\-\\*\\/:\\(\\)\\{\\}\\[\\]])(?:\\s|$)", [LightYellow]),
                simple("\\s(!)", [LightYellow]),

                // Escaped arguments
                simple_preceded_by("(\\\\\\n)\\s+$", "([^\\s\\(\\)\\[\\]\\{}<>\\;\\?\\|\\'\\\"\\$]+)", [Green]),

                // Method names
                Rule::Simple(SimpleRule {
                    matches: Regex::new("(\\.)([a-zA-Z_][a-zA-Z0-9_]*)").unwrap(),
                    inside: None,
                    preceded_by: Some(Regex::new("(^|[@\\$]\\(|[\\|\\n;\\{])\\s*$").unwrap()),
                    followed_by: Some(Regex::new("\\s|$").unwrap()),
                    followed_by_nesting: None,
                    style: RuleStylization::Dynamic(Box::new(|ctx, matched| {
                        let method_exists = ctx.visible_scopes().any(|scope| scope.content.methods.keys().any(|method_name| method_name == &matched[2]));

                        let color = if method_exists {
                            Color::Blue
                        } else {
                            Color::Red
                        };
            
                        vec![Style::new().fg(Color::LightYellow), Style::new().fg(color)]
                    }))
                }),

                // Command names
                Rule::Simple(SimpleRule {
                    matches: Regex::new("(\\^|)([^\\s\\(\\)\\[\\]\\{}<>\\;\\?\\|\\'\\\"\\$\\^]+)").unwrap(),
                    inside: None,
                    preceded_by: Some(Regex::new("(^|[@\\$]\\(|[\\|\\n;\\{])\\s*$").unwrap()),
                    followed_by: None,
                    followed_by_nesting: None,
                    style: RuleStylization::Dynamic(Box::new(|ctx, matched| {
                        let is_external = !matched[1].is_empty();

                        let cmd_type = if is_external {
                            CheckCmdType::ExternalCmd
                        } else {
                            CheckCmdType::BroadCmd
                        };

                        let color = if COMMANDS_CHECKER.lock().unwrap().check(ctx, &matched[2], cmd_type) {
                            Color::Blue
                        } else {
                            Color::Red
                        };


                        vec![Style::new().fg(Color::Magenta), Style::new().fg(color)]
                    }))
                }),

                // Variables
                simple("(\\$(?:[a-zA-Z_][a-zA-Z0-9_]*)?)\\b", [Red]),

                // Booleans
                simple("\\b(true|false)\\b", [LightYellow]),

                // Expansions
                simple("[\\s,](\\.\\.\\.)(?:$|[\\$\\(])", [Red]),
                
                // Method calls
                method_call(),

                // Any other character
                simple("(.)", [Green]),
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
                method_call(),

                // Function calls
                Rule::Simple(SimpleRule {
                    matches: Regex::new("(?:^\\s*|\\b)([a-zA-Z_][a-zA-Z0-9_]*)$").unwrap(),
                    inside: None,
                    followed_by: None,
                    followed_by_nesting: Some(HashSet::from([NestingOpeningType::ExprWithParen])),
                    preceded_by: None,
                    style: RuleStylization::Dynamic(Box::new(|ctx, matched| {
                        let color = if COMMANDS_CHECKER.lock().unwrap().check(ctx, &matched[1], CheckCmdType::Function) {
                            Color::Blue
                        } else {
                            Color::Red
                        };

                        vec![Style::new().fg(color)]
                    }))
                }),

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
                simple("\\b(\\d+(?:\\.\\d+)?)\\b", [LightYellow]),

                // Symbols and operators
                simple("(,|\\s[&\\|;=!<>\\?\\+\\-\\*\\/:\\(\\)\\{\\}\\[\\]\\!]\\s|&&|\\|\\|)", [LightYellow]),

                // 'typeis' operator
                simple("\\b(typeis)\\b", [Magenta]),

                // Other characters
                simple("(.)", [Green])
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

            (NestingOpeningType::CmdOutput, NestedContentRules {
                opening_style: Style::new().fg(LightYellow),
                closing_style: Style::new().fg(LightYellow),
                rules: vec![
                    include_group("commands")
                ]
            }),

            (NestingOpeningType::CmdCall, NestedContentRules {
                opening_style: Style::new().fg(LightYellow),
                closing_style: Style::new().fg(LightYellow),
                rules: vec![
                    include_group("commands")
                ]
            }),

            (NestingOpeningType::Lambda, NestedContentRules {
                opening_style: Style::new().fg(LightBlue),
                closing_style: Style::new().fg(LightBlue),
                rules: vec![
                    include_group("commands")
                ]
            }),

            (NestingOpeningType::FnArgs, NestedContentRules {
                opening_style: Style::new().fg(LightYellow),
                closing_style: Style::new().fg(LightYellow),
                rules: vec![
                    // Normalized flags
                    simple_followed_by("((?:\\-\\-[a-zA-Z0-9_-]+|\\-[a-zA-Z0-9_])[=]?|\\-?\\-)", [LightYellow], "[\\s\\)\\]}<>\\;\\?\\|\\'\\\"\\$]|$"),

                    // Types
                    simple("\\:\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\b", [Magenta]),

                    // Variables
                    simple("([a-zA-Z_][a-zA-Z0-9_]*)\\b", [Red]),
                ]
            })
        ]),

        non_nested_content_rules: vec![
            include_group("instructions")
        ],

        closing_without_opening_style: Style::new().fg(White).on(Red),

        unclosed_style: Style::new().fg(White).on(Red),

        command_separator_style: Style::new().fg(DarkGray),

        use_arguments_separator: false
    };

    Arc::new(ValidatedRuleSet::validate(rule_set).unwrap())
});

fn highlight(input: &str) -> StyledText {
    StyledText {
        buffer: compute_highlight_pieces(input, &RULE_SET)
            .into_iter()
            .map(|piece| {
                let HighlightPiece { start, len, style } = piece;

                (
                    style.unwrap_or_default(),
                    input[start..start + len].to_owned(),
                )
            })
            .collect(),
    }
}
