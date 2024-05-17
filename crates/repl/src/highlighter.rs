//!
//! This module defines the rule set and systems used for highlighting ReShell programs
//! using the [`super::utils::syntax`] module.
//! 

use std::{
    collections::{HashMap, HashSet}, sync::{Arc, LazyLock, Mutex}
};

use nu_ansi_term::{Color, Style};
use reedline::{Highlighter as RlHighlighter, StyledText};
use regex::Regex;
use reshell_runtime::{bin_resolver::BinariesResolver, cmd::try_replace_home_dir_tilde, context::Context};

use crate::{repl::SHARED_CONTEXT, utils::{
    nesting::NestingOpeningType,
    syntax::{
        compute_highlight_pieces, HighlightPiece, NestedContentRules, Rule,
        RuleSet, RuleStylization, SimpleRule, ValidatedRuleSet,
    },
}};

pub fn create_highlighter() -> Box<dyn RlHighlighter> {
    Box::new(Highlighter)
}

pub struct Highlighter;

impl RlHighlighter for Highlighter {
    fn highlight(&self, line: &str, _cursor: usize) -> StyledText {
        if line.is_empty() {
            COMMANDS_CHECKER.lock().unwrap().clear(
                SHARED_CONTEXT.lock().unwrap().as_mut().unwrap().binaries_resolver()
            );
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

    /// Create a simple rule that must be surrounded by two given patterns
    fn simple_nested_and_followed_by<S: Into<Style> + Copy>(
        nesting_type: NestingOpeningType,
        regex: &'static str,
        colors: impl AsRef<[S]>,
        followed_by: &'static str,
    ) -> Rule {
        let mut rule = simple_rule(regex, colors);
        rule.inside = Some(HashSet::from([nesting_type]));
        rule.followed_by = Some(Regex::new(followed_by).unwrap());
        Rule::Simple(rule)
    }

    /// Create a simple rule that must be followed by a specific nesting type
    fn simple_followed_by_nesting<S: Into<Style> + Copy>(
        regex: &'static str,
        colors: impl AsRef<[S]>,
        followed_by: impl Into<HashSet<NestingOpeningType>>,
    ) -> Rule {
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

    // Match method calls
    let method_call = || Rule::Simple(SimpleRule {
        matches: Regex::new("(\\.)([a-zA-Z_][a-zA-Z0-9_]*)$").unwrap(),
        inside: None,
        preceded_by: None,
        followed_by: None,
        followed_by_nesting: Some(HashSet::from([NestingOpeningType::ExprWithParen])),
        style: RuleStylization::Dynamic(Box::new(|ctx, matched| {
            let color = if COMMANDS_CHECKER.lock().unwrap().check(ctx, &matched[0]) {
                Color::Blue
            } else {
                Color::Red
            };

            vec![Style::new().fg(Color::LightYellow), Style::new().fg(color)]
        }))
    });

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
                simple("\\b(direct|include|output)(?:\\s|$)", [Magenta]),

                // Normalized flags
                simple_followed_by("\\s((?:\\-\\-[a-zA-Z0-9_-]+|\\-[a-zA-Z0-9_])[=]?|\\-?\\-)", [LightYellow], "[\\s\\)\\]}<>\\;\\?\\|\\'\\\"\\$]|$"),

                // Keywords
                simple("\\b(alias|fn|for|while|if|else|continue|break|throw|try|catch|return)\\b", [Magenta]),

                // Numbers
                simple("[\\s\\(\\[\\{<>=;\\|](\\d+(?:\\.\\d+)?)(?:[\\s\\(\\)\\[\\]\\{\\}<>=;\\&\\|]|$)", [LightYellow]),

                // Symbols and operators
                simple("\\s(\\!=|&&|\\|\\||[&\\|,;=!<>\\?\\+\\-\\*\\/:\\(\\)\\{\\}\\[\\]])(?:\\s|$)", [LightYellow]),
                simple("\\s(!)", [LightYellow]),

                // Escaped arguments
                simple_preceded_by("(\\\\\\n)\\s+$", "([^\\s\\(\\)\\[\\]\\{}<>\\;\\?\\|\\'\\\"\\$]+)", [Green]),

                // Lambdas
                simple_nested_and_followed_by(NestingOpeningType::Block, "([a-zA-Z0-9_,\\s]+)", [Red], "->"),

                // Method names
                Rule::Simple(SimpleRule {
                    matches: Regex::new("(\\.)([a-zA-Z_][a-zA-Z0-9_]*)").unwrap(),
                    inside: None,
                    preceded_by: Some(Regex::new("(^|[\\|\\n;\\{]|\\->)\\s*$").unwrap()),
                    followed_by: Some(Regex::new("\\s|$").unwrap()),
                    followed_by_nesting: None,
                    style: RuleStylization::Dynamic(Box::new(|ctx, matched| {
                        let method_exists = ctx.visible_scopes().any(|scope| scope.content.methods.keys().any(|(method_name, _)| method_name == &matched[2]));

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
                    matches: Regex::new("([^\\s\\(\\)\\[\\]\\{}<>\\;\\?\\|\\'\\\"\\$]+)").unwrap(),
                    inside: None,
                    preceded_by: Some(Regex::new("(^|\\$\\(|[\\|\\n;\\{]|\\->|\\b(?:direct|output)\\s+|\\s+(?:if|in|=|&&|\\|\\|)\\s+)\\s*$").unwrap()),
                    followed_by: None,
                    followed_by_nesting: None,
                    style: RuleStylization::Dynamic(Box::new(|ctx, matched| {
                        let color = if COMMANDS_CHECKER.lock().unwrap().check(ctx, &matched[1]) {
                            Color::Blue
                        } else {
                            Color::Red
                        };

                        vec![Style::new().fg(color)]
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

                // Expressions
                include_group("expressions")
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
                simple("\\b(\\d+(?:\\.\\d+)?)\\b", [LightYellow]),

                // // Flags
                // simple("(?:[\\|,]\\s*)(\\-[a-zA-Z0-9_-]*)(?:\\s*[:,\\|]|$)", [LightYellow]),

                // Argument names (by elimination we have reached them)
                // simple("(?:[\\|,]\\s*)([a-zA-Z_][a-zA-Z0-9_]*)(?:\\s*[,:\\?\\|]|$)", [Red]),

                // Symbols and operators
                simple("([&\\|,;=!<>\\?\\+\\-\\*\\/:\\(\\)\\{\\}\\[\\]\\!]|&&|\\|\\|)", [LightYellow]),

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

pub struct CommandsChecker {
    for_path: Vec<String>,
    entries: HashMap<String, bool>
}

impl CommandsChecker {
    pub fn new() -> Self {
        Self {
            for_path: vec![],
            entries: HashMap::new()
        }
    }

    pub fn update(&mut self, bin_resolver: &mut BinariesResolver) {
        if &self.for_path != bin_resolver.path_dirs() {
            self.for_path.clone_from(bin_resolver.path_dirs());
            self.update(bin_resolver);
        }
    }

    pub fn clear(&mut self, bin_resolver: &mut BinariesResolver) {
        self.entries = bin_resolver.entries().keys().map(|key| (key.clone(), true)).collect();
    }

    pub fn check(&mut self, ctx: &mut Context, name: &str) -> bool {
        self.update(ctx.binaries_resolver());

        if let Some(exists) = self.entries.get(name) {
            return *exists;
        }

        let exists = match name.strip_prefix('.') {
            Some(name) if !name.starts_with(['/','\\']) => ctx.visible_scopes().any(|scope| scope.content.methods.keys().any(|(method, _)| method == name) || scope.content.cmd_aliases.contains_key(name)),

            Some(_) | None => {
                if !name.starts_with(['/', '\\']) && ctx.visible_scopes().any(|scope| scope.content.fns.contains_key(name) || scope.content.cmd_aliases.contains_key(name)) {
                    return true;
                }

                let Ok(name) = try_replace_home_dir_tilde(name, ctx) else {
                    return false;
                };

                ctx.binaries_resolver().resolve_binary_path(&name).is_ok()
            },
        };

        self.entries.insert(name.to_owned(), exists);

        exists
    }
}

pub static COMMANDS_CHECKER: LazyLock<Arc<Mutex<CommandsChecker>>> = LazyLock::new(|| Arc::new(Mutex::new(CommandsChecker::new())));
