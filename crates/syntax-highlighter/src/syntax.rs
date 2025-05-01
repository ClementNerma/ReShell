//!
//! Syntax highlighting module.
//!
//! Enables highlighting of a source code using a run-time compiled rule set based on regular expressions.
//!
//! Regular expressions work as usual, except for the `^` and `$` symbol which are delimited differently
//! `^` refers to the nearest nested rule opening, or to the beginning of input if nesting level is zero
//!

use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
};

use regex::{Captures, Regex};

use super::{
    coverage::{InputCoverage, InputRange},
    nesting::{NestingAction, NestingActionType, NestingOpeningType, detect_nesting_actions},
};
use crate::ItemType;

#[derive(Debug)]
pub struct ValidatedRuleSet<T>(RuleSet<T>);

#[derive(Debug)]
pub struct RuleSet<T> {
    pub groups: HashMap<String, Vec<Rule<T>>>,
    pub non_nested_content_rules: Vec<Rule<T>>,
    pub nested_content_rules: RulesForNesting<T>,
    pub closing_without_opening_style: ItemType,
    pub unclosed_style: ItemType,
    pub command_separator_style: ItemType,
    pub use_arguments_separator: bool,
}

#[derive(Debug)]
pub struct RulesForNesting<T> {
    pub block: NestedContentRules<T>,
    pub list: NestedContentRules<T>,
    pub expr_with_paren: NestedContentRules<T>,
    pub literal_string: NestedContentRules<T>,
    pub computed_string: NestedContentRules<T>,
    pub expr_in_string: NestedContentRules<T>,
    pub var_spreading: NestedContentRules<T>,
    pub cmd_output: NestedContentRules<T>,
    pub cmd_call: NestedContentRules<T>,
    pub lambda: NestedContentRules<T>,
    pub fn_args: NestedContentRules<T>,
}

impl<T> RulesForNesting<T> {
    pub fn get_for_type(&self, typ: NestingOpeningType) -> &NestedContentRules<T> {
        match typ {
            NestingOpeningType::Block => &self.block,
            NestingOpeningType::List => &self.list,
            NestingOpeningType::ExprWithParen => &self.expr_with_paren,
            NestingOpeningType::LiteralString => &self.literal_string,
            NestingOpeningType::ComputedString => &self.computed_string,
            NestingOpeningType::ExprInString => &self.expr_in_string,
            NestingOpeningType::VarSpreading => &self.var_spreading,
            NestingOpeningType::CmdOutput => &self.cmd_output,
            NestingOpeningType::CmdCall => &self.cmd_call,
            NestingOpeningType::Lambda => &self.lambda,
            NestingOpeningType::FnArgs { lambda: _ } => &self.fn_args,
        }
    }
}

#[derive(Debug)]
pub struct NestedContentRules<T> {
    pub opening_style: fn(usize) -> ItemType,
    pub closing_style: fn(usize) -> ItemType,
    pub rules: Vec<Rule<T>>,
}

#[derive(Debug)]
pub enum Rule<T> {
    Simple(SimpleRule<T>),
    Group(String),
}

#[derive(Debug)]
pub struct SimpleRule<T> {
    pub matches: Regex,
    pub inside: Option<HashSet<NestingOpeningType>>,
    pub preceded_by: Option<Regex>,
    pub followed_by: Option<Regex>,
    pub followed_by_nesting: Option<HashSet<NestingOpeningType>>,
    pub style: RuleStylization<T>,
}

pub enum RuleStylization<T> {
    Static(Vec<ItemType>),
    Dynamic(DynamicRuleStylizer<T>),
}

impl<T> std::fmt::Debug for RuleStylization<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Static(styles) => f.debug_tuple("Static").field(styles).finish(),
            Self::Dynamic(_) => f.debug_tuple("Dynamic").finish(),
        }
    }
}

pub type DynamicRuleStylizer<T> =
    Box<dyn Fn(Vec<String>, &T) -> Vec<ItemType> + Send + Sync + 'static>;

pub fn compute_highlight_pieces<T>(
    input: &str,
    rule_set: &ValidatedRuleSet<T>,
    rule_data: &T,
) -> Vec<SyntaxItem> {
    let ValidatedRuleSet(rule_set) = rule_set;

    let RuleSet {
        groups,
        non_nested_content_rules,
        nested_content_rules,
        closing_without_opening_style,
        unclosed_style,
        command_separator_style,
        use_arguments_separator,
    } = rule_set;

    let mut output = Vec::<SyntaxItem>::new();

    let nesting = detect_nesting_actions(input, *use_arguments_separator);

    let mut opened = Vec::<(NestingAction, NestingOpeningType)>::new();

    for (i, action) in nesting.iter().copied().enumerate() {
        let NestingAction {
            offset,
            len,
            action_type,
            nesting_level,
        } = action;

        match action_type {
            NestingActionType::Opening {
                typ,
                matching_close,
            } => {
                opened.push((action, typ));

                output.push(SyntaxItem {
                    start: offset,
                    len,
                    item: if matching_close {
                        let style = nested_content_rules.get_for_type(typ).opening_style;
                        style(nesting_level)
                    } else {
                        *unclosed_style
                    },
                });
            }

            NestingActionType::Closing { matching_opening } => {
                output.push(SyntaxItem {
                    start: offset,
                    len,
                    item: if matching_opening.is_some() {
                        let style = nested_content_rules
                            .get_for_type(opened.pop().unwrap().1)
                            .closing_style;

                        style(nesting_level)
                    } else {
                        *closing_without_opening_style
                    },
                });
            }

            NestingActionType::CommandSeparator => output.push(SyntaxItem {
                start: offset,
                len,
                item: *command_separator_style,
            }),

            NestingActionType::ArgumentSeparator => {
                // output.push(HighlightedPiece {
                //     start: offset,
                //     len,
                //     // No styling (these are spaces)
                //     item: None,
                // })
            }

            NestingActionType::Content => {
                let rules = match opened.last() {
                    Some((_, opening_type)) => {
                        &nested_content_rules.get_for_type(*opening_type).rules
                    }

                    None => non_nested_content_rules,
                };

                let inside = opened
                    .last()
                    .map(|(action, typ)| (action.offset + action.len, *typ));

                let next_nesting =
                    nesting
                        .get(i + 1)
                        .and_then(|nesting| match nesting.action_type {
                            NestingActionType::Opening {
                                typ,
                                matching_close: _,
                            } => Some(typ),

                            NestingActionType::Closing {
                                matching_opening: _,
                            }
                            | NestingActionType::CommandSeparator
                            | NestingActionType::ArgumentSeparator
                            | NestingActionType::Content => None,
                        });

                let mut covering = InputCoverage::new(len, offset);

                while let Some(uncovered) = covering.next_uncovered() {
                    let matched = find_matching_rule(
                        &input[..offset + len],
                        rules,
                        uncovered,
                        inside,
                        next_nesting,
                        groups,
                    );

                    match matched {
                        Some(matched) => {
                            highlight_piece(&matched, &mut covering, rule_data, &mut output);
                        }

                        None => {
                            // output.push(HighlightedPiece {
                            //     start: uncovered.from,
                            //     len: uncovered.len,
                            //     item: None,
                            // });

                            covering.mark_as_covered(uncovered.from, uncovered.len);
                        }
                    }
                }
            }
        }
    }

    output.sort_by_key(|piece| piece.start);
    output
}

fn find_matching_rule<'h, 'str, T>(
    input: &'str str,
    rules: &'h [Rule<T>],
    range: InputRange,
    inside: Option<(usize, NestingOpeningType)>,
    next_nesting: Option<NestingOpeningType>,
    groups: &'h HashMap<String, Vec<Rule<T>>>,
) -> Option<Match<'h, 'str, T>> {
    rules.iter().find_map(|rule| match rule {
        Rule::Simple(simple) => Match::test(simple, input, range, inside, next_nesting),

        Rule::Group(name) => find_matching_rule(
            input,
            groups.get(name).unwrap(),
            range,
            inside,
            next_nesting,
            groups,
        ),
    })
}

fn highlight_piece<T>(
    matched: &Match<T>,
    covering: &mut InputCoverage,
    rule_data: &T,
    out: &mut Vec<SyntaxItem>,
) {
    // Ensure the highlighted piece is not empty,
    // as this could cause an infinite highlighting
    // (the rule matches an empty piece, then re-matches it, etc.)
    assert!(matched.end > matched.start);

    covering.mark_as_covered(matched.start, matched.end - matched.start);

    let style = match &matched.rule.style {
        RuleStylization::Static(style) => style.clone(),
        RuleStylization::Dynamic(stylizer) => stylizer(
            matched
                ._captured
                .iter()
                .map(|cap| cap.unwrap().as_str().to_owned())
                .collect(),
            rule_data,
        ),
    };

    assert_eq!(
        style.len(),
        matched.rule.matches.static_captures_len().unwrap() - 1
    );

    for (i, style) in style.iter().enumerate() {
        let Some(captured) = matched.get(i + 1) else {
            continue;
        };

        if captured.extract.is_empty() {
            continue;
        }

        // if let Some(prev) = matched.get(i).map(|m| m.start + m.len) {
        //     if captured.start() > prev {
        //         out.push(HighlightedPiece {
        //             start: prev,
        //             len: captured.start() - prev,
        //             item: None,
        //         });
        //     }
        // }

        out.push(SyntaxItem {
            start: captured.start(),
            len: captured.len(),
            item: *style,
        });
    }
}

#[derive(Debug)]
struct Match<'h, 'str, T> {
    nesting_at: usize,
    rule: &'h SimpleRule<T>,
    start: usize,
    end: usize,
    _captured: Captures<'str>,
}

impl<'h, 'str, T> Match<'h, 'str, T> {
    fn test(
        rule: &'h SimpleRule<T>,
        input: &'str str,
        range: InputRange,
        inside: Option<(usize, NestingOpeningType)>,
        next_nesting: Option<NestingOpeningType>,
    ) -> Option<Self> {
        if let Some(must_be_in) = &rule.inside {
            if !must_be_in.contains(&inside?.1) {
                return None;
            }
        }

        let nesting_at = inside.map(|(at, _)| at).unwrap_or(0);

        let (full_match_start, full_match_end, cap) = rule
            .matches
            .captures_at(&input[nesting_at..], range.from - nesting_at)
            .map(|captured| {
                (
                    captured.get(0).unwrap().start() + nesting_at,
                    captured.get(0).unwrap().end() + nesting_at,
                    Self {
                        nesting_at,
                        start: captured.get(1).unwrap().start() + nesting_at,
                        end: captured.get(captured.len() - 1).unwrap().end() + nesting_at,
                        rule,
                        _captured: captured,
                    },
                )
            })?;

        if cap.end > range.from + range.len {
            return None;
        }

        if let Some(preceded_by) = &rule.preceded_by {
            if !preceded_by.is_match(&input[..full_match_start]) {
                return None;
            }
        }

        if let Some(followed_by) = &rule.followed_by {
            let cap = followed_by.captures_at(input, full_match_end)?;

            if cap.get(0).unwrap().start() != full_match_end {
                return None;
            }
        }

        if let Some(followed_by_nesting) = &rule.followed_by_nesting {
            if !followed_by_nesting.contains(&next_nesting?) {
                return None;
            }
        }

        Some(cap)
    }

    fn get(&self, group: usize) -> Option<CapturedGroup<'str>> {
        self._captured.get(group).map(|group| {
            CapturedGroup::new(
                group.as_str(),
                group.start() + self.nesting_at,
                group.len(),
                // group.end() + self.offset,
            )
        })
    }
}

struct CapturedGroup<'str> {
    extract: &'str str,
    start: usize,
    len: usize,
    // end: usize,
}

impl<'str> CapturedGroup<'str> {
    pub fn new(extract: &'str str, start: usize, len: usize /*end: usize*/) -> Self {
        Self {
            extract,
            start,
            len,
            // end,
        }
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn len(&self) -> usize {
        self.len
    }

    // pub fn end(&self) -> usize {
    //     self.end
    // }
}

pub fn validate_rule_set<T>(rule_set: &RuleSet<T>) -> Result<(), String> {
    let RuleSet {
        groups,
        non_nested_content_rules,
        nested_content_rules,
        closing_without_opening_style: _,
        unclosed_style: _,
        command_separator_style: _,
        use_arguments_separator: _,
    } = rule_set;

    fn _validate_simple_rule<T>(rule: &SimpleRule<T>) -> Result<(), String> {
        let SimpleRule {
            matches,
            inside: _,
            preceded_by,
            followed_by: _,
            followed_by_nesting,
            style,
        } = rule;

        if let Some(preceded_by) = &preceded_by {
            if !preceded_by.to_string().ends_with('$') {
                return Err(format!(
                    "Precedence regex '{preceded_by}' should end with a '$'"
                ));
            }
        }

        if followed_by_nesting.is_some() && !matches.to_string().ends_with('$') {
            return Err(
                "Regex that requires a nesting following it should end with a '$'".to_owned(),
            );
        }

        let mut static_len = matches.static_captures_len().ok_or_else(|| {
            format!("Regex '{matches}' does not contain a constant number of capture groups")
        })?;

        // Account the zeroth group (full match)
        static_len -= 1;

        if static_len == 0 {
            return Err(format!("Regex '{matches}' does not have any capture group"));
        }

        match &style {
            RuleStylization::Static(style) => {
                if style.len() != static_len {
                    return Err(format!(
                        "Regex '{matches}' has {static_len} capture group(s) but {} style(s) are associated to it",
                        style.len()
                    ));
                }
            }

            RuleStylization::Dynamic(_) => {}
        }

        Ok(())
    }

    fn _validate_rules<T>(
        rules: &[Rule<T>],
        groups: &HashMap<String, Vec<Rule<T>>>,
    ) -> Result<(), String> {
        for rule in rules {
            match rule {
                Rule::Simple(rule) => _validate_simple_rule(rule)?,

                Rule::Group(group) => {
                    if !groups.contains_key(group.as_str()) {
                        return Err(format!("Unknown group '{group}'"));
                    }
                }
            }
        }

        Ok(())
    }

    _validate_rules(non_nested_content_rules, groups)?;

    let RulesForNesting {
        block,
        list,
        expr_with_paren,
        literal_string,
        computed_string,
        expr_in_string,
        var_spreading,
        cmd_output,
        cmd_call,
        lambda,
        fn_args,
    } = nested_content_rules;

    for rules in [
        block,
        list,
        expr_with_paren,
        literal_string,
        computed_string,
        expr_in_string,
        var_spreading,
        cmd_output,
        cmd_call,
        lambda,
        fn_args,
    ] {
        let NestedContentRules {
            opening_style: _,
            closing_style: _,
            rules,
        } = rules;

        _validate_rules(rules, groups)?;
    }

    for group in groups.values() {
        _validate_rules(group, groups)?;
    }

    Ok(())
}

#[derive(Debug)]
pub struct SyntaxItem {
    pub start: usize,
    pub len: usize,
    pub item: ItemType,
}

impl<T> ValidatedRuleSet<T> {
    pub fn validate(rule_set: RuleSet<T>) -> Result<Self, String> {
        validate_rule_set(&rule_set).map(|()| Self(rule_set))
    }
}

impl<T> Deref for ValidatedRuleSet<T> {
    type Target = RuleSet<T>;

    fn deref(&self) -> &Self::Target {
        let Self(rule_set) = &self;
        rule_set
    }
}
