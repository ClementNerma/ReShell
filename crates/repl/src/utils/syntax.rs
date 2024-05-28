///
/// Syntax highlighting for ReShell's scripting language
///
/// Regular expressions work as usual, except for the `^` and `$` symbol which are delimited differently
/// `^` refers to the nearest nested rule opening, or to the beginning of input if nesting level is zero
///
use std::{collections::HashMap, ops::Deref};

use nu_ansi_term::Style;
use regex::{Captures, Regex};

use crate::utils::nesting::{NestingAction, NestingActionType};

use super::nesting::{detect_nesting_actions, NestingOpeningType};

#[derive(Debug)]
pub struct ValidatedRuleSet(RuleSet);

#[derive(Debug)]
pub struct RuleSet {
    pub groups: HashMap<String, Vec<Rule>>,
    pub non_nested_content_rules: Vec<Rule>,
    pub nested_content_rules: HashMap<NestingOpeningType, NestedContentRules>,
    pub closing_without_opening_style: Style,
    pub unclosed_style: Style,
}

#[derive(Debug)]
pub struct NestedContentRules {
    pub opening_style: Style,
    pub closing_style: Style,
    pub rules: Vec<Rule>,
}

#[derive(Debug)]
pub enum Rule {
    Simple(SimpleRule),
    Progressive(SimpleRule, Vec<SimpleRule>),
    Group(String),
}

#[derive(Debug)]
pub struct SimpleRule {
    pub matches: Regex,
    pub style: Vec<Style>,
}

pub fn compute_highlight_pieces(input: &str, rule_set: &ValidatedRuleSet) -> Vec<HighlightPiece> {
    let ValidatedRuleSet(rule_set) = rule_set;

    let RuleSet {
        groups,
        non_nested_content_rules,
        nested_content_rules,
        closing_without_opening_style,
        unclosed_style,
    } = rule_set;

    let mut output = Vec::<HighlightPiece>::new();

    let nesting = detect_nesting_actions(input);

    let mut opened = Vec::<(NestingAction, NestingOpeningType)>::new();

    for action in nesting {
        let NestingAction {
            offset,
            len,
            action_type,
        } = action;

        match action_type {
            NestingActionType::Opening(typ) => {
                opened.push((action, typ));

                output.push(HighlightPiece {
                    start: offset,
                    len,
                    style: Some(nested_content_rules.get(&typ).unwrap().opening_style),
                });
            }

            NestingActionType::Closing { opening_offset } => {
                let (closing, opening_type) = opened.pop().unwrap();
                assert!(closing.offset == opening_offset);

                output.push(HighlightPiece {
                    start: offset,
                    len,
                    style: Some(
                        nested_content_rules
                            .get(&opening_type)
                            .unwrap()
                            .closing_style,
                    ),
                });
            }

            NestingActionType::Unclosed(typ) => {
                opened.push((action, typ));

                output.push(HighlightPiece {
                    start: offset,
                    len,
                    style: Some(*unclosed_style),
                });
            }

            NestingActionType::ClosingWithoutOpening => output.push(HighlightPiece {
                start: offset,
                len,
                style: Some(*closing_without_opening_style),
            }),

            NestingActionType::Content => {
                let rules = match opened.last() {
                    Some((_, opening_type)) => {
                        &nested_content_rules.get(opening_type).unwrap().rules
                    }

                    None => non_nested_content_rules,
                };

                let mut shift = offset;
                let nesting_at = opened.last().map(|(action, _)| action.offset + action.len);

                let mut highlight_matched = |matched: &Match, shift: &mut usize| -> bool {
                    highlight_piece(matched, &mut output);
                    *shift = matched.end;
                    *shift < input.len()
                };

                'outer: while let Some(matched) = find_nearest_simple_or_progressive_rule(
                    &input[..offset + len],
                    rules,
                    shift,
                    nesting_at,
                    groups,
                ) {
                    if !highlight_matched(&matched, &mut shift) {
                        break;
                    }

                    let Some(following) = matched.following else {
                        continue;
                    };

                    for simple in following {
                        match Match::test(simple, &input[..offset + len], shift, nesting_at) {
                            None => break,
                            Some(matched) => {
                                if !highlight_matched(&matched, &mut shift) {
                                    break 'outer;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    validate_output(input, output)
}

fn validate_output(input: &str, pieces: Vec<HighlightPiece>) -> Vec<HighlightPiece> {
    let mut out = vec![];
    let mut prev_end = 0;

    for piece in pieces {
        assert!(piece.start >= prev_end, "non-consecutive piece: {piece:?}");

        assert!(
            piece.start < input.len(),
            "out-of-bound piece start: {} vs {}",
            piece.start,
            input.len(),
        );

        assert!(
            piece.start + piece.len <= input.len(),
            "out-of-bound piece end: {} vs {}",
            piece.start + piece.len,
            input.len()
        );

        if prev_end < piece.start {
            out.push(HighlightPiece {
                start: prev_end,
                len: piece.start - prev_end,
                style: None,
            });
        }

        prev_end = piece.start + piece.len;
        out.push(piece);
    }

    if prev_end < input.len() {
        out.push(HighlightPiece {
            start: prev_end,
            len: input.len() - prev_end,
            style: None,
        });
    }

    assert_eq!(
        out.iter().map(|piece| piece.len).sum::<usize>(),
        input.len()
    );

    out
}

fn find_nearest_simple_or_progressive_rule<'h, 'str>(
    input: &'str str,
    rules: &'h [Rule],
    shift: usize,
    nesting_at: Option<usize>,
    groups: &'h HashMap<String, Vec<Rule>>,
) -> Option<Match<'h, 'str>> {
    let mut min = None::<Match>;

    for rule in rules {
        let matching = match rule {
            Rule::Simple(simple) => Match::test(simple, input, shift, nesting_at),

            Rule::Progressive(simple, following) => Match::test(simple, input, shift, nesting_at)
                .map(|matched| matched.with_following(following)),

            Rule::Group(name) => find_nearest_simple_or_progressive_rule(
                input,
                groups.get(name).unwrap(),
                shift,
                nesting_at,
                groups,
            ),
        };

        if let Some(matched) = matching {
            if min.is_none() || matches!(min, Some(ref min) if matched.start < min.start) {
                min = Some(matched);
            }
        }
    }

    min
}

fn highlight_piece(matched: &Match, out: &mut Vec<HighlightPiece>) {
    let SimpleRule { matches: _, style } = matched.rule;

    for (i, style) in style.iter().enumerate() {
        let Some(captured) = matched.get(i + 1) else {
            continue;
        };

        if captured.extract.is_empty() {
            continue;
        }

        out.push(HighlightPiece {
            start: captured.start(),
            len: captured.len(),
            style: Some(*style),
        });
    }
}

#[derive(Debug)]
struct Match<'h, 'str> {
    nesting_at: usize,
    rule: &'h SimpleRule,
    start: usize,
    end: usize,
    following: Option<&'h [SimpleRule]>,
    _captured: Captures<'str>,
}

impl<'h, 'str> Match<'h, 'str> {
    fn test(
        rule: &'h SimpleRule,
        input: &'str str,
        offset: usize,
        nesting_at: Option<usize>,
    ) -> Option<Self> {
        let nesting_at = nesting_at.unwrap_or(0);

        rule.matches
            .captures_at(&input[nesting_at..], offset - nesting_at)
            .map(|captured| Self {
                nesting_at,
                start: captured.get(1).unwrap().start() + nesting_at,
                end: captured.get(captured.len() - 1).unwrap().end() + nesting_at,
                rule,
                _captured: captured,
                following: None,
            })
    }

    fn with_following(mut self, following: &'h [SimpleRule]) -> Self {
        self.following = Some(following);
        self
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

pub fn validate_rule_set(rule_set: &RuleSet) -> Result<(), String> {
    let RuleSet {
        groups,
        non_nested_content_rules,
        nested_content_rules,
        closing_without_opening_style: _,
        unclosed_style: _,
    } = rule_set;

    fn _validate_simple_rule(rule: &SimpleRule) -> Result<(), String> {
        let SimpleRule { matches, style } = rule;

        match matches.static_captures_len() {
            None => Err(format!(
                "Regex '{matches}' does not contain a constant number of capture groups"
            )),

            Some(mut static_len) => {
                // Account the zeroth group (full match)
                static_len -= 1;

                if static_len == 0 {
                    Err(format!("Regex '{matches}' does not have any capture group"))
                } else if style.len() != static_len {
                    Err(format!("Regex '{matches}' has {static_len} capture group(s) but {} style(s) are associated to it", style.len()))
                } else {
                    Ok(())
                }
            }
        }
    }

    fn _validate_rules(rules: &[Rule], groups: &HashMap<String, Vec<Rule>>) -> Result<(), String> {
        for rule in rules {
            match rule {
                Rule::Simple(rule) => _validate_simple_rule(rule)?,

                Rule::Progressive(rule, following) => {
                    _validate_simple_rule(rule)?;

                    for rule in following {
                        _validate_simple_rule(rule)?;
                    }
                }

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

    for rules in nested_content_rules.values() {
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
pub struct HighlightPiece {
    pub start: usize,
    pub len: usize,
    pub style: Option<Style>,
}

impl ValidatedRuleSet {
    pub fn validate(rule_set: RuleSet) -> Result<Self, String> {
        validate_rule_set(&rule_set).map(|()| Self(rule_set))
    }
}

impl Deref for ValidatedRuleSet {
    type Target = RuleSet;

    fn deref(&self) -> &Self::Target {
        let Self(rule_set) = &self;
        rule_set
    }
}
