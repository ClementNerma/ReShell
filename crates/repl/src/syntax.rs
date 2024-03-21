use std::{collections::HashMap, ops::Deref};

use nu_ansi_term::Style;
use regex::{Captures, Regex};

#[derive(Debug)]
pub struct ValidatedRuleSet(RuleSet);

#[derive(Debug)]
pub struct RuleSet {
    pub groups: HashMap<String, Vec<Rule>>,
    pub rules: Vec<Rule>,
}

#[derive(Debug)]
pub enum Rule {
    Simple(SimpleRule),
    Progressive(SimpleRule, Vec<SimpleRule>),
    Nested(NestingRule),
    Group(String),
}

#[derive(Debug)]
pub struct SimpleRule {
    pub matches: Regex,
    pub style: Vec<Style>,
}

#[derive(Debug)]
pub struct NestingRule {
    pub begin: SimpleRule,
    pub end: SimpleRule,
    pub inner_rules: Vec<Rule>,
}

#[derive(Debug)]
pub struct Highlighter<'a> {
    rule_set: &'a ValidatedRuleSet,
}

impl<'a> Highlighter<'a> {
    pub fn new(rule_set: &'a ValidatedRuleSet) -> Self {
        Self { rule_set }
    }

    pub fn highlight(&self, text: &str) -> Vec<HighlightPiece> {
        let mut pieces = vec![];
        self.highlight_inner(text, &self.rule_set.rules, 0, &mut pieces);

        let mut out = Vec::with_capacity(pieces.len());
        let mut prev_end = 0;

        for piece in pieces {
            assert!(piece.start >= prev_end, "non-consecutive piece: {piece:?}");

            assert!(
                piece.start < text.len(),
                "out-of-bound piece start: {} vs {}",
                piece.start,
                text.len(),
            );

            assert!(
                piece.start + piece.len <= text.len(),
                "out-of-bound piece end: {} vs {}",
                piece.start + piece.len,
                text.len()
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

        if prev_end < text.len() {
            out.push(HighlightPiece {
                start: prev_end,
                len: text.len() - prev_end,
                style: None,
            });
        }

        assert_eq!(out.iter().map(|piece| piece.len).sum::<usize>(), text.len());

        out
    }

    fn highlight_inner(
        &self,
        text: &str,
        rules: &[Rule],
        mut shift: usize,
        out: &mut Vec<HighlightPiece>,
    ) {
        if text.is_empty() {
            return;
        }

        while let Some(nesting) = self.find_first_nesting(text, rules, shift) {
            let Nesting {
                begin,
                end,
                inner_rules,
            } = nesting;

            self.highlight_inner(&text[..begin.start], rules, shift, out);

            Self::highlight_piece(&begin, out);

            self.highlight_inner(
                &text[..match end {
                    Some(ref end) => end.start,
                    None => text.len(),
                }],
                inner_rules,
                begin.end,
                out,
            );

            let Some(end) = end else {
                return;
            };

            Self::highlight_piece(&end, out);

            shift = end.end;
        }

        self.highlight_with_rules(text, rules, shift, out);
    }

    fn find_first_nesting<'h, 'str>(
        &'h self,
        text: &'str str,
        rules: &'h [Rule],
        shift: usize,
    ) -> Option<Nesting<'h, 'str>> {
        if text.is_empty() {
            return None;
        }

        let mut min = None::<Nesting>;

        for rule in rules {
            match rule {
                Rule::Simple(_) | Rule::Progressive(_, _) => continue,

                Rule::Nested(rule) => {
                    if let Some(begin_matched) = Match::test(&rule.begin, text, shift) {
                        if min.is_none()
                            || matches!(min, Some(ref nested) if begin_matched.start <  nested.begin.start)
                        {
                            let closing_pat = self.find_matching_closing_pattern(
                                text,
                                &rule.end,
                                &rule.inner_rules,
                                begin_matched.end,
                            );

                            min = Some(Nesting {
                                begin: begin_matched,
                                end: closing_pat,
                                inner_rules: &rule.inner_rules,
                            });
                        }
                    }
                }

                Rule::Group(name) => {
                    let rules = self.rule_set.groups.get(name).unwrap();

                    if let Some(pos) = self.find_first_nesting(text, rules, shift) {
                        return Some(pos);
                    }
                }
            }
        }

        min
    }

    fn find_matching_closing_pattern<'h, 'str>(
        &'h self,
        text: &'str str,
        closing_pat: &'h SimpleRule,
        rules: &'h [Rule],
        mut shift: usize,
    ) -> Option<Match<'h, 'str>> {
        if text.is_empty() {
            return None;
        }

        loop {
            let matched = Match::test(closing_pat, text, shift)?;

            if let Some(nesting) = self.find_first_nesting(text, rules, shift) {
                if matched.start >= nesting.begin.start {
                    shift = matched.end;

                    continue;
                }
            }

            return Some(matched);
        }
    }

    // This function assumes there is no nesting in here
    fn highlight_with_rules(
        &self,
        text: &str,
        rules: &[Rule],
        mut shift: usize,
        out: &mut Vec<HighlightPiece>,
    ) {
        if text.is_empty() {
            return;
        }

        let mut highlight_matched = |matched: &Match, shift: &mut usize| -> bool {
            Self::highlight_piece(matched, out);

            *shift = matched.end;

            *shift < text.len()
        };

        while let Some(matched) = self.find_nearest_simple_or_progressive_rule(text, rules, shift) {
            if !highlight_matched(&matched, &mut shift) {
                return;
            }

            let Some(following) = matched.following else {
                continue;
            };

            for simple in following {
                match Match::test(simple, text, shift) {
                    None => break,
                    Some(matched) => {
                        if !highlight_matched(&matched, &mut shift) {
                            return;
                        }
                    }
                }
            }
        }
    }

    fn find_nearest_simple_or_progressive_rule<'h, 'str>(
        &'h self,
        text: &'str str,
        rules: &'h [Rule],
        shift: usize,
    ) -> Option<Match<'h, 'str>> {
        if text.is_empty() {
            return None;
        }

        let mut min = None::<Match>;

        for rule in rules {
            let matching = match rule {
                Rule::Simple(simple) => Match::test(simple, text, shift),

                Rule::Progressive(simple, following) => Match::test(simple, text, shift)
                    .map(|matched| matched.with_following(following)),

                Rule::Nested(_) => continue,

                Rule::Group(name) => self.find_nearest_simple_or_progressive_rule(
                    text,
                    self.rule_set.groups.get(name).unwrap(),
                    shift,
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
}

#[derive(Debug)]
struct Nesting<'h, 'str> {
    begin: Match<'h, 'str>,
    end: Option<Match<'h, 'str>>,
    inner_rules: &'h [Rule],
}

#[derive(Debug)]
struct Match<'h, 'str> {
    // input: &'str str,
    offset: usize,
    rule: &'h SimpleRule,
    start: usize,
    end: usize,
    following: Option<&'h [SimpleRule]>,
    _captured: Captures<'str>,
}

impl<'h, 'str> Match<'h, 'str> {
    fn test(rule: &'h SimpleRule, input: &'str str, offset: usize) -> Option<Self> {
        rule.matches
            .captures(&input[offset..])
            .map(|captured| Self {
                // input,
                offset,
                start: captured.get(1).unwrap().start() + offset,
                end: captured.get(captured.len() - 1).unwrap().end() + offset,
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
                group.start() + self.offset,
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
    let RuleSet { groups, rules } = rule_set;

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

                Rule::Nested(NestingRule {
                    begin,
                    end,
                    inner_rules,
                }) => {
                    _validate_simple_rule(begin)?;
                    _validate_simple_rule(end)?;

                    _validate_rules(inner_rules, groups)?;
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

    _validate_rules(rules, groups)?;

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
