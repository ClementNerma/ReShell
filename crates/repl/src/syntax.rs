use std::collections::HashMap;

use nu_ansi_term::Style;
use regex::{Captures, Regex};

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
pub struct Highlighter {
    rule_set: RuleSet,
}

impl Highlighter {
    pub fn new(rule_set: RuleSet) -> Self {
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
                    if let Some(captured) = rule.begin.matches.captures_at(text, shift) {
                        let begin_matched = Match::new(&rule.begin, captured);

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
            let captured = closing_pat.matches.captures_at(text, shift)?;

            let start = captured.get(0).unwrap().start();
            let end = captured.get(0).unwrap().end();

            if let Some(nesting) = self.find_first_nesting(text, rules, shift) {
                if start >= nesting.begin.start {
                    shift = end;

                    continue;
                }
            }

            return Some(Match::new(closing_pat, captured));
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
                match Self::match_simple_rule(simple, text, shift) {
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
            match rule {
                Rule::Simple(simple) => {
                    if let Some(matched) = Self::match_simple_rule(simple, text, shift) {
                        if min.is_none()
                            || matches!(min, Some(ref min) if matched.start < min.start)
                        {
                            min = Some(matched);
                        }
                    }
                }

                Rule::Progressive(simple, following) => {
                    if let Some(matched) = Self::match_simple_rule(simple, text, shift) {
                        if min.is_none()
                            || matches!(min, Some(ref min) if matched.start < min.start)
                        {
                            min = Some(matched.with_following(following));
                        }
                    }
                }

                Rule::Nested(_) => continue,

                Rule::Group(name) => {
                    let matched = self.find_nearest_simple_or_progressive_rule(
                        text,
                        self.rule_set.groups.get(name).unwrap(),
                        shift,
                    );

                    if let Some(matched) = matched {
                        if min.is_none()
                            || matches!(min, Some(ref min) if matched.start < min.start)
                        {
                            min = Some(matched);
                        }
                    }
                }
            }
        }

        min
    }

    fn highlight_piece(matched: &Match, out: &mut Vec<HighlightPiece>) {
        let Match { rule, captured, .. } = matched;

        let SimpleRule { matches: _, style } = rule;

        for (i, style) in style.iter().enumerate() {
            let Some(captured) = captured.get(i + 1) else {
                continue;
            };

            if captured.is_empty() {
                continue;
            }

            out.push(HighlightPiece {
                start: captured.start(),
                len: captured.len(),
                style: Some(*style),
            });
        }
    }

    fn match_simple_rule<'h, 'str>(
        simple: &'h SimpleRule,
        text: &'str str,
        shift: usize,
    ) -> Option<Match<'h, 'str>> {
        let SimpleRule { matches, style: _ } = simple;

        matches
            .captures_at(text, shift)
            .map(|captured| Match::new(simple, captured))
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
    rule: &'h SimpleRule,
    start: usize,
    end: usize,
    captured: Captures<'str>,
    following: Option<&'h [SimpleRule]>,
}

impl<'h, 'str> Match<'h, 'str> {
    fn new(rule: &'h SimpleRule, captured: Captures<'str>) -> Self {
        Self {
            start: captured.get(1).unwrap().start(),
            end: captured.get(captured.len() - 1).unwrap().end(),
            rule,
            captured,
            following: None,
        }
    }

    fn with_following(mut self, following: &'h [SimpleRule]) -> Self {
        self.following = Some(following);
        self
    }
}

#[derive(Debug)]
pub struct HighlightPiece {
    pub start: usize,
    pub len: usize,
    pub style: Option<Style>,
}
