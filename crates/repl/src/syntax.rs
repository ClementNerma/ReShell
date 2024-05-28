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
                text.len()
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
        curr_shift: usize,
        out: &mut Vec<HighlightPiece>,
    ) {
        if text.is_empty() {
            return;
        }

        let mut inner_shift = 0;

        while let Some(nesting) =
            self.find_first_nesting(&text[inner_shift..], rules, curr_shift + inner_shift)
        {
            let Nesting {
                begin,
                end,
                inner_rules,
            } = nesting;

            self.highlight_inner(
                &text[inner_shift..begin.start],
                rules,
                curr_shift + inner_shift,
                out,
            );

            Self::highlight_piece(&begin, out);

            self.highlight_inner(
                &text[begin.end..match end {
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

            inner_shift = end.end;
        }

        self.highlight_with_rules(&text[inner_shift..], rules, curr_shift + inner_shift, out);
    }

    fn find_first_nesting<'h, 'str>(
        &'h self,
        text: &'str str,
        rules: &'h [Rule],
        curr_shift: usize,
    ) -> Option<Nesting<'h, 'str>> {
        if text.is_empty() {
            return None;
        }

        let mut min = None::<Nesting>;

        for rule in rules {
            match rule {
                Rule::Simple(_) => continue,

                Rule::Nested(rule) => {
                    if let Some(captured) = rule.begin.matches.captures(text) {
                        let start = captured.get(0).unwrap().start();
                        let end = captured.get(0).unwrap().end();

                        if min.is_none()
                            || matches!(min, Some(ref nested) if start <  nested.begin.start)
                        {
                            let closing_pat = self.find_matching_closing_pattern(
                                &text[end..],
                                &rule.end,
                                &rule.inner_rules,
                                curr_shift + end,
                            );

                            min = Some(Nesting {
                                begin: Match::new(&rule.begin, captured, curr_shift),
                                end: closing_pat,
                                inner_rules: &rule.inner_rules,
                            });
                        }
                    }
                }

                Rule::Group(name) => {
                    let rules = self.rule_set.groups.get(name).unwrap();

                    if let Some(pos) = self.find_first_nesting(text, rules, curr_shift) {
                        return Some(pos);
                    }
                }
            }
        }

        min
    }

    fn find_matching_closing_pattern<'h, 'str>(
        &'h self,
        mut text: &'str str,
        closing_pat: &'h SimpleRule,
        rules: &'h [Rule],
        curr_shift: usize,
    ) -> Option<Match<'h, 'str>> {
        if text.is_empty() {
            return None;
        }

        let mut inner_shift = 0;

        loop {
            let captured = closing_pat.matches.captures(text)?;

            let start = captured.get(0).unwrap().start();
            let end = captured.get(0).unwrap().end();

            if let Some(nesting) =
                self.find_first_nesting(&text[inner_shift..], rules, curr_shift + inner_shift)
            {
                if start >= nesting.begin.start {
                    text = &text[start..];
                    inner_shift += end;

                    continue;
                }
            }

            return Some(Match::new(closing_pat, captured, curr_shift + inner_shift));
        }
    }

    // This function assumes there is no nesting in here
    fn highlight_with_rules(
        &self,
        text: &str,
        rules: &[Rule],
        curr_shift: usize,
        out: &mut Vec<HighlightPiece>,
    ) {
        if text.is_empty() {
            return;
        }

        let mut inner_shift = 0;

        while let Some(matched) =
            self.find_nearest_simple_rule(&text[inner_shift..], rules, curr_shift + inner_shift)
        {
            Self::highlight_piece(&matched, out);

            inner_shift += matched.capture_end;

            if inner_shift > text.len() {
                return;
            }
        }
    }

    fn find_nearest_simple_rule<'h, 'str>(
        &'h self,
        text: &'str str,
        rules: &'h [Rule],
        curr_shift: usize,
    ) -> Option<Match<'h, 'str>> {
        if text.is_empty() {
            return None;
        }

        for rule in rules {
            match rule {
                Rule::Simple(simple) => {
                    let SimpleRule { matches, style: _ } = simple;

                    if let Some(captured) = matches.captures(text) {
                        return Some(Match::new(simple, captured, curr_shift));
                    }
                }

                Rule::Nested(_) => continue,

                Rule::Group(name) => {
                    let ret = self.find_nearest_simple_rule(
                        text,
                        self.rule_set.groups.get(name).unwrap(),
                        curr_shift,
                    );

                    if ret.is_some() {
                        return ret;
                    }
                }
            }
        }

        None
    }

    fn highlight_piece(matched: &Match, out: &mut Vec<HighlightPiece>) {
        let Match {
            rule,
            captured,
            start,
            ..
        } = matched;

        let SimpleRule { matches: _, style } = rule;

        let cap_start = captured.get(0).unwrap().start();

        for (i, style) in style.iter().enumerate() {
            let Some(captured) = captured.get(i + 1) else {
                continue;
            };

            if captured.is_empty() {
                continue;
            }

            out.push(HighlightPiece {
                start: start + captured.start() - cap_start,
                len: captured.len(),
                style: Some(*style),
            });
        }
    }
}

struct Nesting<'h, 'str> {
    begin: Match<'h, 'str>,
    end: Option<Match<'h, 'str>>,
    inner_rules: &'h [Rule],
}

#[derive(Debug)]
struct Match<'h, 'str> {
    rule: &'h SimpleRule,
    captured: Captures<'str>,
    start: usize,
    end: usize,
    // capture_start: usize,
    capture_end: usize,
}

impl<'h, 'str> Match<'h, 'str> {
    fn new(rule: &'h SimpleRule, captured: Captures<'str>, curr_shift: usize) -> Self {
        let capture_start = captured.get(0).unwrap().start();
        let capture_end = captured.get(0).unwrap().end();

        Self {
            start: capture_start + curr_shift,
            end: capture_end + curr_shift,
            // capture_start,
            capture_end,
            rule,
            captured,
        }
    }
}

#[derive(Debug)]
pub struct HighlightPiece {
    pub start: usize,
    pub len: usize,
    pub style: Option<Style>,
}
