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
        let mut out = vec![];

        self.highlight_inner(text, &self.rule_set.rules, 0, &mut out);

        if cfg!(debug_assertions) {
            let mut last_piece_end = 0;

            for piece in &out {
                let HighlightPiece {
                    start,
                    len,
                    style: _,
                } = piece;

                assert_eq!(*start, last_piece_end, "pieces are not consecutive");

                last_piece_end += len;
            }

            assert_eq!(
                last_piece_end,
                text.len(),
                "pieces don't cover the whole input"
            );
        }

        out
    }

    fn highlight_inner(
        &self,
        text: &str,
        rules: &[Rule],
        curr_shift: usize,
        out: &mut Vec<HighlightPiece>,
    ) {
        let mut inner_shift = 0;

        while let Some(nesting) = self.find_first_nesting(&text[inner_shift..], rules) {
            let Nesting {
                start,
                end,
                inner_rules,
            } = nesting;

            if !text[inner_shift..=inner_shift + start].is_empty() {
                self.highlight_inner(
                    &text[inner_shift..=inner_shift + start],
                    rules,
                    curr_shift + inner_shift,
                    out,
                );
            }

            self.highlight_inner(
                &text[inner_shift + start..=inner_shift + end.unwrap_or(text.len() - 1)],
                inner_rules,
                curr_shift + inner_shift + start,
                out,
            );

            let Some(end) = end else {
                return;
            };

            inner_shift += end + text[inner_shift..].chars().next().unwrap().len_utf8();
        }

        self.highlight_with_rules(&text[inner_shift..], rules, curr_shift + inner_shift, out);
    }

    fn find_first_nesting<'a>(&'a self, text: &str, rules: &'a [Rule]) -> Option<Nesting<'a>> {
        if text.is_empty() {
            return None;
        }

        let mut min = None::<Nesting>;

        for rule in rules {
            match rule {
                Rule::Simple(_) => continue,

                Rule::Nested(rule) => {
                    if let Some(matched) = rule.begin.matches.find(text) {
                        if min.is_none()
                            || matches!(min, Some(ref nested) if matched.start() < nested.start)
                        {
                            min = Some(Nesting {
                                start: matched.start(),
                                end: self.find_matching_closing_pattern(
                                    text,
                                    &rule.end,
                                    &rule.inner_rules,
                                ),
                                inner_rules: &rule.inner_rules,
                            });
                        }
                    }
                }

                Rule::Group(name) => {
                    let rules = self.rule_set.groups.get(name).unwrap();

                    if let Some(pos) = self.find_first_nesting(text, rules) {
                        return Some(pos);
                    }
                }
            }
        }

        min
    }

    fn find_matching_closing_pattern(
        &self,
        mut text: &str,
        closing_pat: &SimpleRule,
        rules: &[Rule],
    ) -> Option<usize> {
        let mut shift = 0;

        loop {
            let pos = closing_pat.matches.find(text)?.start();

            match self.find_first_nesting(text, rules) {
                Some(nesting) => {
                    if pos < nesting.start {
                        return Some(pos + shift);
                    } else {
                        text = &text[pos..];
                        shift += pos;
                    }
                }

                None => return Some(pos + shift),
            }
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
        let mut inner_shift = 0;
        let mut untouched = 0;

        while let Some((rule, captured)) =
            self.find_nearest_simple_rule(&text[inner_shift..], rules)
        {
            let SimpleRule { matches: _, style } = rule;

            for (i, style) in style.iter().enumerate() {
                let captured = captured.get(i + 1).unwrap();

                if !text[untouched..inner_shift + captured.start()].is_empty() {
                    out.push(HighlightPiece {
                        start: curr_shift + untouched,
                        len: inner_shift + captured.start() - untouched,
                        style: None,
                    });
                }

                out.push(HighlightPiece {
                    start: curr_shift + inner_shift + captured.start(),
                    len: captured.len(),
                    style: Some(*style),
                });

                untouched = inner_shift + captured.start() + captured.len();
            }

            let full_match = captured.get(0).unwrap();

            inner_shift += full_match.start() + full_match.len();
        }

        if !text[untouched..].is_empty() {
            out.push(HighlightPiece {
                start: curr_shift + untouched,
                len: text.len() - untouched,
                style: None,
            });
        }
    }

    fn find_nearest_simple_rule<'a, 'b>(
        &'a self,
        text: &'b str,
        rules: &'a [Rule],
    ) -> Option<(&'a SimpleRule, Captures<'b>)> {
        if text.is_empty() {
            return None;
        }

        for rule in rules {
            match rule {
                Rule::Simple(simple) => {
                    let SimpleRule { matches, style: _ } = simple;

                    if let Some(captured) = matches.captures(text) {
                        return Some((simple, captured));
                    }
                }

                Rule::Nested(_) => unreachable!(),

                Rule::Group(name) => {
                    if let Some(ret) =
                        self.find_nearest_simple_rule(text, self.rule_set.groups.get(name).unwrap())
                    {
                        return Some(ret);
                    }
                }
            }
        }

        None
    }
}

struct Nesting<'a> {
    start: usize,
    end: Option<usize>,
    inner_rules: &'a [Rule],
}

#[derive(Debug)]
pub struct HighlightPiece {
    pub start: usize,
    pub len: usize,
    pub style: Option<Style>,
}
