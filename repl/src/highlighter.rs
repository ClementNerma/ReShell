use nu_ansi_term::{Color, Style};
use reedline::{Highlighter as RlHighlighter, StyledText};
use regex::Regex;

use crate::highlighting::{Highlighted, SyntaxHighlighter};

pub fn create_highlighter() -> Box<dyn RlHighlighter> {
    Box::new(Highlighter)
}

pub struct Highlighter;

impl RlHighlighter for Highlighter {
    fn highlight(&self, line: &str, _cursor: usize) -> StyledText {
        highlight(line)
    }
}

fn highlight(input: &str) -> StyledText {
    let mut h = SyntaxHighlighter::new(input);

    highlight_strings(&mut h);

    macro_rules! highlight {
            ($(($category: expr) => $regex: expr => $color: ident),+) => {
                $(
                    // $category
                    h.regex(
                        Regex::new($regex).unwrap(),
                        &[Style::new().fg(Color::$color)],
                    );
                )+
            }
        }

    highlight!(
        ("flags") => "\\s(\\-\\-[a-zA-Z_][a-zA-Z0-9_]+|\\-[a-zA-Z])\\b" => Yellow,
        ("keywords") => "\\b(let|mut|if|else|for|in|while|continue|break|fn|return|throw|alias|type|do)\\b" => Magenta,
        ("types") => "\\b(any|bool|int|float|string|list|map|error|struct|fn)\\b" => Magenta,
        ("booleans") => "\\b(true|false)\\b" => LightYellow,
        ("null value") => "\\b(null)\\b" => LightYellow,
        ("variables") => "(\\$[a-zA-Z_][a-zA-Z0-9_]*)\\b" => Red,
        ("variables declaration") => "\\blet\\s+(?:mut\\s+)?([a-zA-Z_][a-zA-Z0-9_]*)\\b" => Red,
        ("loop iterator") => "(?:^|\\n|\\s)for\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\b" => Red,
        ("function parameters and struct fields") => "\\b([a-zA-Z_][a-zA-Z0-9_]*)\\s*:" => Red,
        ("numbers") => "\\b(\\d+(?:\\.\\d+)?)\\b" => LightYellow,
        ("command") => "(?:^|[\\(\\{;])\\s*([a-zA-Z0-9_/\\-\\.]+)\\b" => LightBlue,
        ("function calls") => "\\b([a-zA-Z_][a-zA-Z0-9_]*)\\(" => LightBlue,
        ("raw arguments") => "([^\\s\\(\\)\\[\\]\\{\\};=!<>\\?]+)" => Green,
        ("symbols and operators") => "([\\(\\)\\{\\}\\[\\],;=!<>\\?\\+\\-])" => DarkGray
    );

    h.finalize(Style::default())
}

fn highlight_strings(h: &mut SyntaxHighlighter) {
    let input = h.input();

    let mut next_offset = 0;
    let mut opened_string = None;
    let mut escaping = false;

    let chars = input.chars().collect::<Vec<_>>();
    let mut i = 0;

    while i < chars.len() {
        let c = chars[i];

        let offset = next_offset;
        next_offset += c.len_utf8();

        match opened_string {
            None => {
                if c == '"' {
                    opened_string = Some(offset);
                }
            }

            Some(string_started_at) => {
                if escaping {
                    h.range(
                        string_started_at,
                        offset - string_started_at - '\\'.len_utf8(),
                        Style::new().fg(Color::Green),
                    );

                    h.range(
                        offset - '\\'.len_utf8(),
                        '\\'.len_utf8() + c.len_utf8(),
                        Style::new().fg(Color::Cyan),
                    );

                    escaping = false;
                    opened_string = Some(next_offset);
                } else if c == '\\' {
                    escaping = true;
                } else if c == '"' {
                    h.range(
                        string_started_at,
                        next_offset - string_started_at,
                        Style::new().fg(Color::Green),
                    );

                    opened_string = None;
                } else if c == '$' && chars.len() > i + 1 {
                    // don't highlight: $... and $(...)
                    // if c == '$' and not followed by letters => just a normal char
                    // if c == '$' and followed by letters (or underscore) => variable name
                    // if c == '$' and followed by opening paren => expression

                    if chars[i + 1].is_alphabetic() || chars[i + 1] == '_' {
                        h.range(
                            string_started_at,
                            offset - string_started_at,
                            Style::new().fg(Color::Green),
                        );

                        let incomplete = loop {
                            if chars.len() == i + 1 {
                                break true;
                            }

                            if !chars[i + 1].is_alphabetic()
                                && !chars[i + 1].is_alphanumeric()
                                && chars[i + 1] != '_'
                            {
                                break false;
                            }

                            i += 1;
                            next_offset += c.len_utf8();
                        };

                        opened_string = if !incomplete { Some(next_offset) } else { None };
                    } else if chars.get(i + 1) == Some(&'(') {
                        h.range(
                            string_started_at,
                            offset - string_started_at,
                            Style::new().fg(Color::Green),
                        );

                        h.range(
                            offset,
                            c.len_utf8() + '('.len_utf8(),
                            Style::new().fg(Color::Red),
                        );

                        let mut nested = vec![
                            // This will be filled with '(' as it's the next character we're going to encounter
                        ];

                        let incomplete = loop {
                            if chars.len() <= i + 1 {
                                break true;
                            }

                            let c = chars[i + 1];

                            if nested.last() == Some(&c) {
                                nested.pop();

                                if nested.is_empty() {
                                    break false;
                                }
                            } else if c == '\\' && nested.last() == Some(&'"') {
                                if chars.len() == i + 2 {
                                    break true;
                                }

                                i += 1;
                                next_offset += chars[i + 2].len_utf8();
                            } else if c == '(' {
                                nested.push(')');
                            } else if c == '[' {
                                nested.push(']');
                            } else if c == '{' {
                                nested.push('}');
                            } else if c == '"' {
                                nested.push('"');
                            }

                            i += 1;
                            next_offset += c.len_utf8();
                        };

                        let mut sub_h = SyntaxHighlighter::new(
                            &input[offset..std::cmp::min(next_offset, input.len())],
                        );

                        highlight_strings(&mut sub_h);

                        for Highlighted { start, len, style } in sub_h.highlighted() {
                            h.range(start + offset, *len, *style);
                        }

                        if !incomplete {
                            h.range(next_offset, ')'.len_utf8(), Style::new().fg(Color::Red));
                        }

                        opened_string = if !incomplete {
                            Some(next_offset + ')'.len_utf8())
                        } else {
                            None
                        };
                    }
                }
            }
        }

        i += 1;
    }

    if let Some(string_started_at) = opened_string {
        h.range(
            string_started_at,
            input.len() - string_started_at,
            Style::new().fg(Color::Green),
        );
    }
}
