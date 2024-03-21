pub fn handle_nesting(input: &str) -> Vec<(char, usize, NestingCharAction)> {
    let mut opened: Vec<(char, usize)> = vec![];
    let mut opened_strings: Vec<(char, usize)> = vec![];
    let mut output: Vec<(char, usize, NestingCharAction)> = vec![];
    let mut escaping: Option<(char, usize)> = None;

    let mut offset = 0;
    let mut last_char: Option<(char, usize)> = None;
    let mut prev_char: Option<(char, usize)> = None;

    for char in input.chars() {
        if let Some((last_char, last_offset)) = last_char {
            offset += last_char.len_utf8();
            prev_char = Some((last_char, last_offset));
        }

        last_char = Some((char, offset));

        if let Some((escaping_char, prev_offset)) = escaping.take() {
            output.push((escaping_char, prev_offset, NestingCharAction::Escaping));
            output.push((char, offset, NestingCharAction::Escaping));

            continue;
        }

        match char {
            '\\' => match opened.last() {
                Some(('"', _)) => {
                    escaping = Some((char, offset));
                    output.push((char, offset, NestingCharAction::Escaping));
                }

                _ => output.push((char, offset, NestingCharAction::InvalidEscape)),
            },

            '(' | '[' | '{' => {
                if matches!(opened.last().copied(), Some(('"', _))) {
                    match prev_char {
                        Some(('$', dollar_offset)) => {
                            output.push(('$', dollar_offset, NestingCharAction::OpeningPrefix))
                        }

                        _ => continue,
                    }
                }

                opened.push((char, offset));
            }

            ')' | ']' | '}' => {
                if let Some((opening_char, opening_offset)) = opened.last().copied() {
                    if matches!((opening_char, char), ('(', ')') | ('[', ']') | ('{', '}')) {
                        opened.pop();
                        output.push((opening_char, opening_offset, NestingCharAction::Opening));
                        output.push((char, offset, NestingCharAction::Closing));

                        continue;
                    }
                }

                if !matches!(opened.last(), Some(('"', _))) {
                    output.push((char, offset, NestingCharAction::ClosingWithoutOpening));
                }
            }

            '"' => {
                if let Some((opening_char, opening_offset)) = opened_strings.last().copied() {
                    if opened.last().copied() == Some((opening_char, opening_offset)) {
                        opened.pop();
                        opened_strings.pop();
                        output.push((opening_char, opening_offset, NestingCharAction::Opening));
                        output.push((char, offset, NestingCharAction::Closing));

                        continue;
                    }
                }

                opened.push((char, offset));
                opened_strings.push((char, offset));
            }

            _ => {}
        }
    }

    for (char, offset) in opened {
        output.push((char, offset, NestingCharAction::Unclosed));
    }

    if let Some((char, offset)) = escaping {
        output.push((char, offset, NestingCharAction::InvalidEscape));
    }

    output
}

#[derive(PartialEq, Eq)]
pub enum NestingCharAction {
    OpeningPrefix,
    Opening,
    Closing,
    Unclosed,
    ClosingWithoutOpening,
    Escaping,
    InvalidEscape,
}
