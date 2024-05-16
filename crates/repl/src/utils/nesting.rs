//!
//! Nesting computation module.
//!
//! This is an advanced and pretty complex module.
//!
//! Basically it takes an input (prompt) and produces a list of nesting actions.
//!
//! An action can for instance be the opening or closing of a string, the opening of a parenthesis,
//! the presence of a command separation marker, etc. These can be found in the [`NestingActionType`] enum.
//!
//! These informations are then used for checking if a line is complete or not
//!     (cf. [`super::super::validator`])
//! They are also used to extract informations from a command prompt
//!     (cf. [`super::cmd_pieces`])
//!

#[derive(Debug)]
pub struct NestingDetectionResult {
    pub actions: Vec<NestingAction>,

    /// Final nesting level (starting at 0)
    /// e.g. `cmd arg1 $(arg2 $(arg3` would result in a final level of 2
    pub final_nesting_level: usize,
}

pub fn detect_nesting_actions(input: &str, insert_args_separator: bool) -> NestingDetectionResult {
    let mut opened: Vec<(&str, usize)> = vec![];
    let mut output: Vec<NestingAction> = vec![];

    macro_rules! register_content_until {
        ($offset: expr) => {{
            let from = output
                .last()
                .map(|action| action.offset + action.len)
                .unwrap_or(0);

            if from != $offset {
                output.push(NestingAction {
                    action_type: NestingActionType::Content,
                    offset: from,
                    len: $offset - from,
                    nesting_level: opened.len(),
                });
            }
        }};
    }

    macro_rules! push {
        ($offset: expr, $len: expr, $action_type: expr) => {{
            if !matches!($action_type, NestingActionType::ArgumentSeparator)
                || insert_args_separator
            {
                register_content_until!($offset);

                output.push(NestingAction {
                    offset: $offset,
                    len: $len,
                    action_type: $action_type,
                    nesting_level: opened.len(),
                });
            }
        }};
    }

    macro_rules! open {
        ($offset: expr, $opening_str: expr) => {{
            register_content_until!($offset);

            opened.push(($opening_str, $offset));

            output.push(NestingAction {
                offset: $offset,
                len: $opening_str.len(),
                action_type: NestingActionType::Opening {
                    typ: NestingOpeningType::try_from_str($opening_str).unwrap(),
                    matching_close: false,
                },
                nesting_level: opened.len() + 1,
            });
        }};
    }

    let mut opened_strings: Vec<(&str, usize)> = vec![];
    let mut closed = vec![];
    let mut escaping = false;
    let mut commenting = false;

    let mut offset = 0;
    let mut last_char: Option<(&str, usize)> = None;
    let mut prev_char: Option<(&str, usize)> = None;

    for char in input.chars() {
        if let Some((last_str, last_offset)) = last_char {
            offset += last_str.len();
            prev_char = Some((last_str, last_offset));
        }

        let char_as_str = &input[offset..offset + char.len_utf8()];

        last_char = Some((char_as_str, offset));

        if escaping {
            escaping = false;
            continue;
        }

        if commenting {
            if char == '\n' {
                commenting = false;
            }
            continue;
        }

        match opened.last() {
            // We are in a single-quoted string
            Some(("'", opening_offset)) => {
                if char == '\\' {
                    escaping = true;
                } else if char == '\'' {
                    push!(
                        offset,
                        1,
                        NestingActionType::Closing {
                            matching_opening: true,
                        }
                    );

                    closed.push(*opening_offset);

                    opened.pop();
                    opened_strings.pop();
                }
            }

            // We are in a double-quoted string
            Some(("\"", opening_offset)) => match char {
                '\\' => escaping = true,

                '(' => {
                    if let Some(("$", prev_offset)) = prev_char {
                        open!(prev_offset, &input[prev_offset..prev_offset + 2])
                    }
                }

                '`' => open!(offset, char_as_str),

                '"' => {
                    push!(
                        offset,
                        1,
                        NestingActionType::Closing {
                            matching_opening: true,
                        }
                    );

                    closed.push(*opening_offset);

                    opened.pop();
                    opened_strings.pop();
                }

                _ => {}
            },

            // We are not in a single- or double-quoted strnig
            // But we may be in a back-quoted string
            _ => match char {
                // This case is handled here as almost every single other character will be matched exactly
                // like in a non-quoted part
                '`' => {
                    if let Some(("`", opening_offset)) = opened.last() {
                        push!(
                            offset,
                            1,
                            NestingActionType::Closing {
                                matching_opening: true,
                            }
                        );

                        closed.push(*opening_offset);

                        opened.pop();
                    }
                }

                ';' | '|' if !matches!(opened.last(), Some(("`", _))) => {
                    push!(offset, 1, NestingActionType::CommandSeparator);
                }

                '#' => commenting = true,

                '\'' => {
                    open!(offset, char_as_str);
                    opened_strings.push((char_as_str, offset));
                }

                '"' => {
                    open!(offset, char_as_str);
                    opened_strings.push((char_as_str, offset));
                }

                '(' => open!(offset, char_as_str),

                '[' | '{' => open!(offset, char_as_str),

                ')' | ']' | '}' => {
                    if let Some((opening_str, opening_offset)) = opened.last().copied() {
                        if matches!(
                            (opening_str, char),
                            ("(" | "$(", ')') | ("[", ']') | ("{", '}')
                        ) {
                            push!(
                                offset,
                                1,
                                NestingActionType::Closing {
                                    matching_opening: true,
                                }
                            );

                            closed.push(opening_offset);

                            opened.pop();

                            continue;
                        }
                    }

                    push!(
                        offset,
                        1,
                        NestingActionType::Closing {
                            matching_opening: false,
                        }
                    );
                }

                ' ' => {
                    if matches!(opened.last(), None | Some(("$(", _))) {
                        push!(offset, 1, NestingActionType::ArgumentSeparator);
                    }
                }

                _ => {}
            },
        }
    }

    register_content_until!(input.len());

    let mut prev = None;

    for piece in output.iter_mut() {
        if let NestingActionType::Opening {
            typ: _,
            matching_close,
        } = &mut piece.action_type
        {
            assert!(!*matching_close);

            if closed.contains(&piece.offset) {
                *matching_close = true;
            }
        }

        assert!(piece.offset == prev.unwrap_or(0));
        prev = Some(piece.offset + piece.len);
    }

    NestingDetectionResult {
        actions: output,
        final_nesting_level: opened.len(),
    }
}

#[derive(Debug, Clone, Copy)]
pub struct NestingAction {
    pub offset: usize,
    pub len: usize,
    pub action_type: NestingActionType,
    pub nesting_level: usize,
}

#[derive(Debug, Clone, Copy)]
pub enum NestingActionType {
    Opening {
        typ: NestingOpeningType,
        matching_close: bool,
    },
    Closing {
        matching_opening: bool,
    },
    CommandSeparator,
    ArgumentSeparator,
    Content,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NestingOpeningType {
    Block,
    List,
    ExprWithParen,
    LiteralString,
    ComputedString,
    ExprInString,
    CmdOutput,
}

impl NestingOpeningType {
    fn try_from_str(str: &str) -> Result<Self, String> {
        match str {
            "{" => Ok(NestingOpeningType::Block),
            "[" => Ok(NestingOpeningType::List),
            "(" => Ok(NestingOpeningType::ExprWithParen),
            "'" => Ok(NestingOpeningType::LiteralString),
            "\"" => Ok(NestingOpeningType::ComputedString),
            "`" => Ok(NestingOpeningType::ExprInString),
            "$(" => Ok(NestingOpeningType::CmdOutput),
            _ => Err(format!(
                "Internal error: unrecognized opening type: >{str}<"
            )),
        }
    }
}
