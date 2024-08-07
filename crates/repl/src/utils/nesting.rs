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
//! This could technically be considered as a lexer, as it parses the code into meaningful chunks.
//!
//! These informations are then used for checking if a line is complete or not
//!     (cf. [`super::super::validator`])
//! They are also used to extract informations from a command prompt
//!     (cf. [`super::cmd_pieces`])
//!

pub fn detect_nesting_actions(input: &str, insert_args_separator: bool) -> Vec<NestingAction> {
    let mut opened: Vec<(&str, usize)> = vec![];
    let mut output: Vec<NestingAction> = vec![];
    let mut closed = vec![];

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

            assert!($offset + $opening_str.len() <= input.len());

            output.push(NestingAction {
                offset: $offset,
                len: $opening_str.len(),
                action_type: NestingActionType::Opening {
                    typ: NestingOpeningType::try_from_str($opening_str).unwrap(),
                    matching_close: false,
                },
                nesting_level: opened.len(),
            });
        }};
    }

    macro_rules! close {
        ($offset: expr, $len: expr, $opening_offset: expr) => {
            let (opening_str, _) = opened.pop().unwrap();

            push!(
                $offset,
                $len,
                NestingActionType::Closing {
                    matching_opening: Some(NestingOpeningType::try_from_str(opening_str).unwrap()),
                }
            );

            closed.push($opening_offset);
        };
    }

    let mut opened_strings: Vec<(&str, usize)> = vec![];
    let mut escaping = false;
    let mut commenting = false;

    let mut offset = 0;
    let mut last_char = None::<(&str, usize)>;
    let mut prev_char = None::<(&str, usize)>;

    let mut chars = input.chars().peekable();

    while let Some(char) = chars.next() {
        let next_char = chars.peek();

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

        match opened.last().copied() {
            // We are in a single-quoted string
            Some(("'", opening_offset)) => {
                if char == '\\' {
                    escaping = true;
                } else if char == '\'' {
                    close!(offset, 1, opening_offset);
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
                    close!(offset, 1, opening_offset);
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
                    if let Some(("`", opening_offset)) = opened.last().copied() {
                        close!(offset, 1, opening_offset);
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

                '(' | '[' => open!(offset, char_as_str),

                '{' => {
                    if matches!(prev_char, Some(("{", _))) {
                        open!(offset - 1, "{{");
                    } else if matches!(next_char, Some('{')) {
                        continue;
                    } else {
                        open!(offset, "{");
                    }
                }

                '}' if matches!(prev_char, Some(("}", _))) => {
                    if let Some(("{{", opening_offset)) = opened.last().copied() {
                        close!(offset - 1, 2, opening_offset);
                    } else {
                        push!(
                            offset - 1,
                            2,
                            NestingActionType::Closing {
                                matching_opening: None,
                            }
                        );
                    }
                }

                '}' if matches!(next_char, Some('}')) => continue,

                ')' | ']' | '}' => {
                    if let Some((opening_str, opening_offset)) = opened.last().copied() {
                        if matches!(
                            (opening_str, char),
                            ("(" | "$(", ')') | ("[", ']') | ("{", '}')
                        ) {
                            close!(offset, 1, opening_offset);
                            continue;
                        }
                    }

                    push!(
                        offset,
                        1,
                        NestingActionType::Closing {
                            matching_opening: None,
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

    output
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
        matching_opening: Option<NestingOpeningType>,
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
    SingleArgLambda,
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
            "{{" => Ok(NestingOpeningType::SingleArgLambda),
            _ => Err(format!(
                "Internal error: unrecognized opening type: >{str}<"
            )),
        }
    }
}
