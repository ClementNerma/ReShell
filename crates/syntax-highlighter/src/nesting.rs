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

pub fn detect_nesting_actions(input: &str, insert_args_separator: bool) -> Vec<NestingAction> {
    let mut opened: Vec<(NestingOpeningType, usize)> = vec![];
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
        ($offset: expr, $len: expr, $opening_type: expr) => {{
            register_content_until!($offset);

            opened.push(($opening_type, $offset));

            #[allow(clippy::int_plus_one)]
            {
                assert!($offset + $len <= input.len());
            }

            output.push(NestingAction {
                offset: $offset,
                len: $len,
                action_type: NestingActionType::Opening {
                    typ: $opening_type,
                    matching_close: false,
                },
                nesting_level: opened.len(),
            });
        }};
    }

    macro_rules! close {
        ($offset: expr, $len: expr, $opening_offset: expr) => {
            let (opening_type, _) = opened.pop().unwrap();

            push!(
                $offset,
                $len,
                NestingActionType::Closing {
                    matching_opening: Some(opening_type),
                }
            );

            closed.push($opening_offset);
        };
    }

    let mut opened_strings: Vec<(&str, usize)> = vec![];
    let mut escaping = false;
    let mut commenting = false;

    let mut offset = 0;
    let mut last_char = None::<(char, usize)>;
    let mut prev_char = None::<(char, usize)>;

    for char in input.chars() {
        if let Some((last_str, last_offset)) = last_char {
            offset += last_str.len_utf8();
            prev_char = Some((last_str, last_offset));
        }

        let char_as_str = &input[offset..offset + char.len_utf8()];

        last_char = Some((char_as_str.chars().next().unwrap(), offset));

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
            Some((NestingOpeningType::LiteralString, opening_offset)) => {
                if char == '\\' {
                    escaping = true;
                } else if char == '\'' {
                    close!(offset, 1, opening_offset);
                    opened_strings.pop();
                }
            }

            // We are in a double-quoted (= computed) string
            Some((NestingOpeningType::ComputedString, opening_offset)) => match char {
                '\\' => escaping = true,

                '(' => {
                    if let Some(('$', prev_offset)) = prev_char {
                        open!(prev_offset, 2, NestingOpeningType::CmdOutput)
                    } else if input[..offset].ends_with("$^") {
                        open!(offset - 2, 3, NestingOpeningType::CmdOutput);
                    }
                }

                '`' => open!(offset, 1, NestingOpeningType::ExprInString),

                '"' => {
                    close!(offset, 1, opening_offset);
                    opened_strings.pop();
                }

                _ => {}
            },

            Some((NestingOpeningType::VarSpreading, opening_offset)) => match char {
                '\'' => {
                    open!(offset, 1, NestingOpeningType::LiteralString);
                    opened_strings.push((char_as_str, offset));
                }

                '[' | '{' => {
                    open!(offset, 1, NestingOpeningType::VarSpreading);
                }

                ']' if input[..=opening_offset].ends_with('[') => {
                    close!(offset, 1, opening_offset);
                }

                '}' if input[..=opening_offset].ends_with('{') => {
                    close!(offset, 1, opening_offset);
                }

                _ => {}
            },

            // We are not in a single- or double-quoted string
            // But we may be in a back-quoted string
            _ => match char {
                // This case is handled here as almost every single other character will be matched exactly
                // like in a non-quoted part
                '`' => {
                    if let Some((NestingOpeningType::ExprInString, opening_offset)) =
                        opened.last().copied()
                    {
                        close!(offset, 1, opening_offset);
                    }
                }

                '#' => commenting = true,

                '\'' => {
                    open!(offset, 1, NestingOpeningType::LiteralString);
                    opened_strings.push((char_as_str, offset));
                }

                '"' => {
                    open!(offset, 1, NestingOpeningType::ComputedString);
                    opened_strings.push((char_as_str, offset));
                }

                '(' => match prev_char {
                    Some(('$', prev_offset)) => {
                        open!(prev_offset, 2, NestingOpeningType::CmdOutput);
                    }

                    Some(('@', prev_offset)) => {
                        open!(prev_offset, 2, NestingOpeningType::CmdCall);
                    }

                    _ => {
                        if input[..offset].ends_with("$^") {
                            open!(offset - 2, 3, NestingOpeningType::CmdOutput);
                        } else if is_fn_decl(&input[..offset]) {
                            open!(offset, 1, NestingOpeningType::FnArgs { lambda: false });
                        } else {
                            open!(offset, 1, NestingOpeningType::ExprWithParen);
                        }
                    }
                },

                '{' | '['
                    if input[..offset].ends_with(' ')
                        && input[..offset].trim_end().ends_with("let") =>
                {
                    open!(offset, 1, NestingOpeningType::VarSpreading);
                }

                '[' => open!(offset, 1, NestingOpeningType::List),

                '{' => {
                    // 'it' lambda: `:{ ... }`
                    if let Some((':', _)) = prev_char {
                        open!(offset - 1, 2, NestingOpeningType::Lambda)
                    } else {
                        let input_after = &input[offset + char.len_utf8()..];

                        let is_lambda =
                            input_after
                                .chars()
                                .position(|c| c == '|')
                                .is_some_and(|pos| {
                                    input_after.chars().take(pos).all(|c| c.is_whitespace())
                                });

                        if is_lambda {
                            open!(offset, 1, NestingOpeningType::Lambda);
                        } else {
                            open!(offset, 1, NestingOpeningType::Block);
                        }
                    }
                }

                // TODO: handle union types
                '|' => match opened.last().copied() {
                    Some((NestingOpeningType::Lambda, lambda_start))
                        if input[lambda_start + 1..offset]
                            .chars()
                            .all(char::is_whitespace) =>
                    {
                        open!(offset, 1, NestingOpeningType::FnArgs { lambda: true });
                    }

                    Some((NestingOpeningType::FnArgs { lambda: true }, args_start)) => {
                        close!(offset, 1, args_start);
                    }

                    _ => push!(offset, 1, NestingActionType::CommandSeparator),
                },

                ')' | ']' | '}' => {
                    if let Some((opening_str, opening_offset)) = opened.last().copied()
                        && matches!(
                            (opening_str, char),
                            (
                                NestingOpeningType::ExprWithParen
                                    | NestingOpeningType::ComputedString
                                    | NestingOpeningType::CmdOutput
                                    | NestingOpeningType::CmdCall
                                    | NestingOpeningType::FnArgs { lambda: false },
                                ')'
                            ) | (NestingOpeningType::List, ']')
                                | (NestingOpeningType::Block | NestingOpeningType::Lambda, '}')
                        )
                    {
                        close!(offset, 1, opening_offset);
                        continue;
                    }

                    push!(
                        offset,
                        1,
                        NestingActionType::Closing {
                            matching_opening: None,
                        }
                    );
                }

                ';' => {
                    if matches!(
                        opened.last(),
                        None | Some((NestingOpeningType::Block | NestingOpeningType::Lambda, _))
                    ) {
                        push!(offset, 1, NestingActionType::CommandSeparator);
                    }
                }

                ' ' => {
                    if matches!(
                        opened.last(),
                        None | Some((NestingOpeningType::ComputedString, _))
                    ) {
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

fn is_fn_decl(input: &str) -> bool {
    let mut chars = input.chars().rev().peekable();

    let mut try_match = |filter: fn(&char) -> bool| -> bool {
        let mut matched = false;

        while chars.next_if(filter).is_some() {
            matched = true;
        }

        matched
    };

    try_match(|c| c.is_alphanumeric() || *c == '_')
        && try_match(|c| c.is_whitespace())
        && chars.next() == Some('n')
        && chars.next() == Some('f')
        && chars
            .next()
            .is_none_or(|c| !c.is_alphanumeric() && c != '_')
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
    VarSpreading,
    CmdOutput,
    CmdCall,
    Lambda,
    FnArgs { lambda: bool },
}
