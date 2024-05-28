pub fn detect_nesting_actions<'s>(input: &'s str) -> Vec<NestingAction> {
    let register_content = |output: &mut Vec<NestingAction>, offset: usize| {
        let from = output
            .last()
            .map(|action| action.offset + action.len)
            .unwrap_or(0);

        if from == offset {
            return;
        }

        output.push(NestingAction {
            action_type: NestingActionType::Content,
            offset: from,
            len: offset - from,
        });
    };

    let push = |output: &mut Vec<NestingAction>,
                offset: usize,
                len: usize,
                action_type: NestingActionType| {
        register_content(output, offset);
        output.push(NestingAction::new(offset, len, action_type));
    };

    let open = |output: &mut Vec<NestingAction>,
                opened: &mut Vec<(&'s str, usize)>,
                offset: usize,
                opening_str: &'s str| {
        register_content(output, offset);
        opened.push((opening_str, offset));
        output.push(NestingAction::new(
            offset,
            opening_str.len(),
            NestingActionType::Opening {
                typ: NestingOpeningType::try_from_str(opening_str).unwrap(),
                matching_close: false,
            },
        ));
    };

    let mut opened: Vec<(&str, usize)> = vec![];
    let mut opened_strings: Vec<(&str, usize)> = vec![];
    let mut output: Vec<NestingAction> = vec![];
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
                    push(
                        &mut output,
                        offset,
                        1,
                        NestingActionType::Closing {
                            matching_opening: true,
                        },
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
                        open(
                            &mut output,
                            &mut opened,
                            prev_offset,
                            &input[prev_offset..prev_offset + 2],
                        )
                    }
                }

                '`' => open(&mut output, &mut opened, offset, char_as_str),

                '"' => {
                    push(
                        &mut output,
                        offset,
                        1,
                        NestingActionType::Closing {
                            matching_opening: true,
                        },
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
                // This case is handled here as every single other character will be matched exactly
                // like in a non-quoted part
                '`' => {
                    if let Some(("`", opening_offset)) = opened.last() {
                        push(
                            &mut output,
                            offset,
                            1,
                            NestingActionType::Closing {
                                matching_opening: true,
                            },
                        );

                        closed.push(*opening_offset);

                        opened.pop();
                    }
                }

                '#' => commenting = true,

                '\'' => {
                    open(&mut output, &mut opened, offset, char_as_str);
                    opened_strings.push((char_as_str, offset));
                }

                '"' => {
                    open(&mut output, &mut opened, offset, char_as_str);
                    opened_strings.push((char_as_str, offset));
                }

                '(' => open(&mut output, &mut opened, offset, char_as_str),

                '[' | '{' => open(&mut output, &mut opened, offset, char_as_str),

                ')' | ']' | '}' => {
                    if let Some((opening_str, opening_offset)) = opened.last().copied() {
                        if matches!(
                            (opening_str, char),
                            ("(" | "$(", ')') | ("[", ']') | ("{", '}')
                        ) {
                            push(
                                &mut output,
                                offset,
                                1,
                                NestingActionType::Closing {
                                    matching_opening: true,
                                },
                            );

                            closed.push(opening_offset);

                            opened.pop();

                            continue;
                        }
                    }

                    push(
                        &mut output,
                        offset,
                        1,
                        NestingActionType::Closing {
                            matching_opening: false,
                        },
                    );
                }

                _ => {}
            },
        }
    }

    register_content(&mut output, input.len());

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
}

impl NestingAction {
    pub fn new(offset: usize, len: usize, action_type: NestingActionType) -> Self {
        Self {
            offset,
            len,
            action_type,
        }
    }
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
