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
            NestingActionType::Opening(NestingOpeningType::try_from_str(opening_str).unwrap()),
        ));
    };

    let mut opened: Vec<(&str, usize)> = vec![];
    let mut opened_strings: Vec<(&str, usize)> = vec![];
    let mut output: Vec<NestingAction> = vec![];
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

        match char {
            '#' => {
                if !matches!(opened.last(), Some(("\"", _))) {
                    commenting = true;
                }
            }

            '\\' => {
                if matches!(opened.last(), Some(("\"", _))) {
                    escaping = true;
                }
            }

            '(' | '[' | '{' => {
                if let Some(("\"", _)) = opened.last() {
                    if let Some(("$", prev_offset)) = prev_char {
                        open(
                            &mut output,
                            &mut opened,
                            prev_offset,
                            &input[prev_offset..prev_offset + 2],
                        );
                    }

                    continue;
                }

                open(&mut output, &mut opened, offset, char_as_str);
            }

            ')' | ']' | '}' => {
                if matches!(opened.last(), Some(("\"", _))) {
                    continue;
                }

                if let Some((opening_str, opening_offset)) = opened.last().copied() {
                    if matches!(
                        (opening_str, char),
                        ("(" | "$(", ')') | ("[", ']') | ("{" | "${", '}')
                    ) {
                        push(
                            &mut output,
                            offset,
                            1,
                            NestingActionType::Closing { opening_offset },
                        );

                        opened.pop();

                        continue;
                    }
                }

                push(
                    &mut output,
                    offset,
                    1,
                    NestingActionType::ClosingWithoutOpening,
                );
            }

            '"' => {
                if let Some((opening_str, opening_offset)) = opened_strings.last().copied() {
                    if opened.last().copied() == Some((opening_str, opening_offset)) {
                        opened.pop();
                        opened_strings.pop();

                        push(
                            &mut output,
                            offset,
                            1,
                            NestingActionType::Closing { opening_offset },
                        );

                        continue;
                    }
                }

                open(&mut output, &mut opened, offset, char_as_str);
                opened_strings.push((char_as_str, offset));
            }

            _ => {}
        }
    }

    for (_, offset) in opened {
        let unclosed = output
            .iter_mut()
            .find(|piece| piece.offset == offset)
            .unwrap();

        assert!(matches!(
            unclosed.action_type,
            NestingActionType::Opening(_)
        ));

        match unclosed.action_type {
            NestingActionType::Opening(typ) => {
                unclosed.action_type = NestingActionType::Unclosed(typ);
            }

            _ => unreachable!(),
        }
    }

    register_content(&mut output, input.len());

    // Validate results
    let mut openings = 0;

    for (i, piece) in output.iter().enumerate() {
        match piece.action_type {
            NestingActionType::Opening(_) => {
                openings += 1;
            }
            NestingActionType::Closing { opening_offset: _ } => {
                openings -= 1;
            }
            NestingActionType::Unclosed(_) => {}
            NestingActionType::ClosingWithoutOpening => {}
            NestingActionType::Content => {}
        };

        if i == 0 {
            assert!(piece.offset == 0);
        } else {
            let prev = output.get(i - 1).unwrap();
            assert!(piece.offset == prev.offset + prev.len);
        }
    }

    assert!(openings == 0);

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
    Opening(NestingOpeningType),
    Unclosed(NestingOpeningType),
    Closing { opening_offset: usize },
    ClosingWithoutOpening,
    Content,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NestingOpeningType {
    Block,
    List,
    ExprWithParen,
    String,
    ExprInString,
    CmdCallInString,
}

impl NestingOpeningType {
    fn try_from_str(str: &str) -> Result<Self, String> {
        match str {
            "{" => Ok(NestingOpeningType::Block),
            "[" => Ok(NestingOpeningType::List),
            "(" => Ok(NestingOpeningType::ExprWithParen),
            "\"" => Ok(NestingOpeningType::String),
            "$(" => Ok(NestingOpeningType::ExprInString),
            "${" => Ok(NestingOpeningType::CmdCallInString),
            _ => Err(format!(
                "Internal error: unrecognized opening type: '{str}'"
            )),
        }
    }
}
