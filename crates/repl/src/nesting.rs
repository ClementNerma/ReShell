pub fn detect_nesting_actions<'s>(input: &'s str) -> Vec<NestingAction> {
    let mut opened: Vec<(&str, usize)> = vec![];
    let mut opened_strings: Vec<(&str, usize)> = vec![];
    let mut output: Vec<NestingAction> = vec![];
    let mut escaping: Option<(&str, usize)> = None;

    let mut offset = 0;
    let mut last_char: Option<(&str, usize)> = None;
    let mut prev_char: Option<(&str, usize)> = None;

    let register_string_piece =
        |output: &mut Vec<NestingAction>, opened: &mut Vec<(&str, usize)>, offset: usize| {
            if let Some(("\"", string_opened_at)) = opened.last() {
                let start_highlighting = output
                    .iter()
                    .rev()
                    .find(|a| a.offset > *string_opened_at)
                    .map(|a| a.offset + a.len)
                    .unwrap_or(string_opened_at + 1);

                if start_highlighting < offset {
                    output.push(NestingAction::new(
                        start_highlighting,
                        offset - start_highlighting,
                        NestingActionType::StringPiece,
                    ));
                }
            }
        };

    let push = |output: &mut Vec<NestingAction>,
                opened: &mut Vec<(&str, usize)>,
                offset: usize,
                len: usize,
                action_type: NestingActionType| {
        register_string_piece(output, opened, offset);

        output.push(NestingAction::new(offset, len, action_type));
    };

    let open = |output: &mut Vec<NestingAction>,
                opened: &mut Vec<(&'s str, usize)>,
                offset: usize,
                opening_str: &'s str| {
        register_string_piece(output, opened, offset);
        opened.push((opening_str, offset));
        output.push(NestingAction::new(
            offset,
            opening_str.len(),
            NestingActionType::Opening,
        ));
    };

    for char in input.chars() {
        if let Some((last_str, last_offset)) = last_char {
            offset += last_str.len();
            prev_char = Some((last_str, last_offset));
        }

        let char_as_str = &input[offset..offset + char.len_utf8()];

        last_char = Some((char_as_str, offset));

        if let Some((escaping_str, escaping_offset)) = escaping.take() {
            push(
                &mut output,
                &mut opened,
                escaping_offset,
                escaping_str.len(),
                NestingActionType::Escaping,
            );

            push(
                &mut output,
                &mut opened,
                offset,
                char.len_utf8(),
                NestingActionType::Escaping,
            );

            continue;
        }

        match char {
            '\\' => match opened.last() {
                Some(("\"", _)) => {
                    escaping = Some((char_as_str, offset));
                    push(
                        &mut output,
                        &mut opened,
                        offset,
                        1,
                        NestingActionType::Escaping,
                    );
                }

                _ => push(
                    &mut output,
                    &mut opened,
                    offset,
                    1,
                    NestingActionType::InvalidEscape,
                ),
            },

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
                            &mut opened,
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
                    &mut opened,
                    offset,
                    1,
                    NestingActionType::ClosingWithoutOpening,
                );
            }

            '"' => {
                if let Some((opening_str, opening_offset)) = opened_strings.last().copied() {
                    if opened.last().copied() == Some((opening_str, opening_offset)) {
                        // register_string_piece(&mut output, &mut opened, offset);

                        opened.pop();
                        opened_strings.pop();

                        push(
                            &mut output,
                            &mut opened,
                            offset,
                            1,
                            NestingActionType::Closing { opening_offset },
                        );

                        opened_strings.push((char_as_str, offset));

                        continue;
                    }
                }

                open(&mut output, &mut opened, offset, char_as_str);

                opened_strings.push((char_as_str, offset));
            }

            _ => {}
        }
    }

    for (str, offset) in opened {
        output.push(NestingAction::new(
            offset,
            str.len(),
            NestingActionType::Unclosed,
        ));
    }

    if let Some((str, offset)) = escaping {
        output.push(NestingAction::new(
            offset,
            str.len(),
            NestingActionType::InvalidEscape,
        ));
    }

    output
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum NestingActionType {
    Opening,
    Closing { opening_offset: usize },
    Unclosed,
    ClosingWithoutOpening,
    Escaping,
    InvalidEscape,
    StringPiece,
}
