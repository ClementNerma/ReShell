use super::nesting::{detect_nesting_actions, NestingActionType, NestingDetectionResult};

#[derive(Debug, Clone, Copy)]
pub struct CmdPiece<'a> {
    pub start: usize,
    pub content: &'a str,
}

pub fn compute_command_pieces(input: &str) -> Vec<CmdPiece> {
    let detect_nesting_actions = detect_nesting_actions(input);
    let NestingDetectionResult {
        actions,
        final_nesting_level,
    } = detect_nesting_actions;

    let beg = (0..=final_nesting_level).rev().find_map(|level| {
        actions
            .iter()
            .rposition(|action| {
                matches!(
                    action.action_type,
                    NestingActionType::CommandSeparator
                        | NestingActionType::Opening {
                            typ: _,
                            matching_close: _
                        }
                ) && action.nesting_level == level
            })
            .map(|pos| (pos + 1, level))
    });

    let mut segments = vec![];

    let (level, pos, mut from) = match beg {
        Some((pos, level)) => (level, pos, actions[pos].offset),
        None => (0, 0, 0),
    };

    for action in &actions[pos..] {
        if action.nesting_level == level
            && matches!(action.action_type, NestingActionType::ArgumentSeparator)
        {
            let extract = &input[from..action.offset];

            segments.push(CmdPiece {
                start: from,
                content: extract,
            });

            from = action.offset + 1;
        }
    }

    if from <= input.len() {
        segments.push(CmdPiece {
            start: from,
            content: &input[from..],
        });
    }

    let segments_len = segments.len();

    segments
        .into_iter()
        .enumerate()
        .filter_map(|(i, segment)| {
            if segment.content.is_empty() && i + 1 < segments_len {
                None
            } else {
                Some(segment)
            }
        })
        .collect()
}