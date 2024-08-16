//!
//! Command pieces computing.
//!
//! Cuts an input string (prompt) into a list of command pieces ([`CmdPiece`]).
//! Each piece refers to either the command name or one of its arguments, in order.
//! Only the last command in the input is preserved, and only at the current nesting level.
//! It handles strings, escaping, nested commands etc.
//!
//! For instance, `cmd1 $(cmd2 arg` would produce a piece for `cmd2` and `arg`
//! While `cmd1 $(cmd2 arg)` would produce a piece for `cmd1` and `$(cmd2 arg)`
//!
//! This module is used to extract informations for command completions.
//!

use crate::utils::nesting::NestingOpeningType;

use super::nesting::{detect_nesting_actions, NestingActionType};

#[derive(Debug, Clone, Copy)]
pub struct CmdPiece<'a> {
    pub start: usize,
    pub content: &'a str,
}

pub fn compute_command_pieces(input: &str) -> Vec<CmdPiece> {
    let nesting_actions = detect_nesting_actions(input, true);

    let cmd_nesting_levels =
        [0].into_iter().chain(nesting_actions.iter().filter_map(
            |action| match action.action_type {
                NestingActionType::Opening {
                    typ: NestingOpeningType::CmdOutput | NestingOpeningType::Lambda,
                    matching_close: _,
                }
                | NestingActionType::CommandSeparator => Some(action.nesting_level),

                _ => None,
            },
        ));

    let beg = cmd_nesting_levels.rev().find_map(|level| {
        nesting_actions
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
        Some((pos, level)) => {
            if pos == nesting_actions.len() {
                let start = nesting_actions[pos - 1].offset + nesting_actions[pos - 1].len;

                return vec![CmdPiece {
                    start,
                    content: &input[start..],
                }];
            }

            (level, pos, nesting_actions[pos].offset)
        }
        None => (0, 0, 0),
    };

    fn make_piece(extract: &str, start: usize) -> CmdPiece {
        let trimmed = extract.len() - extract.trim().len();

        CmdPiece {
            start: start + trimmed,
            content: extract.trim(),
        }
    }

    for action in &nesting_actions[pos..] {
        if action.nesting_level == level
            && matches!(action.action_type, NestingActionType::ArgumentSeparator)
        {
            segments.push(make_piece(&input[from..action.offset], from));
            from = action.offset + 1;
        }
    }

    if from <= input.len() {
        segments.push(make_piece(&input[from..], from));
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
