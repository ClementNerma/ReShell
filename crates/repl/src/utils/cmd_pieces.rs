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

use reshell_syntax_highlighter::nesting::{
    NestingActionType, NestingOpeningType, detect_nesting_actions,
};

#[derive(Debug, Clone, Copy)]
pub struct CmdPiece<'a> {
    pub start: usize,
    pub content: &'a str,
}

pub fn compute_command_pieces(input: &'_ str) -> Vec<CmdPiece<'_>> {
    let nesting_actions = detect_nesting_actions(input, true);

    let cmd_start = nesting_actions.iter().enumerate().rfind(|(_, action)| {
        matches!(
            action.action_type,
            NestingActionType::Opening {
                typ: NestingOpeningType::CmdOutput | NestingOpeningType::CmdCall,
                matching_close: _,
            } | NestingActionType::Closing {
                matching_opening: Some(NestingOpeningType::FnArgs { lambda: _ }),
            } | NestingActionType::CommandSeparator
        )
    });

    let mut segments = vec![];

    fn make_piece(extract: &'_ str, start: usize) -> CmdPiece<'_> {
        let trimmed = extract.len() - extract.trim().len();

        CmdPiece {
            start: start + trimmed,
            content: extract.trim(),
        }
    }

    let (mut from, level, action_index) = match cmd_start {
        Some((index, action)) => (action.offset + action.len, action.nesting_level, index),
        None => (0, 0, 0),
    };

    for action in &nesting_actions[action_index..] {
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
