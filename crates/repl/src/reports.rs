//!
//! Reports display module.
//!
//! Provides utility functoins to display errors properly.
//!
//! Will display an extract of where the error happened (if applicable) in the nicest
//! human-readable way.
//!

use std::borrow::Cow;

use annotate_snippets::{Level, Renderer, Snippet};
use colored::Colorize;
use parsy::{CodeRange, FileId, ParserExpectation, ParsingError, SourceFileID, Span};
use reshell_checker::CheckerError;
use reshell_parser::{
    ast::{Program, RuntimeCodeRange},
    files::{FilesMap, SourceFile, SourceFileLocation},
};
use reshell_runtime::{
    context::CallStackEntry,
    errors::{ExecError, ExecErrorNature, ExecInfoType, ExecNotActualError},
};
use reshell_shared::pretty::{PrettyPrintOptions, PrettyPrintable};

#[derive(Debug)]
pub enum ReportableError {
    Parsing(ParsingError),
    Checking(CheckerError),
    Runtime(Box<ExecError>, Option<Span<Program>>),
}

impl ReportableError {
    pub fn is_actual_error(&self) -> bool {
        match self {
            ReportableError::Parsing(_) => true,
            ReportableError::Checking(_) => true,

            ReportableError::Runtime(err, _) => match &err.nature {
                ExecErrorNature::ParsingErr(_)
                | ExecErrorNature::CheckingErr(_)
                | ExecErrorNature::CommandFailedToStart { message: _ }
                | ExecErrorNature::CommandFailed {
                    message: _,
                    exit_status: _,
                }
                | ExecErrorNature::Thrown { at: _, message: _ }
                | ExecErrorNature::FailureExit { code: _ }
                | ExecErrorNature::CtrlC
                | ExecErrorNature::Custom(_) => true,

                ExecErrorNature::NotAnError(err) => match err {
                    ExecNotActualError::SuccessfulExit => false,

                    ExecNotActualError::LoopContinuation | ExecNotActualError::LoopBreakage => {
                        unreachable!()
                    }
                },
            },
        }
    }

    pub fn exit_code(&self) -> Option<u8> {
        match self {
            ReportableError::Parsing(_) => None,
            ReportableError::Checking(_) => None,
            ReportableError::Runtime(err, _) => match &err.nature {
                ExecErrorNature::Custom(_)
                | ExecErrorNature::ParsingErr(_)
                | ExecErrorNature::CheckingErr(_)
                | ExecErrorNature::Thrown { at: _, message: _ } => None,

                ExecErrorNature::CommandFailedToStart { message: _ } => Some(1),

                ExecErrorNature::CommandFailed {
                    message: _,
                    exit_status,
                } => Some(
                    exit_status
                        .and_then(|code| u8::try_from(code).ok())
                        .unwrap_or(1),
                ),

                ExecErrorNature::CtrlC => None,

                ExecErrorNature::FailureExit { code } => Some(code.get()),

                ExecErrorNature::NotAnError(err) => match &err {
                    ExecNotActualError::SuccessfulExit => Some(0),

                    ExecNotActualError::LoopContinuation | ExecNotActualError::LoopBreakage => {
                        unreachable!()
                    }
                },
            },
        }
    }
}

pub fn print_error(err: &ReportableError, files: &FilesMap) {
    let (at, nature, msg) = match err {
        ReportableError::Parsing(err) => {
            let (at, err) = parsing_error(err);
            (RuntimeCodeRange::Parsed(at), "Syntax error", err)
        }

        ReportableError::Checking(err) => (
            RuntimeCodeRange::Parsed(err.at),
            "Checking error",
            err.msg.clone(),
        ),

        ReportableError::Runtime(err, _) => match &err.nature {
            ExecErrorNature::FailureExit { code } => (
                err.at,
                "Non-zero exit code",
                format!("program exited with code {code}"),
            ),

            ExecErrorNature::CtrlC => (
                err.at,
                "User interruption",
                "program was interrupted by a Ctrl+C press".to_owned(),
            ),

            ExecErrorNature::Custom(message) => (err.at, "Runtime error", {
                match message {
                    Cow::Borrowed(str) => (*str).to_owned(),
                    Cow::Owned(string) => string.clone(),
                }
            }),

            ExecErrorNature::ParsingErr(err) => {
                let (at, err) = parsing_error(err);
                (
                    RuntimeCodeRange::Parsed(at),
                    "Syntax error (at runtime)",
                    err,
                )
            }

            ExecErrorNature::CheckingErr(err) => (
                RuntimeCodeRange::Parsed(err.at),
                "Checking error",
                err.msg.clone(),
            ),

            ExecErrorNature::CommandFailedToStart { message } => {
                (err.at, "Could not start command", message.clone())
            }

            ExecErrorNature::CommandFailed {
                message,
                exit_status: _,
            } => (err.at, "Command failed", message.clone()),

            ExecErrorNature::Thrown { at: _, message } => (
                err.at,
                "Thrown",
                format!(
                    "thrown: {}",
                    // dbg_loc(*at, files).bright_magenta(),
                    message.bright_red()
                ),
            ),

            ExecErrorNature::NotAnError(_) => unreachable!(),
        },
    };

    let call_stack = match err {
        ReportableError::Parsing(_) => None,
        ReportableError::Checking(_) => None,
        ReportableError::Runtime(err, _) => Some(&err.call_stack),
    };

    let (source_file, offset, len) = match at {
        RuntimeCodeRange::Parsed(at) => match at.start.file_id {
            FileId::None => unreachable!("internal error: got 'None' file ID in error"),
            FileId::Internal => unreachable!("internal error: got internal file ID in error"),

            FileId::Custom(id) => {
                assert_eq!(id, 0);

                let src = "<source-less code>";

                (
                    SourceFile {
                        id: SourceFileID::from(0),
                        location: SourceFileLocation::CustomName("source-less code".to_owned()),
                        content: src.to_string(),
                    },
                    0,
                    src.len(),
                )
            }

            FileId::SourceFile(id) => {
                let file = files.get_file(id).unwrap();

                (file.clone(), at.start.offset(), at.len)
            }
        },

        RuntimeCodeRange::Internal(infos) => {
            let src = format!("<native code: {infos}>");
            let src_len = src.len();

            (
                SourceFile {
                    id: SourceFileID::from(0),
                    location: SourceFileLocation::CustomName("native".to_owned()),
                    content: src,
                },
                0,
                src_len,
            )
        }
    };

    let display_file = match source_file.location {
        SourceFileLocation::CustomName(in_mem) => format!("<{in_mem}>"),
        SourceFileLocation::RealFile(path) => path.to_string_lossy().to_string(),
    };

    let nature = format!("{}", nature.bright_red());

    let source = source_file.content;

    let line = source[..offset].chars().filter(|&c| c == '\n').count();

    let extract_start_line = line.saturating_sub(2) + 1;

    let extract_start_offset = if extract_start_line == 1 {
        0
    } else {
        let mut line_counter = 1;
        let mut shift = 0;

        source[..offset]
            .chars()
            .find_map(|c| {
                if c == '\n' {
                    line_counter += 1;
                }

                shift += c.len_utf8();

                if line_counter == extract_start_line {
                    Some(shift)
                } else {
                    None
                }
            })
            .unwrap_or(0)
    };

    let mut line_counter = 0;

    let afterwards = &source[offset + len..].chars().position(|c| {
        if c == '\n' {
            line_counter += 1;
        }

        line_counter == 2
    });

    let extract_end = match afterwards {
        Some(pos) => offset + len + pos + 1,
        None => source.len(),
    };

    // NOTE: we add a space at the end of the error's line
    // as the reporting library doesn't support displaying
    // offsets after a line's last character
    let extract = format!("{} ", &source[extract_start_offset..extract_end]);

    // Same thing for the error range source
    let range_chars_len = if offset + len.max(1) == source.len() + 1 {
        1
    } else {
        source[offset..offset + len.max(1)].len()
    };

    // Add manual bold + coloring to the error message,
    // as the `annotate-snippets` crate uses a different coloration crate
    // which doesn't support color nesting like `colored`
    let msg = msg.bright_red().bold().to_string();

    let snippet = Level::Error.title(&nature).snippet(
        Snippet::source(&extract)
            .line_start(extract_start_line)
            .origin(&display_file)
            .fold(false)
            .annotation(
                Level::Error
                    .span(
                        offset - extract_start_offset
                            ..offset - extract_start_offset + range_chars_len,
                    )
                    .label(&msg),
            ),
    );

    eprintln!("{}", Renderer::styled().render(snippet));

    let infos = match err {
        ReportableError::Parsing(_) => vec![],
        ReportableError::Checking(err) => err
            .details
            .iter()
            .map(|detail| (ExecInfoType::Note, detail.clone()))
            .collect(),
        ReportableError::Runtime(err, _) => match &err.nature {
            ExecErrorNature::CheckingErr(checking_err) => checking_err
                .details
                .iter()
                .map(|detail| (ExecInfoType::Note, detail.clone()))
                .chain(err.infos.clone())
                .collect(),

            _ => err.infos.clone(),
        },
    };

    for (info_type, content) in infos {
        eprintln!(
            "  = {} {content}",
            format!(
                "{}:",
                match info_type {
                    ExecInfoType::Note => "note",
                    ExecInfoType::Tip => "tip",
                }
            )
            .cyan()
        );
    }

    if let Some(call_stack) = call_stack {
        for entry in call_stack.history().iter().rev() {
            let CallStackEntry { fn_called_at } = entry;

            eprintln!(
                "  = {} function called at: {}",
                "note:".cyan(),
                fn_called_at.display(files, PrettyPrintOptions::inline())
            );
        }
    }
}

fn parser_expection_to_str(err: &ParserExpectation) -> String {
    match err {
        ParserExpectation::Char(c) => format!("expected char '{c}'"),
        ParserExpectation::Str(str) => format!("expected '{str}'"),
        ParserExpectation::Custom(msg) => msg.to_string(),
        ParserExpectation::Break => unreachable!(),
    }
}

fn parsing_error(err: &ParsingError) -> (CodeRange, String) {
    let msg = match err.critical_message() {
        Some(nature) => nature.to_string(),
        None => parser_expection_to_str(err.inner().expected()),
    };

    (err.inner().at(), msg)
}
