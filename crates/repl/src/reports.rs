use std::borrow::Cow;

use annotate_snippets::{
    display_list::{DisplayList, FormatOptions},
    snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation},
};
use colored::Colorize;
use parsy::{CodeRange, Eaten, FileId, ParserExpectation, ParsingError, SourceFileID};
use reshell_checker::CheckerError;
use reshell_parser::{
    ast::{Program, RuntimeCodeRange},
    files::{FilesMap, SourceFile, SourceFileLocation},
};
use reshell_runtime::{
    context::CallStackEntry,
    display::dbg_loc,
    errors::{ExecError, ExecErrorNature},
};

#[derive(Debug)]
pub enum ReportableError {
    Parsing(ParsingError),
    Checking(CheckerError),
    Runtime(Box<ExecError>, Option<Eaten<Program>>),
}

impl ReportableError {
    pub fn exit_code(&self) -> Option<i32> {
        match self {
            ReportableError::Parsing(_) | ReportableError::Checking(_) => None,
            ReportableError::Runtime(err, _) => match err.nature {
                ExecErrorNature::Custom(_)
                | ExecErrorNature::ParsingErr(_)
                | ExecErrorNature::Thrown { value: _ } => None,

                ExecErrorNature::CommandFailedToStart { message: _ } => Some(1),

                ExecErrorNature::CommandFailed {
                    message: _,
                    exit_status,
                } => Some(exit_status.unwrap_or(1)),

                ExecErrorNature::CtrlC => None,

                ExecErrorNature::Exit { code } => Some(code.map(i32::from).unwrap_or(0)),
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

        ReportableError::Checking(err) => {
            let (at, err) = checking_error(err);
            (RuntimeCodeRange::Parsed(at), "Checking error", err)
        }

        ReportableError::Runtime(err, _) => match &err.nature {
            ExecErrorNature::Exit { code: _ } => {
                // These errors are designed to be consumed, not displayed
                unreachable!()
            }

            ExecErrorNature::CtrlC => (
                err.at,
                "User interruption",
                "program was interrupted by a Ctrl+C press".to_owned(),
            ),

            ExecErrorNature::Custom(message) => (
                err.at,
                "Runtime error",
                match message {
                    Cow::Borrowed(str) => (*str).to_owned(),
                    Cow::Owned(string) => string.clone(),
                },
            ),

            ExecErrorNature::ParsingErr(err) => {
                let (at, err) = parsing_error(err);
                (
                    RuntimeCodeRange::Parsed(at),
                    "Syntax error (at runtime)",
                    err,
                )
            }

            ExecErrorNature::CommandFailedToStart { message } => {
                (err.at, "Could not start command", message.clone())
            }

            ExecErrorNature::CommandFailed {
                message,
                exit_status: _,
            } => (err.at, "Command failed", message.clone()),

            ExecErrorNature::Thrown { value } => (
                err.at,
                "Function thrown",
                format!(
                    "function throwned a value at {}",
                    dbg_loc(value.from, files).bright_magenta()
                ),
            ),
        },
    };

    let call_stack = match err {
        ReportableError::Parsing(_) => None,
        ReportableError::Checking(_) => None,
        ReportableError::Runtime(err, _) => Some(&err.call_stack),
    };

    let (source_file, offset, len, msg) = match at {
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
                    src.as_bytes().len(),
                    msg,
                )
            }

            FileId::SourceFile(id) => {
                let file = files.get_file(id).unwrap();

                let offset = at.start.offset();
                let offset = file.content[..offset].chars().count();
                let len = file.content[offset..offset + at.len].chars().count();

                (file.clone(), offset, len, msg)
            }
        },

        RuntimeCodeRange::Internal => {
            let src = "<native code>";

            (
                SourceFile {
                    id: SourceFileID::from(0),
                    location: SourceFileLocation::CustomName("native".to_owned()),
                    content: src.to_string(),
                },
                0,
                src.as_bytes().len(),
                msg,
            )
        }
    };

    let display_file = match source_file.location {
        SourceFileLocation::CustomName(in_mem) => format!("<{in_mem}>"),
        SourceFileLocation::RealFile(path) => path.to_string_lossy().to_string(),
    };

    let nature = format!("{}", nature.bright_red());

    let snippet = Snippet {
        title: Some(Annotation {
            label: Some(&nature),
            id: None,
            annotation_type: AnnotationType::Error,
        }),
        footer: vec![],
        slices: vec![Slice {
            source: &source_file.content,
            line_start: 1, // TODO
            origin: Some(&display_file),
            fold: true, // ?
            annotations: vec![SourceAnnotation {
                label: &msg,
                annotation_type: AnnotationType::Error,
                range: (offset, offset + len.max(1)),
            }],
        }],
        opt: FormatOptions {
            color: true,
            ..Default::default()
        },
    };

    eprintln!("{}", DisplayList::from(snippet));

    let note = match err {
        ReportableError::Parsing(_) => None,
        ReportableError::Checking(_) => {
            Some("Error was encountered before running the program".to_owned())
        }
        ReportableError::Runtime(err, _) => err.note.clone(),
    };

    if let Some(note) = note {
        eprintln!("  = {} {note}", "note:".cyan());
    }

    if let Some(call_stack) = call_stack {
        for entry in call_stack.history().iter().rev() {
            let CallStackEntry { fn_called_at } = entry;

            eprintln!(
                "  = {} function called at: {}",
                "note:".cyan(),
                dbg_loc(*fn_called_at, files).bright_magenta()
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
    let msg = match err.critical() {
        Some(nature) => nature.to_string(),
        None => parser_expection_to_str(err.inner().expected()),
    };

    (err.inner().at(), msg)
}

fn checking_error(err: &CheckerError) -> (CodeRange, String) {
    let CheckerError { at, msg } = err;
    (*at, msg.clone())
}
