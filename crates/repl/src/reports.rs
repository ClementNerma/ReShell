use std::borrow::Cow;

use ariadne::{Fmt, Label, Report, ReportKind, Source};
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
                ExecErrorNature::CommandFailed {
                    message: _,
                    exit_status,
                } => Some(exit_status.unwrap_or(1)),
                ExecErrorNature::Exit { code } => Some(code.map(i32::from).unwrap_or(0)),
            },
        }
    }
}

pub fn print_error(err: &ReportableError, files: &FilesMap) {
    let (at, msg) = match err {
        ReportableError::Parsing(err) => {
            let (at, err) = parsing_error(err);
            (RuntimeCodeRange::Parsed(at), err)
        }

        ReportableError::Checking(err) => {
            let (at, err) = checking_error(err);
            (RuntimeCodeRange::Parsed(at), err)
        }

        ReportableError::Runtime(err, _) => match &err.nature {
            ExecErrorNature::Exit { code: _ } => {
                // This error is designed to be consumed, not displayed
                unreachable!()
            }
            ExecErrorNature::Custom(message) => (
                err.at,
                match message {
                    Cow::Borrowed(str) => (*str).to_owned(),
                    Cow::Owned(string) => string.clone(),
                },
            ),
            ExecErrorNature::ParsingErr(err) => {
                let (at, err) = parsing_error(err);
                (RuntimeCodeRange::Parsed(at), err)
            }
            ExecErrorNature::CommandFailed {
                message,
                exit_status: _,
            } => (err.at, message.clone()),
            ExecErrorNature::Thrown { value } => (
                err.at,
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

    let mut bottom = String::new();

    // if let ReportableError::Runtime(err, _) = err {
    //     if let RuntimeCodeRange::Parsed(range) = err.scope_range {
    //         let curr_scope_msg = format!("* In scope : {}", dbg_loc(range, files).bright_magenta());
    //         bottom = format!("{}", curr_scope_msg.bright_yellow());
    //     }
    // }

    if let Some(call_stack) = call_stack {
        for entry in call_stack.history().iter().rev() {
            let CallStackEntry { fn_called_at } = entry;

            let entry_msg = format!(
                "* Called at: {}",
                dbg_loc(*fn_called_at, files).bright_magenta()
            );

            bottom.push_str(&format!("\n{}", entry_msg.fg(ariadne::Color::Yellow)));
        }
    }

    let mut inner = Report::build(ReportKind::Error, display_file.clone(), offset).with_label(
        Label::new((display_file.clone(), offset..(offset + len)))
            .with_message(msg.fg(ariadne::Color::Red).to_string())
            .with_color(ariadne::Color::Red),
    );

    match err {
        ReportableError::Parsing(_) => {}
        ReportableError::Checking(_) => {
            inner.set_note("Error was encountered before running the program")
        }
        ReportableError::Runtime(err, _) => {
            if let Some(note) = &err.note {
                inner.set_note(note);
            }
        }
    };

    inner
        .finish()
        .print((display_file, Source::from(source_file.content)))
        .unwrap();

    if !bottom.is_empty() {
        println!("{bottom}");
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
