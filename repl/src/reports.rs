use std::ops::Range;

use ariadne::{Fmt, Label, Report, ReportKind, Source};
use parsy::{
    CodeRange, CriticalErrorMsgContent, CriticalErrorNature, FileId, ParserExpectation,
    ParsingError,
};
use reshell_runtime::{
    errors::{ExecError, ExecErrorContent},
    files_map::{FilesMap, ScopableFilePath, SourceFile},
};

use crate::repl::ReplError;

pub struct ReplReport {
    pub inner: Report<'static, (String, Range<usize>)>,
    pub file: SourceFile,
    pub display_file: String,
    pub msg: String,
}

pub fn create_report(at: CodeRange, msg: String, files: &FilesMap) -> ReplReport {
    match at.start.file_id {
        FileId::None => unreachable!(),

        FileId::Internal => {
            let src = "<native code>";

            create_report_raw(
                SourceFile {
                    id: 0,
                    path: ScopableFilePath::InMemory("native"),
                    content: src.to_string(),
                },
                0,
                src.as_bytes().len(),
                msg,
            )
        }

        FileId::SourceLess { name: _ } => {
            let src = "<source-less code>";

            create_report_raw(
                SourceFile {
                    id: 0,
                    path: ScopableFilePath::InMemory("source-less code"),
                    content: src.to_string(),
                },
                0,
                src.as_bytes().len(),
                msg,
            )
        }

        FileId::Id(id) => {
            let file = files.get_file(id).unwrap();

            let offset = at.start.offset();
            let offset = file.content[..offset].chars().count();
            let len = file.content[offset..offset + at.len].chars().count();

            create_report_raw(file.clone(), offset, len, msg)
        }
    }
}

fn create_report_raw(file: SourceFile, offset: usize, len: usize, msg: String) -> ReplReport {
    let display_file = match &file.path {
        ScopableFilePath::InMemory(in_mem) => format!("<{in_mem}>"),
        ScopableFilePath::RealFile(path) => path.to_string_lossy().to_string(),
    };

    let inner = Report::build(ReportKind::Error, display_file.clone(), offset)
        .with_label(
            Label::new((display_file.clone(), offset..(offset + len)))
                .with_message(msg.clone().fg(ariadne::Color::Red).to_string())
                .with_color(ariadne::Color::Red),
        )
        .finish();

    ReplReport {
        inner,
        file,
        display_file,
        msg,
    }
}

// pub fn report_to_str(report: ReplReport) -> String {
//     let mut out = BufWriter::new(Vec::new());

//     report
//         .inner
//         .write(
//             (report.display_file, Source::from(report.file.content)),
//             &mut out,
//         )
//         .unwrap();

//     String::from_utf8(out.into_inner().unwrap()).unwrap()
// }

pub fn print_error_report(report: ReplReport) {
    report
        .inner
        .print((report.display_file, Source::from(report.file.content)))
        .unwrap();
}

pub fn repl_error_report(err: &ReplError, files: &FilesMap) -> ReplReport {
    match err {
        ReplError::ParsingError(err) => parsing_error_report(err, files),
        ReplError::ExecError(err) => exec_error_report(err, files),
    }
}

pub fn exec_error_report(err: &ExecError, files: &FilesMap) -> ReplReport {
    match &err.content {
        ExecErrorContent::Str(str) => create_report(err.at, str.to_string(), files),
        ExecErrorContent::String(string) => create_report(err.at, string.clone(), files),
        ExecErrorContent::ParsingErr(err) => parsing_error_report(err, files),
        ExecErrorContent::CommandFailed {
            message,
            exit_status: _,
        } => create_report(err.at, message.clone(), files),
    }
}

pub fn parsing_error_report(err: &ParsingError, files: &FilesMap) -> ReplReport {
    create_report(
        err.inner().at(),
        match err.critical() {
            Some(msg) => match msg {
                CriticalErrorNature::Direct(content) => critical_error_msg_to_str(content, err),
                CriticalErrorNature::UnexpectedEndOfInput(content) => critical_error_msg_to_str(content, err)
                // format!(
                //     "unexpected end of input ({})",
                //     critical_error_msg_to_str(content, err)
                // ),
            },
            None => parser_expection_to_str(err.inner().expected()),
        },
        files,
    )
}

fn critical_error_msg_to_str(content: &CriticalErrorMsgContent, err: &ParsingError) -> String {
    match content {
        CriticalErrorMsgContent::Inherit => parser_expection_to_str(err.inner().expected()),
        CriticalErrorMsgContent::Custom(msg) => msg.to_string(),
    }
}

fn parser_expection_to_str(err: &ParserExpectation) -> String {
    match err {
        ParserExpectation::Char(c) => format!("expected char '{c}'"),
        ParserExpectation::Str(str) => format!("expected '{str}'"),
        ParserExpectation::Custom(msg) => format!("{msg}"),
        ParserExpectation::Break => {
            "Break (todo: move this not an error but something else)".to_string()
        }
    }
}
