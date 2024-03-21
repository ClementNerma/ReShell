use ariadne::{Fmt, Label, Report, ReportKind, Source};
use parsy::{CodeRange, FileId, ParserExpectation, ParsingError};
use reshell_runtime::{
    display::dbg_loc,
    errors::{ExecError, ExecErrorContent, StackTrace, StackTraceEntry},
    files_map::{FilesMap, ScopableFilePath, SourceFile},
};

pub enum ReportableError {
    ParsingError(ParsingError),
    ExecError(ExecError),
}

pub fn print_error(err: &ReportableError, files: &FilesMap) {
    let (at, msg) = match err {
        ReportableError::ParsingError(err) => parsing_error(err),

        ReportableError::ExecError(err) => match &err.content {
            ExecErrorContent::Str(str) => (err.at, str.to_string()),
            ExecErrorContent::String(string) => (err.at, string.clone()),
            ExecErrorContent::ParsingErr(err) => parsing_error(err),
            ExecErrorContent::CommandFailed {
                message,
                exit_status: _,
            } => (err.at, message.clone()),
        },
    };

    let stack_trace = match err {
        ReportableError::ParsingError(_) => None,
        ReportableError::ExecError(err) => Some(&err.stack_trace),
    };

    let (source_file, offset, len, msg) = match at.start.file_id {
        FileId::None => unreachable!(),

        FileId::Internal => {
            let src = "<native code>";

            (
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

        FileId::Custom(id) => {
            assert_eq!(id, 0);

            let src = "<source-less code>";

            (
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

            (file.clone(), offset, len, msg)
        }
    };

    let display_file = match &source_file.path {
        ScopableFilePath::InMemory(in_mem) => format!("<{in_mem}>"),
        ScopableFilePath::InMemoryWithCounter(in_mem, counter) => format!("<{in_mem}[{counter}]>"),
        ScopableFilePath::RealFile(path) => path.to_string_lossy().to_string(),
    };

    let msg = match stack_trace {
        None => msg,
        Some(StackTrace { history }) => format!(
            "{msg}{}",
            history
                .iter()
                .rev()
                .map(|StackTraceEntry { fn_called_at }| {
                    format!("\n| stacktrace: {}", dbg_loc(*fn_called_at, files))
                })
                .collect::<String>()
                .fg(ariadne::Color::Blue)
        ),
    };

    let inner = Report::build(ReportKind::Error, display_file.clone(), offset)
        .with_label(
            Label::new((display_file.clone(), offset..(offset + len)))
                .with_message(msg.fg(ariadne::Color::Red).to_string())
                .with_color(ariadne::Color::Red),
        )
        .finish();

    inner
        .print((display_file, Source::from(source_file.content)))
        .unwrap();
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
