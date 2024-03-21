use std::cmp::max;

use nu_ansi_term::{Color, Style};
use parsy::{Eaten, FileId, Parser};
use reedline::{Highlighter as RlHighlighter, StyledText};
use reshell_parser::{
    ast::{
        Block, CmdArg, CmdCall, CmdEnvVar, CmdEnvVarValue, CmdPath, CmdPipe, ComputedString,
        ComputedStringPiece, ElsIf, Expr, ExprInner, ExprInnerContent, ExprOp, FnArg, FnArgNames,
        FnCall, FnCallArg, FnSignature, Instruction, LiteralValue, Program, PropAccess,
        PropAccessNature, SingleCmdCall, SingleValueType, SwitchCase, Value, ValueType,
    },
    program,
};
use reshell_runtime::files_map::{FilesMap, ScopableFilePath};

use crate::{highlighting::HighlightList, reports::parsing_error_report};

// TODO: full AST-based highlighter (requires to have parsing recovery in case of error)
// TODO: highlighter error reporting is hiding autocompletion :/

pub fn create_highlighter() -> Box<dyn RlHighlighter> {
    Box::new(Highlighter)
}

pub struct Highlighter;

impl RlHighlighter for Highlighter {
    fn highlight(&self, line: &str, _cursor: usize) -> StyledText {
        let parser = program();

        let mut files = FilesMap::new();
        let file_id = files.register_file(ScopableFilePath::InMemory("<repl>"), line.to_string());

        match parser.parse_str_as_file(line, FileId::Id(file_id)) {
            Ok(ast) => highlight(&ast.data, line),

            Err(err) => {
                let at = err.inner().at();

                assert!(matches!(at.start.file_id, FileId::Id(_)));

                let line_with_s = format!("{line} ");

                let mut out = StyledText::new();

                out.push((Style::default(), line_with_s[..at.start.offset].to_string()));
                out.push((
                    Style::default().fg(Color::Red).underline(),
                    line_with_s[at.start.offset..at.start.offset + max(at.len, 1)].to_string(),
                ));
                out.push((
                    Style::default(),
                    line_with_s[at.start.offset + max(at.len, 1)..].to_string(),
                ));
                out.push((
                    Style::default().fg(Color::Red),
                    format!(" [{}]", parsing_error_report(&err, &files).msg),
                ));

                out
            }
        }
    }
}
