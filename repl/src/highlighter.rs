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

trait Highlight {
    fn highlight(&self, h: &mut HighlightList);
}

fn highlight(program: &Program, src: &str) -> StyledText {
    let mut list = HighlightList::new(src);
    program.highlight(&mut list);
    list.into_rendered()
}

impl Highlight for Program {
    fn highlight(&self, h: &mut HighlightList) {
        let Self { content } = self;
        content.highlight(h);
    }
}

impl Highlight for Block {
    fn highlight(&self, h: &mut HighlightList) {
        let Self { instructions } = self;

        for instr in instructions {
            instr.data.highlight(h);
        }
    }
}

impl Highlight for Instruction {
    fn highlight(&self, h: &mut HighlightList) {
        match self {
            Instruction::Comment { content } => {
                h.comment(content);
            }

            Instruction::Include(path) => {
                path.data.highlight(h);
            }

            Instruction::DeclareVar {
                name,
                mutable: _,
                init_expr,
            } => {
                h.var_name(name);

                if let Some(init_expr) = init_expr {
                    init_expr.data.highlight(h);
                }
            }

            Instruction::AssignVar { name, expr } => {
                h.var_name(name);
                expr.data.highlight(h);
            }

            Instruction::IfCond {
                cond,
                body,
                elsif,
                els,
            } => {
                cond.data.highlight(h);
                body.data.highlight(h);

                for branch in elsif {
                    branch.data.highlight(h);
                }

                if let Some(els) = els {
                    els.data.highlight(h);
                }
            }

            Instruction::ForLoop {
                iter_var,
                iter_on,
                body,
            } => {
                h.var_name(iter_var);

                iter_on.data.highlight(h);
                body.data.highlight(h);
            }

            Instruction::WhileLoop { cond, body } => {
                cond.data.highlight(h);
                body.data.highlight(h);
            }

            Instruction::LoopContinue => {}

            Instruction::LoopBreak => {}

            Instruction::Switch { cases } => {
                for case in cases {
                    case.data.highlight(h);
                }
            }

            Instruction::FnDecl {
                name,
                signature,
                body,
            } => {
                h.fn_name(name);

                signature.highlight(h);
                body.data.highlight(h);
            }

            Instruction::FnReturn { expr } => {
                if let Some(expr) = expr {
                    expr.data.highlight(h);
                }
            }

            Instruction::Throw(expr) => {
                expr.data.highlight(h);
            }

            Instruction::CmdCall(call) => {
                call.data.highlight(h);
            }

            Instruction::BaseBlock(block) => {
                block.data.highlight(h);
            }
        }
    }
}

impl Highlight for ElsIf {
    fn highlight(&self, h: &mut HighlightList) {
        let Self { cond, body } = self;

        cond.data.highlight(h);
        body.data.highlight(h);
    }
}

impl Highlight for SwitchCase {
    fn highlight(&self, _: &mut HighlightList) {
        todo!()
    }
}

impl Highlight for FnSignature {
    fn highlight(&self, h: &mut HighlightList) {
        let Self { args, ret_type } = self;

        for arg in args {
            arg.highlight(h);
        }

        if let Some(ret_type) = ret_type {
            ret_type.data.highlight(h);
        }
    }
}

impl Highlight for FnArg {
    fn highlight(&self, h: &mut HighlightList) {
        let Self {
            names,
            is_optional: _,
            is_rest: _,
            typ,
        } = self;

        names.highlight(h);

        if let Some(typ) = typ {
            typ.data.highlight(h);
        }
    }
}

impl Highlight for FnArgNames {
    fn highlight(&self, h: &mut HighlightList) {
        match self {
            FnArgNames::NotFlag(name) => h.var_name(name),
            FnArgNames::ShortFlag(flag) => h.flag(flag.at),
            FnArgNames::LongFlag(flag) => h.flag(flag.at),
            FnArgNames::LongAndShortFlag { long, short } => {
                h.flag(long.at);
                h.flag(short.at);
            }
        }
    }
}

impl Highlight for ValueType {
    fn highlight(&self, h: &mut HighlightList) {
        match self {
            ValueType::Single(single) => single.highlight(h),
            ValueType::Union(types) => {
                for member in types {
                    member.highlight(h);
                }
            }
        }
    }
}

impl Highlight for Eaten<SingleValueType> {
    fn highlight(&self, h: &mut HighlightList) {
        match &self.data {
            SingleValueType::Any
            | SingleValueType::Null
            | SingleValueType::Bool
            | SingleValueType::Int
            | SingleValueType::Float
            | SingleValueType::String
            | SingleValueType::List
            | SingleValueType::Range
            | SingleValueType::Map
            | SingleValueType::Error
            | SingleValueType::Struct => h.type_name(self.at),
            SingleValueType::Function(signature) => signature.highlight(h),
        }
    }
}

impl Highlight for CmdCall {
    fn highlight(&self, h: &mut HighlightList) {
        let Self { base, pipes } = self;

        base.data.highlight(h);

        for pipe in pipes {
            pipe.highlight(h);
        }
    }
}

impl Highlight for SingleCmdCall {
    fn highlight(&self, h: &mut HighlightList) {
        let Self {
            env_vars,
            path,
            args,
        } = self;

        for env_var in &env_vars.data {
            env_var.data.highlight(h);
        }

        path.data.highlight(h);

        for arg in &args.data {
            arg.data.highlight(h);
        }
    }
}

impl Highlight for CmdEnvVar {
    fn highlight(&self, h: &mut HighlightList) {
        let Self { name, value } = self;

        h.env_var_name(name.at);

        value.data.highlight(h);
    }
}

impl Highlight for CmdEnvVarValue {
    fn highlight(&self, h: &mut HighlightList) {
        match self {
            CmdEnvVarValue::Raw(raw) => h.raw_str(raw.at),
            CmdEnvVarValue::ComputedString(computed_str) => computed_str.highlight(h),
            CmdEnvVarValue::Expr(expr) => expr.data.highlight(h),
        }
    }
}

impl Highlight for CmdPath {
    fn highlight(&self, h: &mut HighlightList) {
        match self {
            CmdPath::Raw(raw) => h.path(raw.at),
            CmdPath::ComputedString(computed_str) => computed_str.highlight(h),
        }
    }
}

impl Highlight for CmdArg {
    fn highlight(&self, h: &mut HighlightList) {
        match self {
            CmdArg::LiteralValue(lit_val) => lit_val.highlight(h),
            CmdArg::ComputedString(computed_str) => computed_str.highlight(h),
            CmdArg::CmdCall(call) => call.data.highlight(h),
            CmdArg::ParenExpr(expr) => expr.data.highlight(h),
            CmdArg::VarName(name) => h.var_name(name),
            CmdArg::FnAsValue(name) => h.fn_name(name),
            CmdArg::Raw(raw) => h.path(raw.at),
            CmdArg::SpreadVar(name) => h.var_name(name), // TODO: change?
        }
    }
}

impl Highlight for CmdPipe {
    fn highlight(&self, h: &mut HighlightList) {
        let Self { pipe_type: _, cmd } = self;

        cmd.data.highlight(h);
    }
}

impl Highlight for Eaten<ComputedString> {
    fn highlight(&self, h: &mut HighlightList) {
        let ComputedString { pieces } = &self.data;

        for piece in pieces {
            piece.data.highlight(h);
        }
    }
}

impl Highlight for ComputedStringPiece {
    fn highlight(&self, h: &mut HighlightList) {
        match self {
            ComputedStringPiece::Literal(lit) => h.string(lit.at),
            ComputedStringPiece::Escaped(escaped) => h.escaped_char(escaped.at),
            ComputedStringPiece::Variable(name) => h.var_name(name),
            ComputedStringPiece::Expr(expr) => expr.data.highlight(h),
            ComputedStringPiece::CmdCall(call) => call.data.highlight(h),
        }
    }
}

impl Highlight for Expr {
    fn highlight(&self, h: &mut HighlightList) {
        let Self { inner, right_ops } = self;

        inner.data.highlight(h);

        for right_op in right_ops {
            right_op.highlight(h);
        }
    }
}

impl Highlight for ExprInner {
    fn highlight(&self, h: &mut HighlightList) {
        let Self { content, prop_acc } = self;

        content.data.highlight(h);

        for acc in prop_acc {
            acc.data.highlight(h);
        }
    }
}

impl Highlight for ExprInnerContent {
    fn highlight(&self, h: &mut HighlightList) {
        match self {
            ExprInnerContent::SingleOp { op: _, right } => right.data.highlight(h),
            ExprInnerContent::ParenExpr(expr) => expr.data.highlight(h),
            ExprInnerContent::Value(value) => value.highlight(h),
        }
    }
}

impl Highlight for PropAccess {
    fn highlight(&self, h: &mut HighlightList) {
        let Self {
            nature,
            nullable: _,
        } = self;

        nature.data.highlight(h);
    }
}

impl Highlight for PropAccessNature {
    fn highlight(&self, h: &mut HighlightList) {
        match self {
            PropAccessNature::Key(expr) => expr.data.highlight(h),
            PropAccessNature::Prop(name) => h.prop_name(name.at),
        }
    }
}

impl Highlight for ExprOp {
    fn highlight(&self, h: &mut HighlightList) {
        let Self { op: _, with } = self;

        with.data.highlight(h);
    }
}

impl Highlight for Eaten<Value> {
    fn highlight(&self, h: &mut HighlightList) {
        match &self.data {
            Value::Null => h.keyword(self.at), // TODO: change?
            Value::Literal(lit_val) => lit_val.highlight(h),
            Value::ComputedString(computed_str) => computed_str.highlight(h),
            Value::List(list) => {
                for item in list {
                    item.data.highlight(h);
                }
            }
            Value::Object(obj) => {
                // TODO: find a way to highlight the names?
                for (_, expr) in obj {
                    expr.data.highlight(h);
                }
            }
            Value::Variable(name) => h.var_name(name),
            Value::FnCall(call) => call.data.highlight(h),
            Value::CmdOutput(call) => call.data.highlight(h),
            Value::CmdSuccess(call) => call.data.highlight(h),
            Value::FnAsValue(name) => h.fn_name(name),
            Value::Closure { signature, body } => {
                signature.highlight(h);
                body.data.highlight(h);
            }
        }
    }
}

impl Highlight for Eaten<LiteralValue> {
    fn highlight(&self, h: &mut HighlightList) {
        match &self.data {
            LiteralValue::Boolean(_) => h.bool(self.at),
            LiteralValue::Integer(_) => h.int(self.at),
            LiteralValue::Float(_) => h.float(self.at),
        }
    }
}

impl Highlight for FnCall {
    fn highlight(&self, h: &mut HighlightList) {
        let Self {
            is_var_name: _,
            name,
            call_args,
        } = self;

        h.var_name(name);

        for arg in &call_args.data {
            arg.highlight(h);
        }
    }
}

impl Highlight for FnCallArg {
    fn highlight(&self, h: &mut HighlightList) {
        match self {
            FnCallArg::Expr(expr) => expr.data.highlight(h),
            FnCallArg::CmdArg(call) => call.data.highlight(h),
        }
    }
}
