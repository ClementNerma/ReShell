use nu_ansi_term::{Color, Style};
use parsy::{Eaten, FileId, Parser};
use reedline::{Highlighter as RlHighlighter, StyledText};
use reshell_parser::{
    ast::{
        Block, CmdArg, CmdCall, CmdEnvVar, CmdEnvVarValue, CmdPath, CmdPipe, ComputedString,
        ComputedStringPiece, ElsIf, Expr, ExprInner, ExprInnerContent, ExprOp, FnArg, FnArgNames,
        FnCall, FnCallArg, FnSignature, Instruction, LiteralValue, Program, PropAccess,
        PropAccessNature, SingleCmdCall, SingleValueType, StructTypeMember, SwitchCase, Value,
        ValueType,
    },
    program,
};
use reshell_runtime::files_map::{FilesMap, ScopableFilePath};

use crate::{highlighting::*, reports::parsing_error_report};

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

                // We need to put an underline under the erroneous part. BUT:
                // 1. The error may be *after* the end of the line (e.g. unexpected end of input)
                // 2. The error may have a zero-width, in which case we must underline at least one char. anyway
                let line_with_s = format!("{line} ");

                let take = if at.len >= 1 {
                    at.len
                } else {
                    line_with_s[at.start.offset..]
                        .chars()
                        .next()
                        .unwrap()
                        .len_utf8()
                };

                // The index separator is stored here
                // It's the &str index of the end of the part to underline
                let sep = at.start.offset + take;

                let mut out = StyledText::new();

                out.push((Style::default(), line_with_s[..at.start.offset].to_string()));
                out.push((
                    Style::default().fg(Color::Red).underline(),
                    line_with_s[at.start.offset..sep].to_string(),
                ));
                out.push((Style::default(), line_with_s[sep..].to_string()));
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

impl Highlight for Eaten<Block> {
    fn highlight(&self, h: &mut HighlightList) {
        let Block { instructions } = &self.data;

        if instructions.is_empty() {
            h.push(&KEYWORD, self.at);
            return;
        }

        h.push_until(&KEYWORD, self.at, instructions.first().unwrap().at);

        for instr in instructions {
            instr.highlight(h);
        }

        h.push_remaining(&KEYWORD, self.at);
    }
}

impl Highlight for Eaten<Instruction> {
    fn highlight(&self, h: &mut HighlightList) {
        match &self.data {
            Instruction::Comment { content } => {
                h.push(&COMMENT, content.at);
            }

            Instruction::DeclareVar {
                name,
                mutable: _,
                init_expr,
            } => {
                h.push_until(&KEYWORD, self.at, name.at);

                h.push(&VAR_NAME, name.at);

                if let Some(init_expr) = init_expr {
                    init_expr.data.highlight(h);
                }
            }

            Instruction::AssignVar { name, expr } => {
                h.push(&VAR_NAME, name.at);
                expr.data.highlight(h);
            }

            Instruction::IfCond {
                cond,
                body,
                elsif,
                els,
            } => {
                h.push_until(&KEYWORD, self.at, cond.at);

                cond.data.highlight(h);
                body.highlight(h);

                for branch in elsif {
                    branch.highlight(h);
                }

                if let Some(els) = els {
                    h.push_everything_until(&KEYWORD, els.at);
                    els.highlight(h);
                }
            }

            Instruction::ForLoop {
                iter_var,
                iter_on,
                body,
            } => {
                h.push_until(&KEYWORD, self.at, iter_var.at);
                h.push(&VAR_NAME, iter_var.at);
                h.push_everything_until(&KEYWORD, iter_on.at);

                iter_on.data.highlight(h);
                body.highlight(h);
            }

            Instruction::WhileLoop { cond, body } => {
                cond.data.highlight(h);
                body.highlight(h);
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
                h.push_until(&KEYWORD, self.at, name.at);
                h.push(&FN_NAME, name.at);

                signature.highlight(h);
                body.highlight(h);
            }

            Instruction::FnReturn { expr } => {
                if let Some(expr) = expr {
                    expr.data.highlight(h);
                }
            }

            Instruction::Throw(expr) => {
                expr.data.highlight(h);
            }

            Instruction::CmdAliasDecl { name, content } => {
                h.push_everything_until(&KEYWORD, name.at);
                h.push(&FN_NAME, name.at);
                h.push_everything_until(&KEYWORD, content.at);
                content.data.highlight(h);
            }

            Instruction::TypeAliasDecl { name, content } => {
                h.push_everything_until(&KEYWORD, name.at);
                h.push(&TYPE_NAME, name.at);
                h.push_everything_until(&KEYWORD, content.at);
                content.data.highlight(h);
            }

            Instruction::CmdCall(call) => {
                call.data.highlight(h);
            }

            Instruction::BaseBlock(block) => {
                block.highlight(h);
            }
        }
    }
}

impl Highlight for Eaten<ElsIf> {
    fn highlight(&self, h: &mut HighlightList) {
        let ElsIf { cond, body } = &self.data;

        h.push_until(&KEYWORD, self.at, cond.at);

        cond.data.highlight(h);
        body.highlight(h);
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
            FnArgNames::NotFlag(name) => h.push(&VAR_NAME, name.at),
            FnArgNames::ShortFlag(flag) => h.push(&FLAG, flag.at),
            FnArgNames::LongFlag(flag) => h.push(&FLAG, flag.at),
            FnArgNames::LongAndShortFlag { long, short } => {
                h.push(&FLAG, long.at);
                h.push(&FLAG, short.at);
            }
        }
    }
}

impl Highlight for ValueType {
    fn highlight(&self, h: &mut HighlightList) {
        match self {
            ValueType::Single(single) => single.eaten().unwrap().highlight(h),
            ValueType::Union(types) => {
                for member in types {
                    member.eaten().unwrap().highlight(h);
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
            | SingleValueType::UntypedStruct => h.push(&TYPE_NAME, self.at),
            SingleValueType::TypedStruct(members) => {
                if members.is_empty() {
                    h.push(&TYPE_NAME, self.at);
                    return;
                }

                h.push_everything_until(&TYPE_NAME, members.first().unwrap().eaten().unwrap().at);

                for member in members {
                    h.push_everything_until(&KEYWORD, member.eaten().unwrap().at);
                    member.data().highlight(h);
                }
            }
            SingleValueType::Function(signature) => signature.highlight(h),
            SingleValueType::TypeAlias(_) => h.push(&VAR_NAME, self.at),
        }
    }
}

impl Highlight for StructTypeMember {
    fn highlight(&self, h: &mut HighlightList) {
        let Self { name, typ } = &self;

        h.push(&VAR_NAME, name.eaten().unwrap().at);
        h.push_everything_until(&KEYWORD, typ.eaten().unwrap().at);
        typ.data().highlight(h);
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
            arg.highlight(h);
        }
    }
}

impl Highlight for CmdEnvVar {
    fn highlight(&self, h: &mut HighlightList) {
        let Self { name, value } = self;

        h.push(&ENV_VAR_NAME, name.at);

        value.data.highlight(h);
    }
}

impl Highlight for CmdEnvVarValue {
    fn highlight(&self, h: &mut HighlightList) {
        match self {
            CmdEnvVarValue::Raw(raw) => h.push(&RAW_STR, raw.at),
            CmdEnvVarValue::ComputedString(computed_str) => computed_str.highlight(h),
            CmdEnvVarValue::Expr(expr) => expr.data.highlight(h),
        }
    }
}

impl Highlight for CmdPath {
    fn highlight(&self, h: &mut HighlightList) {
        match self {
            CmdPath::Raw(raw) => h.push(&PATH, raw.at),
            CmdPath::ComputedString(computed_str) => computed_str.highlight(h),
        }
    }
}

impl Highlight for Eaten<CmdArg> {
    fn highlight(&self, h: &mut HighlightList) {
        match &self.data {
            CmdArg::LiteralValue(lit_val) => lit_val.highlight(h),
            CmdArg::ComputedString(computed_str) => computed_str.highlight(h),
            CmdArg::CmdCall(call) => call.data.highlight(h),
            CmdArg::ParenExpr(expr) => {
                h.push_until(&KEYWORD, self.at, expr.at);
                expr.data.highlight(h);
                h.push_remaining(&KEYWORD, self.at);
            }
            CmdArg::VarName(name) => h.push(&VAR_NAME, name.at),
            CmdArg::FnAsValue(name) => h.push(&FN_NAME, name.at),
            CmdArg::Raw(raw) => h.push(&RAW_STR, raw.at),
            CmdArg::SpreadVar(name) => h.push(&VAR_NAME, name.at), // TODO: change?
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

        if pieces.is_empty() {
            h.push(&STRING, self.at);
            return;
        }

        h.push_until(&STRING, self.at, pieces.first().unwrap().at);

        for piece in pieces {
            piece.data.highlight(h);
        }

        h.push_remaining(&STRING, self.at);
    }
}

impl Highlight for ComputedStringPiece {
    fn highlight(&self, h: &mut HighlightList) {
        match self {
            ComputedStringPiece::Literal(lit) => h.push(&STRING, lit.at),
            ComputedStringPiece::Escaped(escaped) => h.push(&ESCAPED_CHAR, escaped.at),
            ComputedStringPiece::Variable(name) => h.push(&VAR_NAME, name.at),
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
            PropAccessNature::Prop(name) => h.push(&PROP_NAME, name.at),
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
            Value::Null => h.push(&KEYWORD, self.at), // TODO: change?
            Value::Literal(lit_val) => lit_val.highlight(h),
            Value::ComputedString(computed_str) => computed_str.highlight(h),
            Value::List(list) => {
                if list.is_empty() {
                    h.push(&KEYWORD, self.at);
                    return;
                }

                h.push_until(&KEYWORD, self.at, list.first().unwrap().at);

                for item in list {
                    item.data.highlight(h);
                }

                h.push_remaining(&KEYWORD, self.at);
            }
            Value::Object(obj) => {
                // TODO: find a way to highlight the names?
                for expr in obj.values() {
                    expr.data.highlight(h);
                }
            }
            Value::Variable(name) => h.push(&VAR_NAME, name.at),
            Value::FnCall(call) => call.highlight(h),
            Value::CmdOutput(call) => call.data.highlight(h),
            Value::CmdSuccess(call) => call.data.highlight(h),
            Value::FnAsValue(name) => h.push(&FN_NAME, name.at),
            Value::Closure { signature, body } => {
                signature.highlight(h);
                body.highlight(h);
            }
        }
    }
}

impl Highlight for Eaten<LiteralValue> {
    fn highlight(&self, h: &mut HighlightList) {
        match &self.data {
            LiteralValue::Boolean(_) => h.push(&BOOL, self.at),
            LiteralValue::Integer(_) => h.push(&INT, self.at),
            LiteralValue::Float(_) => h.push(&FLOAT, self.at),
        }
    }
}

impl Highlight for Eaten<FnCall> {
    fn highlight(&self, h: &mut HighlightList) {
        let FnCall {
            is_var_name: _,
            name,
            call_args,
        } = &self.data;

        h.push_until(&KEYWORD, self.at, name.at);
        h.push(&VAR_NAME, name.at);

        for arg in &call_args.data {
            h.push_everything_until(&KEYWORD, arg.at);
            arg.data.highlight(h);
        }
    }
}

impl Highlight for FnCallArg {
    fn highlight(&self, h: &mut HighlightList) {
        match self {
            FnCallArg::Expr(expr) => expr.data.highlight(h),
            FnCallArg::CmdArg(call) => call.highlight(h),
        }
    }
}
