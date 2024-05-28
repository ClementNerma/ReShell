use std::{
    collections::{HashMap, HashSet},
    ffi::OsStr,
    path::PathBuf,
    rc::Rc,
};

use indexmap::IndexSet;
use parsy::{CodeRange, Eaten, FileId, Location, SourceFileID};
use reshell_checker::{
    CheckerOutput, Dependency, DependencyType, DevelopedCmdAliasCall, DevelopedSingleCmdCall,
};
use reshell_parser::{
    ast::{
        Block, CmdArg, CmdCall, CmdEnvVar, CmdEnvVarValue, CmdFlagArg, CmdFlagNameArg,
        CmdFlagValueArg, CmdPath, CmdPipe, CmdPipeType, CmdValueMakingArg, ComputedString,
        ComputedStringPiece, DoubleOp, ElsIf, ElsIfExpr, EscapableChar, Expr, ExprInner,
        ExprInnerChaining, ExprInnerContent, ExprOp, FlagValueSeparator, FnArg, FnCall, FnCallArg,
        FnFlagArgNames, FnSignature, Function, FunctionBody, Instruction, LiteralValue, Program,
        PropAccess, PropAccessNature, RuntimeCodeRange, RuntimeEaten, SingleCmdCall, SingleOp,
        SingleValueType, StructTypeMember, SwitchCase, Value, ValueType,
    },
    files::{FilesMap, FilesMapInner, SourceFile, SourceFileLocation},
};

use crate::{
    cmd::{CmdSingleArgResult, FlagArgValueResult},
    conf::{HistoryConf, RuntimeConf},
    context::{CallStackEntry, Scope, ScopeCmdAlias, ScopeContent, ScopeFn, ScopeVar},
    gc::{GcCell, GcOnceCell, GcReadOnlyCell},
    values::{
        CapturedDependencies, ErrorValueContent, InternalFnBody, LocatedValue, RuntimeCmdAlias,
        RuntimeFnBody, RuntimeFnSignature, RuntimeFnValue, RuntimeValue,
    },
};

pub trait ComputableSize {
    fn compute_heap_size(&self) -> usize;

    fn compute_total_size(&self) -> usize {
        std::mem::size_of_val(self) + self.compute_heap_size()
    }
}

impl ComputableSize for () {
    fn compute_heap_size(&self) -> usize {
        0
    }
}

impl ComputableSize for bool {
    fn compute_heap_size(&self) -> usize {
        0
    }
}

impl ComputableSize for i64 {
    fn compute_heap_size(&self) -> usize {
        0
    }
}

impl ComputableSize for u64 {
    fn compute_heap_size(&self) -> usize {
        0
    }
}

impl ComputableSize for usize {
    fn compute_heap_size(&self) -> usize {
        0
    }
}

impl ComputableSize for f64 {
    fn compute_heap_size(&self) -> usize {
        0
    }
}

impl ComputableSize for char {
    fn compute_heap_size(&self) -> usize {
        0
    }
}

impl ComputableSize for String {
    fn compute_heap_size(&self) -> usize {
        self.len()
    }
}

impl<T: ComputableSize> ComputableSize for Option<T> {
    fn compute_heap_size(&self) -> usize {
        match self {
            Some(value) => value.compute_heap_size(),
            None => 0,
        }
    }
}

impl<T: ComputableSize> ComputableSize for Vec<T> {
    fn compute_heap_size(&self) -> usize {
        self.iter().map(|val| val.compute_total_size()).sum()
    }
}

impl<T: ComputableSize> ComputableSize for HashSet<T> {
    fn compute_heap_size(&self) -> usize {
        self.iter().map(|val| val.compute_total_size()).sum()
    }
}

impl<T: ComputableSize> ComputableSize for IndexSet<T> {
    fn compute_heap_size(&self) -> usize {
        self.iter().map(|val| val.compute_total_size()).sum()
    }
}

impl<K: ComputableSize, V: ComputableSize> ComputableSize for HashMap<K, V> {
    fn compute_heap_size(&self) -> usize {
        self.iter()
            .map(|(key, value)| key.compute_total_size() + value.compute_total_size())
            .sum()
    }
}

impl<T: ComputableSize> ComputableSize for Box<T> {
    fn compute_heap_size(&self) -> usize {
        T::compute_total_size(self)
    }
}

impl<T: ComputableSize> ComputableSize for Rc<T> {
    fn compute_heap_size(&self) -> usize {
        T::compute_total_size(self)
    }
}

impl ComputableSize for CodeRange {
    fn compute_heap_size(&self) -> usize {
        let Self { start, len } = self;
        start.compute_heap_size() + len.compute_heap_size()
    }
}

impl ComputableSize for RuntimeCodeRange {
    fn compute_heap_size(&self) -> usize {
        match self {
            RuntimeCodeRange::Parsed(parsed) => parsed.compute_heap_size(),
            RuntimeCodeRange::Internal => 0,
        }
    }
}

impl ComputableSize for Location {
    fn compute_heap_size(&self) -> usize {
        let Self { file_id, offset } = self;
        file_id.compute_heap_size() + offset.compute_heap_size()
    }
}

impl ComputableSize for FileId {
    fn compute_heap_size(&self) -> usize {
        0
    }
}

impl ComputableSize for SourceFileID {
    fn compute_heap_size(&self) -> usize {
        0
    }
}

impl<T: ComputableSize> ComputableSize for GcCell<T> {
    fn compute_heap_size(&self) -> usize {
        // TODO: this doesn't take content of Rc<RefCell<>> into account!
        self.with_ref(|val| val.compute_total_size())
    }
}

impl<T: ComputableSize> ComputableSize for GcReadOnlyCell<T> {
    fn compute_heap_size(&self) -> usize {
        // TODO: this doesn't take content of Rc<RefCell<>> into account!
        T::compute_total_size(self)
    }
}

impl<T: ComputableSize> ComputableSize for GcOnceCell<T> {
    fn compute_heap_size(&self) -> usize {
        // TODO: this doesn't take content of Rc<RefCell<>> into account!
        Option::compute_total_size(&self.get())
    }
}

impl ComputableSize for CmdSingleArgResult {
    fn compute_heap_size(&self) -> usize {
        match self {
            CmdSingleArgResult::Basic(value) => value.compute_heap_size(),
            CmdSingleArgResult::Flag { name, value } => {
                name.compute_heap_size() + value.compute_heap_size()
            }
            CmdSingleArgResult::RestSeparator(rest_sep) => rest_sep.compute_heap_size(),
        }
    }
}

impl ComputableSize for LocatedValue {
    fn compute_heap_size(&self) -> usize {
        let Self { value, from } = self;
        value.compute_heap_size() + from.compute_heap_size()
    }
}

impl<T: ComputableSize> ComputableSize for Eaten<T> {
    fn compute_heap_size(&self) -> usize {
        let Self { at, data } = self;
        at.compute_heap_size() + data.compute_heap_size()
    }
}

impl<T: ComputableSize> ComputableSize for RuntimeEaten<T> {
    fn compute_heap_size(&self) -> usize {
        match self {
            RuntimeEaten::Parsed(data) => data.compute_heap_size(),
            RuntimeEaten::Internal(data) => data.compute_heap_size(),
        }
    }
}

impl ComputableSize for CmdFlagNameArg {
    fn compute_heap_size(&self) -> usize {
        match self {
            CmdFlagNameArg::Short(short) => short.compute_heap_size(),
            CmdFlagNameArg::Long(long) => long.compute_heap_size(),
        }
    }
}

impl ComputableSize for FlagArgValueResult {
    fn compute_heap_size(&self) -> usize {
        let Self { value, value_sep } = self;
        value.compute_heap_size() + value_sep.compute_heap_size()
    }
}

impl ComputableSize for FlagValueSeparator {
    fn compute_heap_size(&self) -> usize {
        match self {
            FlagValueSeparator::Space => 0,
            FlagValueSeparator::Equal => 0,
        }
    }
}

impl ComputableSize for RuntimeFnValue {
    fn compute_heap_size(&self) -> usize {
        let Self {
            signature,
            body,
            parent_scopes,
            captured_deps,
        } = self;

        signature.compute_heap_size()
            + body.compute_heap_size()
            + parent_scopes.compute_heap_size()
            + captured_deps.compute_heap_size()
    }
}

impl ComputableSize for RuntimeFnSignature {
    fn compute_heap_size(&self) -> usize {
        match self {
            RuntimeFnSignature::Shared(signature) => signature.compute_heap_size(),
            RuntimeFnSignature::Owned(signature) => signature.compute_heap_size(),
        }
    }
}

impl ComputableSize for FnSignature {
    fn compute_heap_size(&self) -> usize {
        let Self { args, ret_type } = self;
        args.compute_heap_size() + ret_type.compute_heap_size()
    }
}

impl ComputableSize for FnArg {
    fn compute_heap_size(&self) -> usize {
        match self {
            FnArg::Positional {
                name,
                is_optional,
                typ,
            } => {
                name.compute_heap_size() + is_optional.compute_heap_size() + typ.compute_heap_size()
            }

            FnArg::PresenceFlag { names } => names.compute_heap_size(),

            FnArg::NormalFlag {
                names,
                is_optional,
                typ,
            } => {
                names.compute_heap_size()
                    + is_optional.compute_heap_size()
                    + typ.compute_heap_size()
            }

            FnArg::Rest { name } => name.compute_heap_size(),
        }
    }
}

impl ComputableSize for ValueType {
    fn compute_heap_size(&self) -> usize {
        match self {
            ValueType::Single(single) => single.compute_heap_size(),
            ValueType::Union(union) => union.compute_heap_size(),
        }
    }
}

impl ComputableSize for SingleValueType {
    fn compute_heap_size(&self) -> usize {
        match self {
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
            | SingleValueType::UntypedStruct
            | SingleValueType::ArgSpread => 0,

            SingleValueType::TypedStruct(typed) => typed.compute_heap_size(),
            SingleValueType::Function(func) => func.compute_heap_size(),
            SingleValueType::TypeAlias(alias) => alias.compute_heap_size(),
        }
    }
}

impl ComputableSize for StructTypeMember {
    fn compute_heap_size(&self) -> usize {
        let Self { name, typ } = self;
        name.compute_heap_size() + typ.compute_heap_size()
    }
}

impl ComputableSize for FnFlagArgNames {
    fn compute_heap_size(&self) -> usize {
        match self {
            FnFlagArgNames::ShortFlag(short) => short.compute_heap_size(),
            FnFlagArgNames::LongFlag(long) => long.compute_heap_size(),
            FnFlagArgNames::LongAndShortFlag { long, short } => {
                long.compute_heap_size() + short.compute_heap_size()
            }
        }
    }
}

impl ComputableSize for RuntimeFnBody {
    fn compute_heap_size(&self) -> usize {
        match self {
            RuntimeFnBody::Block(block) => block.compute_heap_size(),
            RuntimeFnBody::Internal(internal) => std::mem::size_of_val::<InternalFnBody>(internal),
        }
    }
}

impl ComputableSize for CapturedDependencies {
    fn compute_heap_size(&self) -> usize {
        let Self {
            vars,
            fns,
            cmd_aliases,
        } = self;

        vars.compute_heap_size() + fns.compute_heap_size() + cmd_aliases.compute_heap_size()
    }
}

impl ComputableSize for Dependency {
    fn compute_heap_size(&self) -> usize {
        let Self {
            name,
            declared_name_at,
            dep_type,
        } = self;

        name.compute_heap_size()
            + declared_name_at.compute_heap_size()
            + dep_type.compute_heap_size()
    }
}

impl ComputableSize for DependencyType {
    fn compute_heap_size(&self) -> usize {
        match self {
            DependencyType::Variable | DependencyType::Function | DependencyType::CmdAlias => 0,
        }
    }
}

impl ComputableSize for ScopeVar {
    fn compute_heap_size(&self) -> usize {
        let Self {
            name_at,
            is_mut,
            value,
        } = self;
        name_at.compute_heap_size() + is_mut.compute_heap_size() + value.compute_heap_size()
    }
}

impl ComputableSize for ScopeFn {
    fn compute_heap_size(&self) -> usize {
        let Self { name_at, value } = self;
        name_at.compute_heap_size() + value.compute_heap_size()
    }
}

impl ComputableSize for ScopeCmdAlias {
    fn compute_heap_size(&self) -> usize {
        let Self { name_at, value } = self;
        name_at.compute_heap_size() + value.compute_heap_size()
    }
}

impl ComputableSize for RuntimeCmdAlias {
    fn compute_heap_size(&self) -> usize {
        let Self {
            name_declared_at,
            alias_content,
            parent_scopes,
            captured_deps,
        } = self;

        name_declared_at.compute_heap_size()
            + alias_content.compute_heap_size()
            + parent_scopes.compute_heap_size()
            + captured_deps.compute_heap_size()
    }
}

impl ComputableSize for CmdCall {
    fn compute_heap_size(&self) -> usize {
        let Self { base, pipes } = self;
        base.compute_heap_size() + pipes.compute_heap_size()
    }
}

impl ComputableSize for CmdPipe {
    fn compute_heap_size(&self) -> usize {
        let Self { pipe_type, cmd } = self;
        pipe_type.compute_heap_size() + cmd.compute_heap_size()
    }
}

impl ComputableSize for CmdPipeType {
    fn compute_heap_size(&self) -> usize {
        match self {
            CmdPipeType::Stdout | CmdPipeType::Stderr | CmdPipeType::Value => 0,
        }
    }
}

impl ComputableSize for SingleCmdCall {
    fn compute_heap_size(&self) -> usize {
        let Self {
            env_vars,
            path,
            args,
        } = self;

        env_vars.compute_heap_size() + path.compute_heap_size() + args.compute_heap_size()
    }
}

impl ComputableSize for CmdEnvVar {
    fn compute_heap_size(&self) -> usize {
        let Self { name, value } = self;
        name.compute_heap_size() + value.compute_heap_size()
    }
}

impl ComputableSize for CmdEnvVarValue {
    fn compute_heap_size(&self) -> usize {
        match self {
            CmdEnvVarValue::Raw(raw) => raw.compute_heap_size(),
            CmdEnvVarValue::ComputedString(c_str) => c_str.compute_heap_size(),
            CmdEnvVarValue::Expr(expr) => expr.compute_heap_size(),
        }
    }
}

impl ComputableSize for CmdPath {
    fn compute_heap_size(&self) -> usize {
        match self {
            CmdPath::RawString(raw) => raw.compute_heap_size(),
            CmdPath::Direct(direct) => direct.compute_heap_size(),
            CmdPath::Expr(expr) => expr.compute_heap_size(),
            CmdPath::ComputedString(c_str) => c_str.compute_heap_size(),
        }
    }
}

impl ComputableSize for CmdArg {
    fn compute_heap_size(&self) -> usize {
        match self {
            CmdArg::ValueMaking(value_making) => value_making.compute_heap_size(),
            CmdArg::Flag(flag) => flag.compute_heap_size(),
            CmdArg::SpreadVar(spread_var) => spread_var.compute_heap_size(),
            CmdArg::RestSeparator(rest_sep) => rest_sep.compute_heap_size(),
        }
    }
}

impl ComputableSize for CmdFlagArg {
    fn compute_heap_size(&self) -> usize {
        let Self { name, value } = self;
        name.compute_heap_size() + value.compute_heap_size()
    }
}

impl ComputableSize for CmdFlagValueArg {
    fn compute_heap_size(&self) -> usize {
        let Self { value_sep, value } = self;
        value_sep.compute_heap_size() + value.compute_heap_size()
    }
}

impl ComputableSize for CmdValueMakingArg {
    fn compute_heap_size(&self) -> usize {
        match self {
            CmdValueMakingArg::LiteralValue(lit) => lit.compute_heap_size(),
            CmdValueMakingArg::ComputedString(c_str) => c_str.compute_heap_size(),
            CmdValueMakingArg::CmdCall(cmd_call) => cmd_call.compute_heap_size(),
            CmdValueMakingArg::ParenExpr(expr) => expr.compute_heap_size(),
            CmdValueMakingArg::VarName(var_name) => var_name.compute_heap_size(),
            CmdValueMakingArg::Raw(raw) => raw.compute_heap_size(),
        }
    }
}

impl ComputableSize for LiteralValue {
    fn compute_heap_size(&self) -> usize {
        match self {
            LiteralValue::Boolean(bool) => bool.compute_heap_size(),
            LiteralValue::Integer(int) => int.compute_heap_size(),
            LiteralValue::Float(float) => float.compute_heap_size(),
        }
    }
}

impl ComputableSize for ComputedString {
    fn compute_heap_size(&self) -> usize {
        let Self { pieces } = self;
        pieces.compute_heap_size()
    }
}

impl ComputableSize for ComputedStringPiece {
    fn compute_heap_size(&self) -> usize {
        match self {
            ComputedStringPiece::Literal(lit) => lit.compute_heap_size(),
            ComputedStringPiece::Escaped(escaped) => escaped.compute_heap_size(),
            ComputedStringPiece::Variable(var) => var.compute_heap_size(),
            ComputedStringPiece::Expr(expr) => expr.compute_heap_size(),
            ComputedStringPiece::CmdCall(cmd_call) => cmd_call.compute_heap_size(),
        }
    }
}

impl ComputableSize for EscapableChar {
    fn compute_heap_size(&self) -> usize {
        match self {
            EscapableChar::Newline => 0,
            EscapableChar::CarriageReturn => 0,
            EscapableChar::DoubleQuote => 0,
            EscapableChar::Backslash => 0,
            EscapableChar::DollarSign => 0,
        }
    }
}

impl ComputableSize for Expr {
    fn compute_heap_size(&self) -> usize {
        let Self { inner, right_ops } = self;
        inner.compute_heap_size() + right_ops.compute_heap_size()
    }
}

impl ComputableSize for ExprInner {
    fn compute_heap_size(&self) -> usize {
        let Self { content, chainings } = self;

        content.compute_heap_size() + chainings.compute_heap_size()
    }
}

impl ComputableSize for ExprInnerContent {
    fn compute_heap_size(&self) -> usize {
        match self {
            ExprInnerContent::SingleOp { op, right } => {
                op.compute_heap_size() + right.compute_heap_size()
            }

            ExprInnerContent::ParenExpr(expr) => expr.compute_heap_size(),

            ExprInnerContent::Value(value) => value.compute_heap_size(),

            ExprInnerContent::FnAsValue(name) => name.compute_heap_size(),

            ExprInnerContent::Ternary {
                cond,
                body,
                elsif,
                els,
            } => {
                cond.compute_heap_size()
                    + body.compute_heap_size()
                    + elsif.compute_heap_size()
                    + els.compute_heap_size()
            }

            ExprInnerContent::Try {
                fn_call,
                catch_var,
                catch_expr,
            } => {
                fn_call.compute_heap_size()
                    + catch_var.compute_heap_size()
                    + catch_expr.compute_heap_size()
            }
        }
    }
}

impl ComputableSize for ExprInnerChaining {
    fn compute_heap_size(&self) -> usize {
        match self {
            ExprInnerChaining::PropAccess(prop_acc) => prop_acc.compute_heap_size(),
            ExprInnerChaining::MethodCall(method_call) => method_call.compute_heap_size(),
        }
    }
}

impl ComputableSize for SingleOp {
    fn compute_heap_size(&self) -> usize {
        match self {
            SingleOp::Neg => 0,
        }
    }
}

impl ComputableSize for ExprOp {
    fn compute_heap_size(&self) -> usize {
        let Self { op, with } = self;
        op.compute_heap_size() + with.compute_heap_size()
    }
}

impl ComputableSize for DoubleOp {
    fn compute_heap_size(&self) -> usize {
        match self {
            DoubleOp::Add
            | DoubleOp::Sub
            | DoubleOp::Mul
            | DoubleOp::Div
            | DoubleOp::And
            | DoubleOp::Or
            | DoubleOp::Eq
            | DoubleOp::Neq
            | DoubleOp::Lt
            | DoubleOp::Lte
            | DoubleOp::Gt
            | DoubleOp::Gte
            | DoubleOp::NullFallback => 0,
        }
    }
}

impl ComputableSize for PropAccess {
    fn compute_heap_size(&self) -> usize {
        let Self { nature, nullable } = self;
        nature.compute_heap_size() + nullable.compute_heap_size()
    }
}

impl ComputableSize for PropAccessNature {
    fn compute_heap_size(&self) -> usize {
        match self {
            PropAccessNature::Key(key) => key.compute_heap_size(),
            PropAccessNature::Prop(prop) => prop.compute_heap_size(),
        }
    }
}

impl ComputableSize for Value {
    fn compute_heap_size(&self) -> usize {
        match self {
            Value::Null => 0,
            Value::Literal(lit) => lit.compute_heap_size(),
            Value::ComputedString(c_str) => c_str.compute_heap_size(),
            Value::List(list) => list.compute_heap_size(),
            Value::Struct(members) => members.compute_heap_size(),
            Value::Variable(var_name) => var_name.compute_heap_size(),
            Value::FnCall(fn_call) => fn_call.compute_heap_size(),
            Value::CmdOutput(cmd_call) => cmd_call.compute_heap_size(),
            Value::CmdSuccess(cmd_call) => cmd_call.compute_heap_size(),
            Value::FnAsValue(fn_as_val) => fn_as_val.compute_heap_size(),
            Value::Closure(closure) => closure.compute_heap_size(),
        }
    }
}

impl ComputableSize for Function {
    fn compute_heap_size(&self) -> usize {
        let Self { signature, body } = self;
        signature.compute_heap_size() + body.compute_heap_size()
    }
}

impl ComputableSize for FunctionBody {
    fn compute_heap_size(&self) -> usize {
        match self {
            FunctionBody::Expr(expr) => expr.compute_heap_size(),
            FunctionBody::Block(block) => block.compute_heap_size(),
        }
    }
}

impl ComputableSize for FnCall {
    fn compute_heap_size(&self) -> usize {
        let Self {
            is_var_name,
            name,
            call_args,
        } = self;

        is_var_name.compute_heap_size() + name.compute_heap_size() + call_args.compute_heap_size()
    }
}

impl ComputableSize for FnCallArg {
    fn compute_heap_size(&self) -> usize {
        match self {
            FnCallArg::Expr(expr) => expr.compute_heap_size(),
            FnCallArg::Flag { name, value } => name.compute_heap_size() + value.compute_heap_size(),
            FnCallArg::CmdArg(cmd_arg) => cmd_arg.compute_heap_size(),
        }
    }
}

impl ComputableSize for ElsIfExpr {
    fn compute_heap_size(&self) -> usize {
        let Self { cond, body } = self;
        cond.compute_heap_size() + body.compute_heap_size()
    }
}

impl ComputableSize for Block {
    fn compute_heap_size(&self) -> usize {
        let Self { instructions } = self;
        instructions.compute_heap_size()
    }
}

impl ComputableSize for Instruction {
    fn compute_heap_size(&self) -> usize {
        match self {
            Instruction::DeclareVar {
                name,
                init_expr,
                mutable,
            } => {
                name.compute_heap_size()
                    + init_expr.compute_heap_size()
                    + mutable.compute_heap_size()
            }

            Instruction::AssignVar {
                name,
                prop_acc,
                list_push,
                expr,
            } => {
                name.compute_heap_size()
                    + prop_acc.compute_heap_size()
                    + list_push.compute_heap_size()
                    + expr.compute_heap_size()
            }

            Instruction::IfCond {
                cond,
                body,
                elsif,
                els,
            } => {
                cond.compute_heap_size()
                    + body.compute_heap_size()
                    + elsif.compute_heap_size()
                    + els.compute_heap_size()
            }

            Instruction::ForLoop {
                iter_var,
                iter_on,
                body,
            } => {
                iter_var.compute_heap_size()
                    + iter_on.compute_heap_size()
                    + body.compute_heap_size()
            }

            Instruction::ForLoopKeyed {
                key_iter_var,
                value_iter_var,
                iter_on,
                body,
            } => {
                key_iter_var.compute_heap_size()
                    + value_iter_var.compute_heap_size()
                    + iter_on.compute_heap_size()
                    + body.compute_heap_size()
            }

            Instruction::WhileLoop { cond, body } => {
                cond.compute_heap_size() + body.compute_heap_size()
            }

            Instruction::LoopContinue => 0,

            Instruction::LoopBreak => 0,

            Instruction::Switch { expr, cases } => {
                expr.compute_heap_size() + cases.compute_heap_size()
            }

            Instruction::FnDecl { name, content } => {
                name.compute_heap_size() + content.compute_heap_size()
            }

            Instruction::FnReturn { expr } => expr.compute_heap_size(),

            Instruction::Throw(value) => value.compute_heap_size(),

            Instruction::Try {
                call,
                catch_var,
                catch_body,
            } => {
                call.compute_heap_size()
                    + catch_var.compute_heap_size()
                    + catch_body.compute_heap_size()
            }

            Instruction::CmdAliasDecl { name, content } => {
                name.compute_heap_size() + content.compute_heap_size()
            }

            Instruction::TypeAliasDecl { name, content } => {
                name.compute_heap_size() + content.compute_heap_size()
            }

            Instruction::DoBlock(block) => block.compute_heap_size(),

            Instruction::FnCall(fn_call) => fn_call.compute_heap_size(),

            Instruction::CmdCall(cmd_call) => cmd_call.compute_heap_size(),

            Instruction::Imported(program) => program.compute_heap_size(),
        }
    }
}

impl ComputableSize for ElsIf {
    fn compute_heap_size(&self) -> usize {
        let Self { cond, body } = self;
        cond.compute_heap_size() + body.compute_heap_size()
    }
}

impl ComputableSize for SwitchCase {
    fn compute_heap_size(&self) -> usize {
        let Self { cond, body } = self;
        cond.compute_heap_size() + body.compute_heap_size()
    }
}

impl ComputableSize for Program {
    fn compute_heap_size(&self) -> usize {
        let Self { content } = self;
        content.compute_heap_size()
    }
}

impl ComputableSize for RuntimeValue {
    fn compute_heap_size(&self) -> usize {
        match self {
            RuntimeValue::Null => 0,
            RuntimeValue::Bool(bool) => bool.compute_heap_size(),
            RuntimeValue::Int(int) => int.compute_heap_size(),
            RuntimeValue::Float(float) => float.compute_heap_size(),
            RuntimeValue::String(string) => string.compute_heap_size(),
            RuntimeValue::Range { from, to } => from.compute_heap_size() + to.compute_heap_size(),
            RuntimeValue::Error(err) => err.compute_heap_size(),
            RuntimeValue::List(values) => values.compute_heap_size(),
            RuntimeValue::Map(map) => map.compute_heap_size(),
            RuntimeValue::Struct(members) => members.compute_heap_size(),
            RuntimeValue::Function(func) => func.compute_heap_size(),
            RuntimeValue::ArgSpread(spread) => spread.compute_heap_size(),
        }
    }
}

impl ComputableSize for ErrorValueContent {
    fn compute_heap_size(&self) -> usize {
        let Self { at, msg } = self;
        at.compute_heap_size() + msg.compute_heap_size()
    }
}

impl ComputableSize for RuntimeConf {
    fn compute_heap_size(&self) -> usize {
        let Self {
            initial_home_dir,
            call_stack_limit,
            history,
        } = self;

        initial_home_dir.compute_heap_size()
            + call_stack_limit.compute_heap_size()
            + history.compute_heap_size()
    }
}

impl ComputableSize for HistoryConf {
    fn compute_heap_size(&self) -> usize {
        let Self {
            enabled,
            custom_location,
        } = self;

        enabled.compute_heap_size() + custom_location.compute_heap_size()
    }
}

impl ComputableSize for PathBuf {
    fn compute_heap_size(&self) -> usize {
        self.as_os_str().compute_heap_size()
    }
}

impl ComputableSize for OsStr {
    fn compute_heap_size(&self) -> usize {
        self.len()
    }
}

impl ComputableSize for Scope {
    fn compute_heap_size(&self) -> usize {
        let Self {
            id,
            range,
            content,
            parent_scopes,
            deps_scope,
            previous_scope,
            call_stack,
        } = self;

        id.compute_heap_size()
            + range.compute_heap_size()
            + content.compute_heap_size()
            + parent_scopes.compute_heap_size()
            + deps_scope.compute_heap_size()
            + previous_scope.compute_heap_size()
            + call_stack.compute_heap_size()
    }
}

impl ComputableSize for ScopeContent {
    fn compute_heap_size(&self) -> usize {
        let Self {
            vars,
            fns,
            cmd_aliases,
        } = self;

        vars.compute_heap_size() + fns.compute_heap_size() + cmd_aliases.compute_heap_size()
    }
}

impl ComputableSize for CallStackEntry {
    fn compute_heap_size(&self) -> usize {
        let Self { fn_called_at } = self;
        fn_called_at.compute_heap_size()
    }
}

impl ComputableSize for FilesMap {
    fn compute_heap_size(&self) -> usize {
        self.with_inner(|inner| {
            let FilesMapInner {
                counter,
                file_loader: _,
                map,
            } = inner;

            // NOTE: file_loader is not accounted for as we cannot compute the size of a
            // `Box<dyn ...>`

            counter.compute_heap_size() + map.compute_heap_size()
        })
    }
}

impl ComputableSize for SourceFile {
    fn compute_heap_size(&self) -> usize {
        let Self {
            id,
            location,
            content,
        } = self;
        id.compute_heap_size() + location.compute_heap_size() + content.compute_heap_size()
    }
}

impl ComputableSize for SourceFileLocation {
    fn compute_heap_size(&self) -> usize {
        match self {
            SourceFileLocation::CustomName(name) => name.compute_heap_size(),
            SourceFileLocation::RealFile(path) => path.compute_heap_size(),
        }
    }
}

impl ComputableSize for DevelopedSingleCmdCall {
    fn compute_heap_size(&self) -> usize {
        let Self {
            at,
            is_function,
            developed_aliases,
        } = self;

        at.compute_heap_size()
            + is_function.compute_heap_size()
            + developed_aliases.compute_heap_size()
    }
}

impl ComputableSize for DevelopedCmdAliasCall {
    fn compute_heap_size(&self) -> usize {
        let Self {
            content_at,
            called_alias_name,
        } = self;

        content_at.compute_heap_size() + called_alias_name.compute_heap_size()
    }
}

impl ComputableSize for CheckerOutput {
    fn compute_heap_size(&self) -> usize {
        let Self {
            deps,
            type_aliases_decl,
            type_aliases_usages,
            type_aliases_decl_by_scope,
            fn_signatures,
            fn_bodies,
            cmd_aliases,
            cmd_calls,
        } = self;

        deps.compute_heap_size()
            + type_aliases_decl.compute_heap_size()
            + type_aliases_usages.compute_heap_size()
            + type_aliases_decl_by_scope.compute_heap_size()
            + fn_signatures.compute_heap_size()
            + fn_bodies.compute_heap_size()
            + cmd_aliases.compute_heap_size()
            + cmd_calls.compute_heap_size()
    }
}
