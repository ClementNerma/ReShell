use std::collections::HashMap;

use parsy::{CodeRange, CodeRangeComparisonError, Eaten};

#[derive(Debug, Clone)]
pub struct Program {
    pub content: Eaten<Block>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub instructions: Vec<Eaten<Instruction>>,
    pub code_range: CodeRange,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Comment {
        content: Eaten<String>,
    },

    DeclareVar {
        name: Eaten<String>,
        mutable: Option<Eaten<()>>,
        init_expr: Option<Eaten<Expr>>,
    },

    AssignVar {
        name: Eaten<String>,
        prop_acc: Vec<Eaten<PropAccessNature>>,
        list_push: Option<Eaten<()>>,
        expr: Eaten<Expr>,
    },

    IfCond {
        cond: Eaten<Expr>,
        body: Eaten<Block>,
        elsif: Vec<Eaten<ElsIf>>,
        els: Option<Eaten<Block>>,
    },

    ForLoop {
        iter_var: Eaten<String>,
        iter_on: Eaten<Expr>,
        body: Eaten<Block>,
    },

    ForLoopKeyed {
        key_iter_var: Eaten<String>,
        value_iter_var: Eaten<String>,
        iter_on: Eaten<Expr>,
        body: Eaten<Block>,
    },

    WhileLoop {
        cond: Eaten<Expr>,
        body: Eaten<Block>,
    },

    LoopContinue,

    LoopBreak,

    Switch {
        expr: Eaten<Expr>,
        cases: Vec<SwitchCase>,
    },

    FnDecl {
        name: Eaten<String>,
        content: Function,
    },

    FnReturn {
        expr: Option<Eaten<Expr>>,
    },

    Throw(Eaten<Expr>),

    Try {
        call: Eaten<FnCall>,
        catch_var: Eaten<String>,
        catch_body: Eaten<Block>,
    },

    CmdAliasDecl {
        name: Eaten<String>,
        content: Eaten<SingleCmdCall>,
    },

    TypeAliasDecl {
        name: Eaten<String>,
        content: Eaten<ValueType>,
    },

    BaseBlock(Eaten<Block>),

    FnCall(Eaten<FnCall>),

    CmdCall(Eaten<CmdCall>),

    Imported(Eaten<Program>),
}

#[derive(Debug, Clone)]
pub struct ElsIf {
    pub cond: Eaten<Expr>,
    pub body: Eaten<Block>,
}

#[derive(Debug, Clone)]
pub struct SwitchCase {
    pub cond: Eaten<Expr>,
    pub body: Eaten<Block>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub signature: Eaten<FnSignature>,
    pub body: Eaten<Block>,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub inner: Eaten<ExprInner>,
    pub right_ops: Vec<ExprOp>,
}

#[derive(Debug, Clone)]
pub struct ExprInner {
    pub content: Eaten<ExprInnerContent>,
    pub prop_acc: Vec<Eaten<PropAccess>>,
}

#[derive(Debug, Clone)]
pub enum ExprInnerContent {
    SingleOp {
        op: Eaten<SingleOp>,
        right: Eaten<Box<ExprInnerContent>>,
    },
    ParenExpr(Eaten<Box<Expr>>),
    Value(Eaten<Value>),
    Ternary {
        cond: Eaten<Box<Expr>>,
        body: Eaten<Box<Expr>>,
        elsif: Vec<Eaten<ElsIfExpr>>,
        els: Eaten<Box<Expr>>,
    },
    Try {
        fn_call: Eaten<FnCall>,
        catch_var: Eaten<String>,
        catch_expr: Eaten<Box<Expr>>,
    },
}

#[derive(Debug, Clone)]
pub struct ElsIfExpr {
    pub cond: Eaten<Box<Expr>>,
    pub body: Eaten<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub struct PropAccess {
    pub nature: Eaten<PropAccessNature>,
    pub nullable: bool,
}

#[derive(Debug, Clone)]
pub enum PropAccessNature {
    Key(Box<Eaten<Expr>>),
    Prop(Eaten<String>),
}

#[derive(Debug, Clone)]
pub struct ExprOp {
    pub op: Eaten<DoubleOp>,
    pub with: Box<Eaten<ExprInner>>,
}

#[derive(Debug, Clone)]
pub enum Value {
    Null,
    Literal(Eaten<LiteralValue>),
    ComputedString(Eaten<ComputedString>),
    List(Vec<Eaten<Expr>>),
    Struct(HashMap<String, Eaten<Expr>>),
    Variable(Eaten<String>),
    FnCall(Eaten<FnCall>),
    CmdOutput(Eaten<CmdCall>),
    CmdSuccess(Eaten<CmdCall>),
    FnAsValue(Eaten<String>),
    Closure(Function),
}

#[derive(Debug, Clone)]
pub enum ValueType {
    Single(RuntimeEaten<SingleValueType>),
    Union(Vec<RuntimeEaten<SingleValueType>>),
}

#[derive(Debug, Clone)]
pub enum SingleValueType {
    Any,
    Null,
    Bool,
    Int,
    Float,
    String,
    List,
    Range,
    Map,
    Error,
    UntypedStruct,
    TypedStruct(Vec<RuntimeEaten<StructTypeMember>>),
    Function(RuntimeEaten<FnSignature>),
    TypeAlias(Eaten<String>),
    ArgSpread,
}

#[derive(Debug, Clone)]
pub struct StructTypeMember {
    pub name: RuntimeEaten<String>,
    pub typ: RuntimeEaten<ValueType>,
}

#[derive(Debug, Clone)]
pub struct FnSignature {
    pub args: RuntimeEaten<Vec<FnArg>>,
    pub ret_type: Option<RuntimeEaten<Box<ValueType>>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SingleOp {
    Neg,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DoubleOp {
    Add,          // +
    Sub,          // -
    Mul,          // *
    Div,          // /
    And,          // &&
    Or,           // ||
    Eq,           // ==
    Neq,          // !=
    Lt,           // <
    Lte,          // <=
    Gt,           // >
    Gte,          // <=
    NullFallback, // ??
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
    Boolean(bool),
    Integer(i64),
    Float(f64),
    // String(String),
}

#[derive(Debug, Clone)]

pub struct ComputedString {
    pub pieces: Vec<Eaten<ComputedStringPiece>>,
}

#[derive(Debug, Clone)]
pub enum ComputedStringPiece {
    Literal(String),
    Escaped(EscapableChar),
    Variable(Eaten<String>),
    Expr(Eaten<Expr>),
    CmdCall(Eaten<CmdCall>),
}

#[derive(Debug, Clone, Copy)]
pub enum EscapableChar {
    Newline,
    DoubleQuote,
    Backslash,
    DollarSign,
}

#[derive(Debug, Clone)]
pub struct CmdCall {
    pub base: Eaten<SingleCmdCall>,
    pub pipes: Vec<CmdPipe>,
}

#[derive(Debug, Clone)]
pub struct SingleCmdCall {
    pub env_vars: Eaten<Vec<Eaten<CmdEnvVar>>>,
    pub path: Eaten<CmdPath>,
    pub args: Eaten<Vec<Eaten<CmdArg>>>,
}

#[derive(Debug, Clone)]
pub struct CmdEnvVar {
    pub name: Eaten<String>,
    pub value: Eaten<CmdEnvVarValue>,
}

#[derive(Debug, Clone)]
pub enum CmdEnvVarValue {
    Raw(Eaten<String>),
    ComputedString(Eaten<ComputedString>),
    Expr(Eaten<Expr>),
}

#[derive(Debug, Clone)]
pub enum CmdPath {
    RawString(Eaten<String>),
    Direct(Eaten<String>),
    Expr(Eaten<Box<Expr>>),
    ComputedString(Eaten<ComputedString>),
}

#[derive(Debug, Clone)]
pub enum CmdArg {
    ValueMaking(CmdValueMakingArg),
    Flag(CmdFlagArg),
    SpreadVar(Eaten<String>),
    RestSeparator(Eaten<()>),
}

#[derive(Debug, Clone)]
pub enum CmdValueMakingArg {
    LiteralValue(Eaten<LiteralValue>),
    ComputedString(Eaten<ComputedString>),
    CmdCall(Eaten<CmdCall>),
    ParenExpr(Eaten<Expr>),
    VarName(Eaten<String>),
    FnAsValue(Eaten<String>),
    Raw(Eaten<String>),
}

#[derive(Debug, Clone)]
pub struct CmdFlagArg {
    pub name: Eaten<CmdFlagNameArg>,
    pub value: Option<CmdFlagValueArg>,
}

#[derive(Debug, Clone)]
pub enum CmdFlagNameArg {
    Short(char),
    Long(String),
}

#[derive(Debug, Clone)]
pub struct CmdFlagValueArg {
    pub value_sep: FlagValueSeparator,
    pub value: Eaten<CmdValueMakingArg>,
}

#[derive(Debug, Clone, Copy)]
pub enum FlagValueSeparator {
    Space,
    Equal,
}

#[derive(Debug, Clone)]
pub struct CmdPipe {
    pub pipe_type: Eaten<CmdPipeType>,
    pub cmd: Eaten<SingleCmdCall>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CmdPipeType {
    Stdout,
    Stderr,
    // Both,
}

#[derive(Debug, Clone)]
pub enum FnArg {
    Positional {
        name: RuntimeEaten<String>,
        is_optional: bool,
        typ: Option<RuntimeEaten<ValueType>>,
    },
    PresenceFlag {
        names: FnFlagArgNames,
    },
    NormalFlag {
        names: FnFlagArgNames,
        is_optional: bool,
        typ: Option<RuntimeEaten<ValueType>>,
    },
    Rest {
        name: RuntimeEaten<String>,
    },
}

#[derive(Debug, Clone)]
pub enum FnFlagArgNames {
    ShortFlag(RuntimeEaten<char>),
    LongFlag(RuntimeEaten<String>),
    LongAndShortFlag {
        long: RuntimeEaten<String>,
        short: RuntimeEaten<char>,
    },
}

#[derive(Debug, Clone)]
pub struct FnCall {
    pub is_var_name: bool,
    pub name: Eaten<String>,
    pub call_args: Eaten<Vec<Eaten<FnCallArg>>>,
}

#[derive(Debug, Clone)]
pub enum FnCallArg {
    Expr(Eaten<Expr>),
    Flag(Eaten<CmdFlagArg>),
    CmdArg(Eaten<CmdArg>),
}

/// A token that's either eaten from a real input or generated at runtime
#[derive(Debug, Clone, Copy, Hash)]
pub enum RuntimeEaten<T> {
    Eaten(Eaten<T>),
    Internal(T),
}

impl<T> RuntimeEaten<T> {
    pub fn at(&self) -> RuntimeCodeRange {
        match self {
            RuntimeEaten::Eaten(eaten) => RuntimeCodeRange::Parsed(eaten.at),
            RuntimeEaten::Internal(_) => RuntimeCodeRange::Internal,
        }
    }

    pub fn data(&self) -> &T {
        match &self {
            Self::Eaten(eaten) => &eaten.data,
            Self::Internal(raw) => raw,
        }
    }

    pub fn eaten(&self) -> Option<&Eaten<T>> {
        match self {
            RuntimeEaten::Eaten(eaten) => Some(eaten),
            RuntimeEaten::Internal(_) => None,
        }
    }

    pub fn map<U>(self, func: impl FnOnce(T) -> U) -> RuntimeEaten<U> {
        match self {
            RuntimeEaten::Eaten(eaten) => RuntimeEaten::Eaten(eaten.map(func)),
            RuntimeEaten::Internal(data) => RuntimeEaten::Internal(func(data)),
        }
    }
}

/// Either a [`CodeRange`] or an internal location
#[derive(Debug, Clone, Copy)]
pub enum RuntimeCodeRange {
    Parsed(CodeRange),
    Internal,
}

impl RuntimeCodeRange {
    pub fn real(&self) -> Option<CodeRange> {
        match self {
            RuntimeCodeRange::Parsed(range) => Some(*range),
            RuntimeCodeRange::Internal => None,
        }
    }

    pub fn contains(&self, range: CodeRange) -> Result<bool, CodeRangeComparisonError> {
        match self {
            RuntimeCodeRange::Parsed(c) => c.contains(range),
            RuntimeCodeRange::Internal => Ok(false),
        }
    }
}

impl From<CodeRange> for RuntimeCodeRange {
    fn from(range: CodeRange) -> Self {
        Self::Parsed(range)
    }
}
