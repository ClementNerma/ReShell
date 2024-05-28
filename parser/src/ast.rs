use std::collections::HashMap;

use parsy::{Eaten, MaybeEaten};

#[derive(Debug, Clone)]
pub struct Program {
    pub content: Eaten<Block>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub id: u64,
    pub visible_scopes: Vec<u64>,
    pub instructions: Vec<Eaten<Instruction>>,
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
        signature: FnSignature,
        body: Eaten<Block>,
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

    CmdCall(Eaten<CmdCall>),
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
    // TODO
    // Try {
    //     call: Eaten<FnCall>,
    //     catch_var: Eaten<String>,
    //     catch_body: Eaten<Box<Expr>>,
    // },
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
    Object(HashMap<String, Eaten<Expr>>),
    Variable(Eaten<String>),
    FnCall(Eaten<FnCall>),
    CmdOutput(Eaten<CmdCall>),
    CmdSuccess(Eaten<CmdCall>),
    FnAsValue(Eaten<String>),
    Closure {
        signature: FnSignature,
        body: Eaten<Block>,
    },
}

#[derive(Debug, Clone)]
pub enum ValueType {
    Single(MaybeEaten<SingleValueType>),
    Union(Vec<MaybeEaten<SingleValueType>>),
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
    TypedStruct(Vec<MaybeEaten<StructTypeMember>>),
    Function(FnSignature),
    TypeAlias(Eaten<String>),
}

#[derive(Debug, Clone)]
pub struct StructTypeMember {
    pub name: MaybeEaten<String>,
    pub typ: MaybeEaten<ValueType>,
}

#[derive(Debug, Clone)]
pub struct FnSignature {
    pub args: Vec<FnArg>,
    pub ret_type: Option<Eaten<Box<ValueType>>>,
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
    pub method: CmdCallMethod,
    pub path: Eaten<CmdPath>,
    pub args: Eaten<Vec<Eaten<CmdArg>>>,
}

#[derive(Debug, Clone)]
pub enum CmdCallMethod {
    Normal,
    Raw(Eaten<()>),
    Var(Eaten<()>),
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
    Raw(Eaten<String>),
    ComputedString(Eaten<ComputedString>),
}

#[derive(Debug, Clone)]
pub enum CmdArg {
    LiteralValue(Eaten<LiteralValue>),
    ComputedString(Eaten<ComputedString>),
    CmdCall(Eaten<CmdCall>),
    ParenExpr(Eaten<Expr>),
    VarName(Eaten<String>),
    FnAsValue(Eaten<String>),
    Raw(Eaten<String>),
    SpreadVar(Eaten<String>),
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
pub struct FnArg {
    pub names: FnArgNames,
    pub is_optional: bool,
    pub is_rest: bool,
    pub typ: Option<Eaten<ValueType>>,
}

#[derive(Debug, Clone)]
pub enum FnArgNames {
    NotFlag(Eaten<String>),
    ShortFlag(Eaten<char>),
    LongFlag(Eaten<String>),
    LongAndShortFlag {
        long: Eaten<String>,
        short: Eaten<char>,
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
    CmdArg(Eaten<CmdArg>),
}

// Utilities
impl FnArgNames {
    pub fn is_flag(&self) -> bool {
        match self {
            FnArgNames::NotFlag(_) => false,
            FnArgNames::ShortFlag(_) => true,
            FnArgNames::LongFlag(_) => true,
            FnArgNames::LongAndShortFlag { long: _, short: _ } => true,
        }
    }

    pub fn short_flag(&self) -> Option<Eaten<char>> {
        match self {
            FnArgNames::NotFlag(_) => None,
            FnArgNames::ShortFlag(flag) => Some(*flag),
            FnArgNames::LongFlag(_) => None,
            FnArgNames::LongAndShortFlag { long: _, short } => Some(*short),
        }
    }

    pub fn long_flag(&self) -> Option<&Eaten<String>> {
        match self {
            FnArgNames::NotFlag(_) => None,
            FnArgNames::ShortFlag(_) => None,
            FnArgNames::LongFlag(flag) => Some(flag),
            FnArgNames::LongAndShortFlag { long, short: _ } => Some(long),
        }
    }
}
