use std::collections::HashMap;

use parsy::{CodeRange, Eaten};

use crate::scope::AstScopeId;

/// A complete parsed program
#[derive(Debug, Clone)]
pub struct Program {
    pub content: Eaten<Block>,
}

/// A block (= set of instructions)
#[derive(Debug, Clone)]
pub struct Block {
    pub scope_id: AstScopeId,
    pub instructions: Vec<Eaten<Instruction>>,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    /// Variable declaration
    DeclareVar {
        name: Eaten<String>,
        init_expr: Eaten<Expr>,

        /// Location of this item points to the "mut" keyword
        mutable: Option<Eaten<()>>,
    },

    /// Variable assignment
    AssignVar {
        name: Eaten<String>,
        prop_acc: Vec<Eaten<PropAccessNature>>,
        list_push: Option<Eaten<()>>,
        expr: Eaten<Expr>,
    },

    /// 'if' conditional
    IfCond {
        cond: Eaten<Expr>,
        body: Eaten<Block>,
        elsif: Vec<Eaten<ElsIf>>,
        els: Option<Eaten<Block>>,
    },

    /// 'for' loop
    ForLoop {
        iter_var: Eaten<String>,
        iter_on: Eaten<Expr>,
        body: Eaten<Block>,
    },

    /// Keyed 'for' loop
    ForLoopKeyed {
        key_iter_var: Eaten<String>,
        value_iter_var: Eaten<String>,
        iter_on: Eaten<Expr>,
        body: Eaten<Block>,
    },

    /// 'while' loop
    WhileLoop {
        cond: Eaten<Expr>,
        body: Eaten<Block>,
    },

    /// 'continue' in a loop
    LoopContinue,

    /// 'break' in a loop
    LoopBreak,

    /// Switch statement
    Switch {
        expr: Eaten<Expr>,
        cases: Vec<SwitchCase>,
    },

    /// Function declaration
    FnDecl {
        name: Eaten<String>,
        content: Function,
    },

    /// Function return statement
    FnReturn { expr: Option<Eaten<Expr>> },

    /// Function throw statement
    Throw(Eaten<Expr>),

    /// Try block
    Try {
        call: Eaten<FnCall>,
        catch_var: Eaten<String>,
        catch_body: Eaten<Block>,
    },

    /// Command alias declaratin
    CmdAliasDecl {
        name: Eaten<String>,
        content_scope_id: AstScopeId,
        content: Eaten<SingleCmdCall>,
    },

    /// Type alias declaration
    TypeAliasDecl {
        name: Eaten<String>,
        content: Eaten<ValueType>,
    },

    /// 'do' block
    DoBlock(Eaten<Block>),

    /// Function call
    FnCall(Eaten<FnCall>),

    /// Command call
    CmdCall(Eaten<CmdCall>),

    /// Program inclusion
    Include(Eaten<Program>),
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
    pub body: Eaten<FunctionBody>,
}

#[derive(Debug, Clone)]
pub enum FunctionBody {
    Expr {
        content: Eaten<Box<Expr>>,
        scope_id: AstScopeId,
    },
    Block(Eaten<Block>),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub inner: Eaten<ExprInner>,
    pub right_ops: Vec<ExprOp>,
}

#[derive(Debug, Clone)]
pub struct ExprInner {
    pub content: Eaten<ExprInnerContent>,
    pub chainings: Vec<Eaten<ExprInnerChaining>>,
}

#[derive(Debug, Clone)]
pub enum ExprInnerContent {
    SingleOp {
        op: Eaten<SingleOp>,
        right: Eaten<Box<ExprInnerContent>>,
        right_chainings: Vec<Eaten<ExprInnerDirectChaining>>,
    },
    ParenExpr(Eaten<Box<Expr>>),
    Value(Eaten<Value>),
    FnAsValue(Eaten<String>),
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
        catch_expr_scope_id: AstScopeId,
    },
}

#[derive(Debug, Clone)]
pub struct ElsIfExpr {
    pub cond: Eaten<Box<Expr>>,
    pub body: Eaten<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub enum ExprInnerChaining {
    Direct(ExprInnerDirectChaining),
    FnCall(Eaten<FnCall>),
}

#[derive(Debug, Clone)]
pub enum ExprInnerDirectChaining {
    PropAccess(Eaten<PropAccess>),
    MethodCall(Eaten<FnCall>),
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
    CmdCall(Eaten<CmdCall>),
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
    CmdCall,
    ArgSpread,
    // TODO: parsable?
    Custom(&'static str),
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
    String(String),
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
    CarriageReturn,
    DoubleQuote,
    BackQuote,
    SingleQuote,
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
    pub value: Eaten<CmdValueMakingArg>,
}

#[derive(Debug, Clone)]
pub enum CmdPath {
    Direct(Eaten<CmdComputedString>),
    Expr(Eaten<Box<Expr>>),
    ComputedString(Eaten<ComputedString>),
    CmdComputedString(Eaten<CmdComputedString>),
}

#[derive(Debug, Clone)]

pub struct CmdComputedString {
    pub pieces: Vec<Eaten<CmdComputedStringPiece>>,
}

#[derive(Debug, Clone)]
pub enum CmdComputedStringPiece {
    Literal(String),
    Escaped(char),
    Variable(Eaten<String>),
}

#[derive(Debug, Clone)]
pub enum CmdArg {
    ValueMaking(CmdValueMakingArg),
    Flag(CmdFlagArg),
    SpreadVar(Eaten<String>),
}

#[derive(Debug, Clone)]
pub enum CmdValueMakingArg {
    LiteralValue(Eaten<LiteralValue>),
    ComputedString(Eaten<ComputedString>),
    CmdOutput(Eaten<CmdCall>),
    ParenExpr(Eaten<Expr>),
    CmdComputedString(Eaten<CmdComputedString>),
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
    LongNoConvert(String),
}

#[derive(Debug, Clone)]
pub struct CmdFlagValueArg {
    pub value_sep: FlagValueSeparator,
    pub value: Eaten<CmdValueMakingArg>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    Value,
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
    Flag {
        name: Eaten<CmdFlagNameArg>,
        value: Eaten<Expr>,
    },
    CmdArg(Eaten<CmdArg>),
}

/// A token that's either eaten from a real input or generated at runtime
#[derive(Debug, Clone, Copy, Hash)]
pub enum RuntimeEaten<T> {
    Parsed(Eaten<T>),
    Internal(T),
}

/// Either a [`CodeRange`] or an internal location
#[derive(Debug, Clone, Copy)]
pub enum RuntimeCodeRange {
    Parsed(CodeRange),
    Internal,
}
