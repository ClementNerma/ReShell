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
        names: Eaten<VarDeconstruction>,
        init_expr: Eaten<Expr>,
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

    /// Ranged 'for' loop
    ForLoopRanged {
        iter_var: Eaten<String>,
        iter_from: Eaten<RangeBound>,
        iter_to: Eaten<RangeBound>,
        inclusive: bool,
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

    /// 'match' statement
    Match {
        expr: Eaten<Expr>,
        cases: Vec<MatchCase>,
        els: Option<Eaten<Block>>,
    },

    /// 'typematch' statement
    TypeMatch {
        expr: Eaten<Expr>,
        cases: Vec<TypeMatchCase>,
        els: Option<Eaten<Block>>,
    },

    /// Function declaration
    FnDecl {
        name: Eaten<String>,
        content: Function,
    },

    /// Method declaration
    MethodDecl {
        name: Eaten<String>,
        on_type: Eaten<ValueType>,
        content: Function,
    },

    /// Function return statement
    FnReturn { expr: Option<Eaten<Expr>> },

    /// Throw statement
    Throw(Eaten<Expr>),

    /// Try block
    Try {
        try_expr: Eaten<Expr>,
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

    /// Command call
    CmdCall(Eaten<CmdCall>),

    /// Program inclusion
    Include(Eaten<Program>),
}

#[derive(Debug, Clone)]
pub enum VarDeconstruction {
    Single(SingleVarDecl),
    Tuple(Vec<Eaten<VarDeconstruction>>),
    MapOrStruct(Vec<(Eaten<SingleVarDecl>, Option<MapDestructBinding>)>),
}

#[derive(Debug, Clone)]
pub enum MapDestructBinding {
    BindTo(Eaten<String>),
    Destruct(Box<Eaten<VarDeconstruction>>),
}

#[derive(Debug, Clone)]
pub struct SingleVarDecl {
    pub name: Eaten<String>,

    /// Location of this item points to the "mut" keyword
    pub is_mut: Option<Eaten<()>>,

    /// Enforced type
    pub enforced_type: Option<ValueType>,
}

#[derive(Debug, Clone)]
pub struct ElsIf {
    pub cond: Eaten<Expr>,
    pub body: Eaten<Block>,
}

#[derive(Debug, Clone)]
pub struct MatchCase {
    pub matches: Eaten<Expr>,
    pub body: Eaten<Block>,
}

#[derive(Debug, Clone)]
pub struct TypeMatchCase {
    pub matches: Eaten<ValueType>,
    pub body: Eaten<Block>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub signature: Eaten<FnSignature>,
    pub body: Eaten<Block>,
}

#[derive(Debug, Clone)]
pub enum RangeBound {
    Literal(i64),
    Variable(Eaten<String>),
    Expr(Eaten<Expr>),
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
        right_chainings: Vec<Eaten<ExprInnerChaining>>,
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
    Match {
        expr: Eaten<Box<Expr>>,
        cases: Vec<MatchExprCase>,
        els: Eaten<Box<Expr>>,
    },
    TypeMatch {
        expr: Eaten<Box<Expr>>,
        cases: Vec<TypeMatchExprCase>,
        els: Eaten<Box<Expr>>,
    },
    Try {
        try_expr: Eaten<Box<Expr>>,
        catch_var: Eaten<String>,
        catch_expr: Eaten<Box<Expr>>,
        catch_expr_scope_id: AstScopeId,
    },
    Throw(Eaten<Box<Expr>>),
}

#[derive(Debug, Clone)]
pub struct ElsIfExpr {
    pub cond: Eaten<Box<Expr>>,
    pub body: Eaten<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub struct MatchExprCase {
    pub matches: Eaten<Expr>,
    pub then: Eaten<Expr>,
}

#[derive(Debug, Clone)]
pub struct TypeMatchExprCase {
    pub matches: Eaten<ValueType>,
    pub then: Eaten<Expr>,
}

#[derive(Debug, Clone)]
pub enum ExprInnerChaining {
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
    Lambda(Lambda),
}

#[derive(Debug, Clone, Hash)]
pub enum ValueType {
    Single(RuntimeEaten<SingleValueType>),
    Union(Vec<RuntimeEaten<SingleValueType>>),
}

#[derive(Debug, Clone, Hash)]
pub enum SingleValueType {
    Any,
    Void,
    Null,
    Bool,
    Int,
    Float,
    String,
    Error,
    UntypedList,
    TypedList(Box<ValueType>),
    UntypedMap,
    TypedMap(Box<ValueType>),
    UntypedStruct,
    TypedStruct(Vec<RuntimeEaten<StructTypeMember>>),
    Function(RuntimeEaten<FnSignature>),
    TypeAlias(Eaten<String>),
    CmdCall,
    CmdArg,
    // TODO: parsable?
    Custom(&'static str),
}

#[derive(Debug, Clone, Hash)]
pub struct StructTypeMember {
    pub name: RuntimeEaten<String>,
    pub typ: RuntimeEaten<ValueType>,
}

#[derive(Debug, Clone, Hash)]
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
    Mod,          // %
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

#[derive(Debug, Clone)]
pub enum Lambda {
    ExplicitParams(Function),
    ImplicitSingleParam(Eaten<Block>),
}

#[derive(Debug, Clone, Copy)]
pub enum EscapableChar {
    Newline,
    CarriageReturn,
    Tab,
    DoubleQuote,
    BackQuote,
    SingleQuote,
    Backslash,
    DollarSign,
}

#[derive(Debug, Clone)]
pub struct CmdCall {
    pub base: CmdCallBase,
    pub pipes: Vec<CmdPipe>,
}

#[derive(Debug, Clone)]
pub enum CmdCallBase {
    Expr(Eaten<Box<Expr>>),
    SingleCmdCall(Eaten<SingleCmdCall>),
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
    Direct(Eaten<String>),
    Method(Eaten<String>),
    LiteralString(Eaten<String>),
    ComputedString(Eaten<ComputedString>),
    Raw(Eaten<String>),
}

#[derive(Debug, Clone)]

pub struct CmdRawString {
    pub pieces: Vec<Eaten<CmdRawStringPiece>>,
}

#[derive(Debug, Clone)]
pub enum CmdRawStringPiece {
    Literal(String),
    Variable(Eaten<String>),
}

#[derive(Debug, Clone)]
pub enum CmdArg {
    ValueMaking(CmdValueMakingArg),
    Flag(CmdFlagArg),
    Spread(Eaten<CmdSpreadArg>),
}

#[derive(Debug, Clone)]
pub enum CmdSpreadArg {
    Variable(Eaten<String>),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum CmdValueMakingArg {
    LiteralValue(Eaten<LiteralValue>),
    ComputedString(Eaten<ComputedString>),
    InlineCmdCall(Eaten<CmdCall>),
    CmdOutput(Eaten<CmdCall>),
    ParenExpr(Eaten<Expr>),
    CmdRawString(Eaten<CmdRawString>),
    Variable(Eaten<String>),
    Lambda(Eaten<Lambda>),
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
    /// Optimization: flag names which are already in camel case
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
    Stderr,
    ValueOrStdout,
}

#[derive(Debug, Clone, Hash)]
pub enum FnArg {
    Positional(FnPositionalArg),
    PresenceFlag(FnPresenceFlagArg),
    NormalFlag(FnNormalFlagArg),
    Rest(FnRestArg),
}

#[derive(Debug, Clone, Hash)]
pub struct FnPositionalArg {
    pub name: RuntimeEaten<String>,
    pub is_optional: bool,
    pub typ: Option<RuntimeEaten<ValueType>>,
}

#[derive(Debug, Clone, Hash)]
pub struct FnPresenceFlagArg {
    pub names: FnFlagArgNames,
}

#[derive(Debug, Clone, Hash)]
pub struct FnNormalFlagArg {
    pub names: FnFlagArgNames,
    pub is_optional: bool,
    pub typ: RuntimeEaten<ValueType>,
}

#[derive(Debug, Clone, Hash)]
pub struct FnRestArg {
    pub name: RuntimeEaten<String>,
    pub typ: Option<RuntimeEaten<ValueType>>,
}

#[derive(Debug, Clone, Hash)]
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
    pub nature: FnCallNature,
    pub name: Eaten<String>,
    pub call_args: Eaten<Vec<Eaten<FnCallArg>>>,
}

#[derive(Debug, Clone, Copy)]
pub enum FnCallNature {
    NamedFunction,
    Method,
    Variable,
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
pub struct RuntimeEaten<T> {
    pub at: RuntimeCodeRange,
    pub data: T,
}

/// Either a [`CodeRange`] or an internal location
#[derive(Debug, Clone, Copy, Hash)]
pub enum RuntimeCodeRange {
    Parsed(CodeRange),
    Internal(&'static str),
}
