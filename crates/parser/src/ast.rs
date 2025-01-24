use parsy::{CodeRange, Span};

use crate::scope::AstScopeId;

/// A complete parsed program
#[derive(Debug, Clone)]
pub struct Program {
    pub content: Span<Block>,
}

/// A block (= set of instructions)
#[derive(Debug, Clone)]
pub struct Block {
    pub scope_id: AstScopeId,
    pub instructions: Vec<Span<Instruction>>,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    /// Variable declaration
    DeclareVar {
        names: Span<VarDeconstruction>,
        init_expr: Span<Expr>,
    },

    /// Variable assignment
    AssignVar {
        name: Span<String>,
        prop_acc: Vec<Span<PropAccessNature>>,
        list_push: Option<Span<()>>,
        expr: Span<Expr>,
    },

    /// 'if' conditional
    IfCond {
        cond: Span<Expr>,
        body: Span<Block>,
        elsif: Vec<Span<ElsIf>>,
        els: Option<Span<Block>>,
    },

    /// 'for' loop
    ForLoop {
        iter_var: Span<String>,
        iter_on: Span<Expr>,
        body: Span<Block>,
    },

    /// Ranged 'for' loop
    ForLoopRanged {
        iter_var: Span<String>,
        iter_from: Span<RangeBound>,
        iter_to: Span<RangeBound>,
        inclusive: bool,
        body: Span<Block>,
    },

    /// Keyed 'for' loop
    ForLoopKeyed {
        key_iter_var: Span<String>,
        value_iter_var: Span<String>,
        iter_on: Span<Expr>,
        body: Span<Block>,
    },

    /// 'while' loop
    WhileLoop { cond: Span<Expr>, body: Span<Block> },

    /// 'continue' in a loop
    LoopContinue,

    /// 'break' in a loop
    LoopBreak,

    /// 'match' statement
    Match {
        expr: Span<Expr>,
        cases: Vec<MatchCase>,
        els: Option<Span<Block>>,
    },

    /// 'typematch' statement
    TypeMatch {
        expr: Span<Expr>,
        cases: Vec<TypeMatchCase>,
        els: Option<Span<Block>>,
    },

    /// Function declaration
    FnDecl {
        name: Span<String>,
        content: Function,
    },

    /// Method declaration
    MethodDecl {
        name: Span<String>,
        on_type: ValueType,
        content: Function,
    },

    /// Function return statement
    FnReturn { expr: Option<Span<Expr>> },

    /// Throw statement
    Throw(Span<Expr>),

    /// Try block
    Try {
        try_expr: Span<Expr>,
        catch_var: Span<String>,
        catch_body: Span<Block>,
    },

    /// Command alias declaration
    CmdAliasDecl {
        name: Span<String>,
        content_scope_id: AstScopeId,
        content: Span<SingleCmdCall>,
    },

    /// Type alias declaration
    TypeAliasDecl {
        name: Span<String>,
        content: Span<ValueType>,
    },

    /// 'do' block
    DoBlock(Span<Block>),

    /// Command call
    CmdCall(Span<CmdCall>),

    /// Program inclusion
    Include(Program),
}

#[derive(Debug, Clone)]
pub enum VarDeconstruction {
    Single(SingleVarDecl),
    Tuple(Vec<Span<VarDeconstruction>>),
    MapOrStruct(Vec<ObjDestructuringItem>),
}

#[derive(Debug, Clone)]
pub struct ObjDestructuringItem {
    /// Key in the map / Field in the struct
    pub name: Span<String>,

    /// Is the destruct item mutable?
    pub is_mut: bool,

    /// How to deconstruct this item
    pub binding: Option<ObjDestructuringItemBinding>,

    /// Optional default value if this key/field does not exist
    pub default_value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub enum ObjDestructuringItemBinding {
    BindTo(Span<String>),
    Destruct(Box<Span<VarDeconstruction>>),
}

#[derive(Debug, Clone)]
pub struct SingleVarDecl {
    pub name: Span<String>,
    pub is_mut: bool,

    /// Type specified by the user
    pub enforced_type: Option<ValueType>,
}

#[derive(Debug, Clone)]
pub struct ElsIf {
    pub cond: Span<Expr>,
    pub body: Span<Block>,
}

#[derive(Debug, Clone)]
pub struct MatchCase {
    pub matches: Span<Expr>,
    pub body: Span<Block>,
}

#[derive(Debug, Clone)]
pub struct TypeMatchCase {
    pub matches: ValueType,
    pub body: Span<Block>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub signature: Span<FnSignature>,
    pub body: Span<Block>,
}

#[derive(Debug, Clone)]
pub enum RangeBound {
    Literal(i64),
    Variable(Span<String>),
    Expr(Span<Expr>),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub inner: Span<ExprInner>,
    pub right_ops: Vec<ExprOp>,
}

#[derive(Debug, Clone)]
pub struct ExprInner {
    pub content: Span<ExprInnerContent>,
    pub chainings: Vec<ExprInnerChaining>,
}

#[derive(Debug, Clone)]
pub enum ExprInnerContent {
    SingleOp {
        op: SingleOp,
        right: Span<Box<ExprInnerContent>>,
        right_chainings: Vec<Span<ExprInnerChaining>>,
    },
    ParenExpr(Box<Expr>),
    Value(Value),
    FnAsValue(Span<String>),
    Ternary {
        cond: Span<Box<Expr>>,
        body: Box<Expr>,
        elsif: Vec<ElsIfExpr>,
        els: Box<Expr>,
    },
    Match {
        expr: Box<Expr>,
        cases: Vec<MatchExprCase>,
        els: Box<Expr>,
    },
    TypeMatch {
        expr: Box<Expr>,
        cases: Vec<TypeMatchExprCase>,
        els: Box<Expr>,
    },
    Try {
        try_expr: Box<Expr>,
        catch_var: Span<String>,
        catch_expr: Box<Expr>,
        catch_expr_scope_id: AstScopeId,
    },
    Throw(Span<Box<Expr>>),
}

#[derive(Debug, Clone)]
pub struct ElsIfExpr {
    pub cond: Span<Box<Expr>>,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct MatchExprCase {
    pub matches: Span<Expr>,
    pub then: Expr,
}

#[derive(Debug, Clone)]
pub struct TypeMatchExprCase {
    pub matches: ValueType,
    pub then: Expr,
}

#[derive(Debug, Clone)]
pub enum ExprInnerChaining {
    PropAccess(PropAccess),
    MethodCall(Span<FnCall>),
}

#[derive(Debug, Clone)]
pub struct PropAccess {
    pub nature: Span<PropAccessNature>,
    pub nullable: bool,
}

#[derive(Debug, Clone)]
pub enum PropAccessNature {
    Key(Box<Span<Expr>>),
    Prop(Span<String>),
}

#[derive(Debug, Clone)]
pub enum ExprOp {
    DoubleOp {
        op: Span<DoubleOp>,
        right_op: Box<Span<ExprInner>>,
    },
    TypeIs {
        right_op: Span<ValueType>,
    },
}

#[derive(Debug, Clone)]
pub enum Value {
    Null,
    Literal(LiteralValue),
    ComputedString(ComputedString),
    List(Vec<Span<Expr>>),
    Map(Vec<(Span<MapKey>, Expr)>),
    Struct(Vec<(Span<String>, Expr)>),
    Variable(Span<String>),
    FnCall(Span<FnCall>),
    CmdOutput(Span<CmdCall>),
    CmdCall(Span<CmdCall>),
    FnAsValue(Span<String>),
    Lambda(Function),
}

#[derive(Debug, Clone)]
pub enum MapKey {
    Raw(String),
    LiteralString(String),
    ComputedString(ComputedString),
    Expr(Expr),
}

#[derive(Debug, Clone, Hash)]
pub enum ValueType {
    Single(SingleValueType),
    Union(Vec<SingleValueType>),
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
    TypedStruct(Vec<StructTypeMember>),
    Function(RuntimeSpan<FnSignature>),
    TypeAlias(Span<String>),
    CmdCall,
    CmdArg,
    // TODO: parsable?
    Custom(&'static str),
}

#[derive(Debug, Clone, Hash)]
pub struct StructTypeMember {
    pub name: RuntimeSpan<String>,
    pub optional: bool,
    pub typ: ValueType,
}

#[derive(Debug, Clone, Hash)]
pub struct FnSignature {
    pub args: RuntimeSpan<Vec<FnArg>>,
    pub ret_type: Option<RuntimeSpan<Box<ValueType>>>,
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
    pub pieces: Vec<ComputedStringPiece>,
}

#[derive(Debug, Clone)]
pub enum ComputedStringPiece {
    Literal(String),
    Escaped(EscapableChar),
    Variable(Span<String>),
    Expr(Span<Expr>),
    CmdCall(Span<CmdCall>),
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
    Caret,
}

#[derive(Debug, Clone)]
pub struct CmdCall {
    pub base: CmdCallBase,
    pub pipes: Vec<CmdPipe>,
}

#[derive(Debug, Clone)]
// The 'Expr' variant is rare enough to not cause a problem with enum size variants difference
#[allow(clippy::large_enum_variant)]
pub enum CmdCallBase {
    Expr(Span<Box<Expr>>),
    SingleCmdCall(Span<SingleCmdCall>),
}

#[derive(Debug, Clone)]
pub struct SingleCmdCall {
    pub env_vars: Span<Vec<Span<CmdEnvVar>>>,
    pub path: Span<CmdPath>,
    pub args: Span<Vec<Span<CmdArg>>>,
}

#[derive(Debug, Clone)]
pub struct CmdEnvVar {
    pub name: Span<String>,
    pub value: Span<CmdValueMakingArg>,
}

#[derive(Debug, Clone)]
pub enum CmdPath {
    Raw(Span<String>),
    External(CmdExternalPath),
    Method(Span<String>),
}

#[derive(Debug, Clone)]
pub enum CmdExternalPath {
    RawString(Span<CmdRawString>),
    LiteralString(Span<String>),
    ComputedString(Span<ComputedString>),
}

#[derive(Debug, Clone)]

pub struct CmdRawString {
    pub pieces: Vec<Span<CmdRawStringPiece>>,
}

#[derive(Debug, Clone)]
pub enum CmdRawStringPiece {
    Literal(String),
    Variable(Span<String>),
}

#[derive(Debug, Clone)]
pub enum CmdArg {
    ValueMaking(Span<CmdValueMakingArg>),
    Flag(CmdFlagArg),
    Spread(Span<CmdSpreadArg>),
}

#[derive(Debug, Clone)]
pub enum CmdSpreadArg {
    Variable(Span<String>),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum CmdValueMakingArg {
    LiteralValue(LiteralValue),
    ComputedString(ComputedString),
    InlineCmdCall(Span<CmdCall>),
    CmdOutput(Span<CmdCall>),
    ParenExpr(Span<Expr>),
    CmdRawString(Span<CmdRawString>),
    Variable(Span<String>),
    Lambda(Span<Function>),
}

#[derive(Debug, Clone)]
pub struct CmdFlagArg {
    pub name: Span<CmdFlagNameArg>,
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
    pub value: Span<CmdValueMakingArg>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FlagValueSeparator {
    Space,
    Equal,
}

#[derive(Debug, Clone)]
pub struct CmdPipe {
    pub pipe_type: Span<CmdPipeType>,
    pub cmd: Span<SingleCmdCall>,
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
    pub name: RuntimeSpan<String>,
    pub is_optional: bool,
    pub typ: Option<ValueType>,
}

#[derive(Debug, Clone, Hash)]
pub struct FnPresenceFlagArg {
    pub names: FnFlagArgNames,
}

#[derive(Debug, Clone, Hash)]
pub struct FnNormalFlagArg {
    pub names: FnFlagArgNames,
    pub is_optional: bool,
    pub typ: ValueType,
}

#[derive(Debug, Clone, Hash)]
pub struct FnRestArg {
    pub name: RuntimeSpan<String>,
    pub typ: Option<RuntimeSpan<ValueType>>,
}

#[derive(Debug, Clone, Hash)]
pub enum FnFlagArgNames {
    ShortFlag(RuntimeSpan<char>),
    LongFlag(RuntimeSpan<String>),
    LongAndShortFlag {
        long: RuntimeSpan<String>,
        short: RuntimeSpan<char>,
    },
}

#[derive(Debug, Clone)]
pub struct FnCall {
    pub nature: FnCallNature,
    pub name: Span<String>,
    pub call_args: Span<Vec<Span<FnCallArg>>>,
}

#[derive(Debug, Clone, Copy)]
pub enum FnCallNature {
    NamedFunction,
    Method,
    Variable,
}

#[derive(Debug, Clone)]
pub enum FnCallArg {
    Expr(Span<Expr>),
    Flag {
        name: Span<CmdFlagNameArg>,
        value: Span<Expr>,
    },
    CmdArg(Span<CmdArg>),
}

/// A token that's either eaten from a real input or generated at runtime
#[derive(Debug, Clone, Copy, Hash)]
pub struct RuntimeSpan<T> {
    pub at: RuntimeCodeRange,
    pub data: T,
}

/// Either a [`CodeRange`] or an internal location
#[derive(Debug, Clone, Copy, Hash)]
pub enum RuntimeCodeRange {
    Parsed(CodeRange),
    Internal(&'static str),
}
