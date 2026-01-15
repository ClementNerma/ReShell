use parsy::{InputRange, Span};

/// A scope ID
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AstScopeId(pub(crate) u64);

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
        names: Span<ValueDestructuring>,
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
        body: Block,
        elsif: Vec<ElsIf>,
        els: Option<Block>,
    },

    /// 'for' loop
    ForLoop {
        destructure_as: Span<ValueDestructuring>,
        iter_on: Span<Expr>,
        body: Block,
    },

    /// Keyed 'for' loop
    ForLoopKeyed {
        key_iter_var: Span<String>,
        destructure_as: Span<ValueDestructuring>,
        iter_on: Span<Expr>,
        body: Block,
    },

    /// 'while' loop
    WhileLoop { cond: Span<Expr>, body: Block },

    /// 'continue' in a loop
    LoopContinue,

    /// 'break' in a loop
    LoopBreak,

    /// 'match' statement
    Match {
        expr: Span<Expr>,
        cases: Vec<MatchCase>,
        els: Option<Block>,
    },

    /// 'typematch' statement
    TypeMatch {
        expr: Span<Expr>,
        cases: Vec<TypeMatchCase>,
        els: Option<Block>,
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
        try_body: Span<Block>,
        catch_var: Span<String>,
        catch_body: Block,
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
    DoBlock(Block),

    /// Command call
    CmdCall(Span<CmdCall>),

    /// Program inclusion
    Include(Program),
}

#[derive(Debug, Clone)]
pub enum ValueDestructuring {
    Single(SingleVarDecl),
    Tuple(Vec<Span<ValueDestructuring>>),
    MapOrStruct(Vec<ObjPropSpreading>),
}

#[derive(Debug, Clone)]
pub struct ObjPropSpreading {
    pub typ: ObjPropSpreadingType,
    pub default_value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub enum ObjPropSpreadingType {
    RawKeyToConst {
        name: Span<String>,
        binding: Option<ObjPropSpreadingBinding>,
    },
    RawKeyToMut {
        name: Span<String>,
    },
    LiteralKeyToConst {
        literal_name: Span<String>,
        binding: ObjPropSpreadingBinding,
    },
}

#[derive(Debug, Clone)]
pub enum ObjPropSpreadingBinding {
    BindTo { alias: Span<String>, is_mut: bool },
    Deconstruct(Box<Span<ValueDestructuring>>),
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
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct MatchCase {
    pub matches: Span<Expr>,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct TypeMatchCase {
    pub matches: ValueType,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub signature: Span<FnSignature>,
    pub body: Span<Block>,
}

#[derive(Debug, Clone)]
pub struct Range {
    pub from: RangeBound,
    pub to: RangeBound,
    pub include_last_value: bool,
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
        right: Box<Span<ExprInnerContent>>,
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
    LoopContinue,
    LoopBreak,
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
    Range(Box<Range>),
    List(Vec<ListItem>),
    Map(Vec<MapItem>),
    Struct(Vec<StructItem>),
    Variable(Span<String>),
    FnCall(Box<Span<FnCall>>),
    CmdOutput(Box<CmdOutputCapture>),
    CmdCall(Span<CmdCall>),
    FnAsValue(Span<String>),
    Lambda(Box<Function>),
}

#[derive(Debug, Clone)]
pub enum ListItem {
    Single(Expr),
    Spread(Span<SpreadValue>),
}

#[derive(Debug, Clone)]
pub enum MapItem {
    Single { key: Span<MapKey>, value: Expr },
    Spread(Span<SpreadValue>),
}

#[derive(Debug, Clone)]
pub enum MapKey {
    Raw(String),
    LiteralString(String),
    ComputedString(ComputedString),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum StructItem {
    Single { field: Span<String>, value: Expr },
    Spread(Span<SpreadValue>),
}

#[derive(Debug, Clone)]
pub enum SpreadValue {
    Variable(Span<String>),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub struct CmdOutputCapture {
    pub capture: Span<CmdCaptureType>,
    pub cmd_call: Span<CmdCall>,
}

#[derive(Debug, Clone, Copy)]
pub enum CmdCaptureType {
    Stdout,
    Stderr,
}

#[derive(Debug, Clone, Hash)]
pub enum ValueType {
    Single(SingleValueType),
    Union(Vec<SingleValueType>),
}

#[derive(Debug, Clone, Hash)]
pub enum SingleValueType {
    // Primitives
    Any,
    Void,
    Null,
    Bool,
    Int,
    Float,
    Range,

    // String-based
    String,
    StringLiteral(String),

    // Time-based
    DateTime,
    Instant,
    Duration,

    // Misc.
    Regex,
    Error,

    // Containers
    UntypedList,
    TypedList(Box<ValueType>),
    UntypedMap,
    TypedMap(Box<ValueType>),
    UntypedStruct,
    TypedStruct(Vec<StructTypeMember>),

    // Functions
    Function(RuntimeSpan<FnSignature>),

    // Command-related
    CmdCall,
    CmdArg,

    // Type alias
    TypeAlias(Span<String>),
}

#[derive(Debug, Clone, Hash)]
pub struct StructTypeMember {
    pub name: RuntimeSpan<String>,
    pub optional: bool,
    pub typ: ValueType,
}

#[derive(Debug, Clone, Hash)]
pub struct FnSignature {
    pub args: RuntimeSpan<Vec<FnSignatureArg>>,
    pub ret_type: Option<RuntimeSpan<Box<ValueType>>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SingleOp {
    Neg,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DoubleOp {
    Arithmetic(ArithmeticDoubleOp),
    EqualityCmp(EqualityCmpDoubleOp),
    OrderingCmp(OrderingCmpDoubleOp),
    Logic(LogicDoubleOp),
    NullFallback, // ??
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArithmeticDoubleOp {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EqualityCmpDoubleOp {
    Eq,  // ==
    Neq, // !=
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OrderingCmpDoubleOp {
    Lt,  // <
    Lte, // <=
    Gt,  // >
    Gte, // <=
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicDoubleOp {
    And, // &&
    Or,  // ||
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
    CmdOutput(Box<CmdOutputCapture>),
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
    pub base: Box<CmdCallBase>,
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
    pub redirects: Option<Span<CmdRedirects>>,
}

#[derive(Debug, Clone)]
pub struct CmdEnvVar {
    pub name: Span<String>,
    pub value: Span<CmdValueMakingArg>,
}

#[derive(Debug, Clone)]
pub enum CmdRedirects {
    StdoutToFile(Span<CmdRawString>),
    StderrToFile(Span<CmdRawString>),
    StderrToStdout,
    StdoutToStderr,
    StdoutAndStderrToFile(Span<CmdRawString>),
    StdoutToFileAndStderrToFile {
        path_for_stdout: Span<CmdRawString>,
        path_for_stderr: Span<CmdRawString>,
    },
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
    CmdCapturedOutput(CmdOutputCapture),
}

#[derive(Debug, Clone)]
pub enum CmdArg {
    ValueMaking(Span<CmdValueMakingArg>),
    Flag(CmdFlagArg),
    Spread(Span<SpreadValue>),
}

#[derive(Debug, Clone)]
pub enum CmdValueMakingArg {
    LiteralValue(LiteralValue),
    ComputedString(ComputedString),
    List(Vec<ListItem>),
    InlineCmdCall(Span<CmdCall>),
    ParenExpr(Span<Expr>),
    CmdRawString(Span<CmdRawString>),
    Variable(Span<String>),
    Lambda(Span<Function>),
}

#[derive(Debug, Clone)]
pub struct CmdFlagArg {
    pub name: Span<CmdFlagArgName>,
    pub value: Option<CmdFlagValueArg>,
}

#[derive(Debug, Clone)]
pub enum CmdFlagArgName {
    Short(char),
    Long(String),
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
pub enum FnSignatureArg {
    Positional(FnSignaturePositionalArg),
    PresenceFlag(FnSignaturePresenceFlagArg),
    NormalFlag(FnSignatureNormalFlagArg),
    Rest(FnSignatureRestArg),
}

#[derive(Debug, Clone, Hash)]
pub struct FnSignaturePositionalArg {
    pub name: RuntimeSpan<String>,
    pub is_optional: bool,
    pub typ: Option<ValueType>,
}

#[derive(Debug, Clone, Hash)]
pub struct FnSignaturePresenceFlagArg {
    pub names: FnSignatureFlagArgNames,
}

#[derive(Debug, Clone, Hash)]
pub struct FnSignatureNormalFlagArg {
    pub names: FnSignatureFlagArgNames,
    pub is_optional: bool,
    pub typ: ValueType,
}

#[derive(Debug, Clone, Hash)]
pub struct FnSignatureRestArg {
    pub name: RuntimeSpan<String>,
    pub typ: Option<RuntimeSpan<ValueType>>,
}

#[derive(Debug, Clone, Hash)]
pub enum FnSignatureFlagArgNames {
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
    pub call_args: Span<Vec<Span<FnArg>>>,
}

#[derive(Debug, Clone, Copy)]
pub enum FnCallNature {
    NamedFunction,
    Method,
    Variable,
}

#[derive(Debug, Clone)]
pub enum FnArg {
    Expr(Span<Expr>),
    Flag {
        name: Span<FnFlagArgName>,
        value: Option<Span<Expr>>,
    },
}

#[derive(Debug, Clone)]
pub enum FnFlagArgName {
    Short(char),
    Long(String),
}

/// A token that's either eaten from a real input or generated at runtime
#[derive(Debug, Clone, Copy, Hash)]
pub struct RuntimeSpan<T> {
    pub at: RuntimeCodeRange,
    pub data: T,
}

/// Either a [`InputRange`] or an internal location
#[derive(Debug, Clone, Copy, Hash)]
pub enum RuntimeCodeRange {
    Parsed(InputRange),
    Internal(&'static str),
}
