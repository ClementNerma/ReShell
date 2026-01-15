use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::{DefaultHasher, Hash, Hasher},
    sync::{Arc, OnceLock},
    time::{Duration, Instant},
};

use indexmap::{IndexMap, IndexSet};
use jiff::Zoned;
use parsy::{InputRange, Span};
use regex::Regex;
use reshell_parser::ast::{
    AstScopeId, Block, CmdFlagArgName, FnSignature, RuntimeCodeRange, RuntimeSpan, SingleCmdCall,
    SingleValueType, StructTypeMember, ValueType,
};
use reshell_prettify::{PrettyPrintOptions, PrettyPrintable, PrettyPrintablePiece};

use crate::{
    cmd::FlagArgValueResult,
    context::{Context, ScopeContent},
    errors::{ExecInfoType, ExecResult},
    functions::ValidatedFnCallArg,
    gc::GcCell,
};

/// Runtime function value
#[derive(Debug)]
pub struct RuntimeFnValue {
    pub signature: RuntimeFnSignature,
    pub body: RuntimeFnBody,
    pub parent_scopes: IndexSet<u64>,
    pub is_method: bool,

    /// Function's captured dependencies
    /// Uninit before the function's actual declaration point
    ///
    /// TODO: Use an `Option` to avoid allocating if no dependency to capture
    pub captured_deps: OnceLock<ScopeContent>,
}

/// Runtime function signature
#[derive(Debug)]
pub enum RuntimeFnSignature {
    Shared(Arc<Span<FnSignature>>),
    Owned(FnSignature),
}

/// Runtime function body
pub enum RuntimeFnBody {
    Block(Arc<Span<Block>>),
    Internal(InternalFnBody),
}

impl RuntimeFnSignature {
    pub fn inner(&self) -> &FnSignature {
        match &self {
            RuntimeFnSignature::Shared(shared) => &shared.data,
            RuntimeFnSignature::Owned(owned) => owned,
        }
    }
}

impl Debug for RuntimeFnBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Block(block) => f.debug_tuple("Block").field(block).finish(),
            Self::Internal(_) => f.debug_tuple("Internal").finish(),
        }
    }
}

/// Internal function call data
///
/// Used when an internal component calls a function.
pub struct InternalFnCallData<'c> {
    /// Call's location (can be internal)
    pub call_at: RuntimeCodeRange,

    /// Location of arguments (can be internal)
    pub args_at: RuntimeCodeRange,

    /// Named arguments
    pub args: HashMap<String, ValidatedFnCallArg>,

    /// Runtime context
    pub ctx: &'c mut Context,
}

/// Body of an internal function (used for e.g. builtin functions)
pub type InternalFnBody = fn(InternalFnCallData) -> ExecResult<Option<LocatedValue>>;

/// Runtime command alias
#[derive(Debug, Clone)]
pub struct RuntimeCmdAlias {
    /// Location of the command alias' name inside its declaration
    pub name_declared_at: InputRange,

    /// Name of the alias
    pub name: String,

    /// Content of the alias
    pub content: Arc<Span<SingleCmdCall>>,

    /// Content scope ID
    pub content_scope_id: AstScopeId,

    /// Parent scopes of the alias' declaration
    pub parent_scopes: IndexSet<u64>,

    /// Captured depenencies for evaluation
    pub captured_deps: ScopeContent,
}

/// Content of a command argument
#[derive(Debug, Clone)]
pub enum CmdArgValue {
    Basic(LocatedValue),
    Flag(CmdFlagValue),
}

/// Content of a command flag
#[derive(Debug, Clone)]
pub struct CmdFlagValue {
    pub name: RuntimeSpan<CmdFlagArgName>,
    pub value: Option<FlagArgValueResult>,
}

/// Content of a range value
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RangeValue {
    pub from: i64,
    pub to: i64,
    pub include_last_value: bool,
}

#[derive(Debug, Clone)]
pub enum RuntimeValue {
    // Primitives
    // These can be cloned pretty cheaply
    Void,
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    DateTime(Arc<Zoned>),
    Instant(Instant),
    Duration(Duration),
    Regex(Arc<Regex>),
    Range(RangeValue),
    Error(Arc<ErrorValue>),
    CmdCall(Arc<CmdCallValue>),
    CmdArg(Arc<CmdArgValue>),

    // Containers
    // These can be cloned cheaply thanks to them using a GcCell
    List(GcCell<Vec<RuntimeValue>>),
    Map(GcCell<IndexMap<String, RuntimeValue>>),
    Struct(GcCell<IndexMap<String, RuntimeValue>>),
    Function(Arc<RuntimeFnValue>),
}

impl RuntimeValue {
    /// Compute the type of a runtime value
    pub fn compute_type(&self) -> SingleValueType {
        match self {
            RuntimeValue::Void => SingleValueType::Void,
            RuntimeValue::Null => SingleValueType::Null,
            RuntimeValue::Bool(_) => SingleValueType::Bool,
            RuntimeValue::Int(_) => SingleValueType::Int,
            RuntimeValue::Float(_) => SingleValueType::Float,
            RuntimeValue::String(_) => SingleValueType::String,
            RuntimeValue::DateTime(_) => SingleValueType::DateTime,
            RuntimeValue::Instant(_) => SingleValueType::Instant,
            RuntimeValue::Duration(_) => SingleValueType::Duration,
            RuntimeValue::Regex(_) => SingleValueType::Regex,
            RuntimeValue::Range(_) => SingleValueType::Range,
            RuntimeValue::CmdCall(_) => SingleValueType::CmdCall,
            RuntimeValue::CmdArg(_) => SingleValueType::CmdArg,
            RuntimeValue::Error(_) => SingleValueType::Error,

            RuntimeValue::List(items) => {
                match generate_values_types(items.read_promise_no_write().iter()) {
                    Some(typ) => SingleValueType::TypedList(Box::new(typ)),
                    None => SingleValueType::UntypedList,
                }
            }

            RuntimeValue::Map(entries) => {
                match generate_values_types(
                    entries
                        .read_promise_no_write()
                        .iter()
                        .map(|(_, value)| value),
                ) {
                    Some(typ) => SingleValueType::TypedMap(Box::new(typ)),
                    None => SingleValueType::UntypedMap,
                }
            }

            RuntimeValue::Struct(members) => SingleValueType::TypedStruct(
                members
                    .read_promise_no_write()
                    .iter()
                    .map(|(name, value)| StructTypeMember {
                        name: RuntimeSpan::internal("type deducer", name.clone()),
                        typ: ValueType::Single(value.compute_type()),
                        optional: false,
                    })
                    .collect(),
            ),

            RuntimeValue::Function(content) => {
                // TODO: performance (use already collected data from checker?)
                SingleValueType::Function(match &content.signature {
                    RuntimeFnSignature::Shared(shared) => RuntimeSpan::from(Span::clone(shared)),
                    RuntimeFnSignature::Owned(owned) => {
                        RuntimeSpan::internal("type deducer", owned.clone())
                    }
                })
            }
        }
    }

    /// Check if a value is a container (e.g. list, map, struct)
    pub fn is_container(&self) -> bool {
        match self {
            RuntimeValue::Void
            | RuntimeValue::Null
            | RuntimeValue::Bool(_)
            | RuntimeValue::Int(_)
            | RuntimeValue::Float(_)
            | RuntimeValue::String(_)
            | RuntimeValue::DateTime(_)
            | RuntimeValue::Instant(_)
            | RuntimeValue::Duration(_)
            | RuntimeValue::Regex(_)
            | RuntimeValue::Range(_)
            | RuntimeValue::Error(_)
            | RuntimeValue::CmdCall(_)
            | RuntimeValue::CmdArg(_)
            | RuntimeValue::Function(_) => false,

            RuntimeValue::List(_) | RuntimeValue::Map(_) | RuntimeValue::Struct(_) => true,
        }
    }
}

/// Generate types for list of values
fn generate_values_types<'a>(values: impl Iterator<Item = &'a RuntimeValue>) -> Option<ValueType> {
    let mut types = vec![];
    let mut types_hash = HashSet::new();

    for item in values {
        let typ = item.compute_type();

        let mut hasher = DefaultHasher::new();
        typ.hash(&mut hasher);
        let type_hash = hasher.finish();

        if types_hash.insert(type_hash) {
            types.push(typ);
        }
    }

    match types.len() {
        0 => None,
        1 => Some(ValueType::Single(types.remove(0))),
        _ => Some(ValueType::Union(types)),
    }
}

/// Content of an error value
#[derive(Debug, Clone)]
pub struct ErrorValue {
    /// Error location
    pub at: InputRange,

    // TOOD: improve?
    /// Pretty-printed error location
    pub pretty_at: PrettyPrintablePiece,

    /// Data attached to the error
    pub data: RuntimeValue,
}

/// Content of a command call value
#[derive(Debug, Clone)]
pub struct CmdCallValue {
    /// Error location
    pub content_at: InputRange,

    // TOOD: improve?
    /// Pretty-printed error location
    pub pretty_content_at: PrettyPrintablePiece,
}

/// Runtime value with a location
#[derive(Debug, Clone)]
pub struct LocatedValue {
    /// The value itself
    pub value: RuntimeValue,

    /// Where the value comes from
    pub from: RuntimeCodeRange,
}

impl LocatedValue {
    /// Create a located value
    pub fn new(from: RuntimeCodeRange, value: RuntimeValue) -> Self {
        Self { value, from }
    }
}

/// Check if two values are equal
///
/// Nested comparisons will be performed for containers
pub fn are_values_equal(
    left: &RuntimeValue,
    right: &RuntimeValue,
) -> Result<bool, NotComparableTypeErr> {
    match (left, right) {
        (_, RuntimeValue::Void) | (RuntimeValue::Void, _) => Ok(false),

        (RuntimeValue::Null, RuntimeValue::Null) => Ok(true),
        (RuntimeValue::Null, _) | (_, RuntimeValue::Null) => Ok(false),

        (RuntimeValue::Bool(a), RuntimeValue::Bool(b)) => Ok(a == b),
        (RuntimeValue::Bool(_), _) | (_, RuntimeValue::Bool(_)) => Ok(false),

        (RuntimeValue::Int(a), RuntimeValue::Int(b)) => Ok(a == b),
        (RuntimeValue::Int(_), _) | (_, RuntimeValue::Int(_)) => Ok(false),

        (RuntimeValue::Float(a), RuntimeValue::Float(b)) => Ok(a == b),
        (RuntimeValue::Float(_), _) | (_, RuntimeValue::Float(_)) => Ok(false),

        (RuntimeValue::String(a), RuntimeValue::String(b)) => Ok(a == b),
        (RuntimeValue::String(_), _) | (_, RuntimeValue::String(_)) => Ok(false),

        (RuntimeValue::DateTime(a), RuntimeValue::DateTime(b)) => Ok(a == b),
        (RuntimeValue::DateTime(_), _) | (_, RuntimeValue::DateTime(_)) => Ok(false),

        (RuntimeValue::Instant(a), RuntimeValue::Instant(b)) => Ok(a == b),
        (RuntimeValue::Instant(_), _) | (_, RuntimeValue::Instant(_)) => Ok(false),

        (RuntimeValue::Duration(a), RuntimeValue::Duration(b)) => Ok(a == b),
        (RuntimeValue::Duration(_), _) | (_, RuntimeValue::Duration(_)) => Ok(false),

        (RuntimeValue::Regex(_), RuntimeValue::Regex(_)) => Err(NotComparableTypeErr {
            reason: "cannot compare regular expressions",
        }),
        (RuntimeValue::Regex(_), _) | (_, RuntimeValue::Regex(_)) => Ok(false),

        (RuntimeValue::Range(a), RuntimeValue::Range(b)) => Ok(a == b),
        (RuntimeValue::Range(_), _) | (_, RuntimeValue::Range(_)) => Ok(false),

        (RuntimeValue::List(a), RuntimeValue::List(b)) => {
            let a = a.read_promise_no_write();
            let b = b.read_promise_no_write();

            Ok(a.len() == b.len()
                && a.iter()
                    .zip(b.iter())
                    .all(|(a, b)| are_values_equal(a, b).unwrap_or(false)))
        }

        (RuntimeValue::List(_), _) | (_, RuntimeValue::List(_)) => Ok(false),

        (RuntimeValue::Map(a), RuntimeValue::Map(b)) => {
            let a = a.read_promise_no_write();
            let b = b.read_promise_no_write();

            Ok(a.len() == b.len()
                && a.iter().all(|(a_key, a_value)| match b.get(a_key) {
                    None => false,
                    Some(b_value) => are_values_equal(a_value, b_value).unwrap_or_default(),
                }))
        }
        (RuntimeValue::Map(_), _) | (_, RuntimeValue::Map(_)) => Ok(false),

        (RuntimeValue::Struct(a), RuntimeValue::Struct(b)) => {
            let a = a.read_promise_no_write();
            let b = b.read_promise_no_write();

            Ok(a.len() == b.len()
                && a.iter().all(|(a_key, a_value)| match b.get(a_key) {
                    None => false,
                    Some(b_value) => are_values_equal(a_value, b_value).unwrap_or_default(),
                }))
        }
        (RuntimeValue::Struct(_), _) | (_, RuntimeValue::Struct(_)) => Ok(false),

        (RuntimeValue::Error(_), RuntimeValue::Error(_)) => Err(NotComparableTypeErr {
            reason: "cannot compare errors",
        }),
        (RuntimeValue::Error(_), _) | (_, RuntimeValue::Error(_)) => Ok(false),

        (RuntimeValue::CmdCall(_), RuntimeValue::CmdCall(_)) => Err(NotComparableTypeErr {
            reason: "cannot compare command calls",
        }),

        (RuntimeValue::CmdCall(_), _) | (_, RuntimeValue::CmdCall(_)) => {
            // TODO
            Ok(false)
        }

        (RuntimeValue::CmdArg(_), RuntimeValue::CmdArg(_)) => Err(NotComparableTypeErr {
            reason: "cannot compare command arguments",
        }),
        (RuntimeValue::CmdArg(_), _) | (_, RuntimeValue::CmdArg(_)) => Ok(false),

        (RuntimeValue::Function(_), RuntimeValue::Function(_)) => Err(NotComparableTypeErr {
            reason: "cannot compare functions",
        }),

        #[allow(unreachable_patterns)]
        (RuntimeValue::Function(_), _) | (_, RuntimeValue::Function(_)) => Ok(false),
    }
}

/// Error returned when two values are not comparable
pub struct NotComparableTypeErr {
    pub reason: &'static str,
}

/// Convert a value to a string, when possible
///
/// `value`: the value to convert to a string
/// `at`: where the conversion happens (to locate the error in case of conversion issue)
pub fn value_to_str(
    value: &RuntimeValue,
    at: impl Into<RuntimeCodeRange>,
    type_error_tip: impl Into<String>,
    ctx: &Context,
) -> ExecResult<String> {
    match value {
        RuntimeValue::Bool(bool) => Ok(bool.to_string()),
        RuntimeValue::Int(num) => Ok(num.to_string()),
        RuntimeValue::Float(num) => Ok(num.to_string()),
        RuntimeValue::String(str) => Ok(str.clone()),
        RuntimeValue::Void
        | RuntimeValue::Null
        | RuntimeValue::DateTime(_)
        | RuntimeValue::Instant(_)
        | RuntimeValue::Duration(_)
        | RuntimeValue::Regex(_)
        | RuntimeValue::Range(_)
        | RuntimeValue::List(_)
        | RuntimeValue::Map(_)
        | RuntimeValue::Struct(_)
        | RuntimeValue::Function(_)
        | RuntimeValue::Error(_)
        | RuntimeValue::CmdCall(_)
        | RuntimeValue::CmdArg(_) => Err(ctx.hard_error_with_infos(
            at,
            format!(
                "could not convert a value of type {} to a string",
                value.compute_type().display(PrettyPrintOptions::inline())
            ),
            [(ExecInfoType::Note, type_error_tip)],
        )),
    }
}
