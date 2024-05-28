use std::any::Any;
use std::collections::HashSet;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::sync::Arc;
use std::{collections::HashMap, fmt::Debug};

use dyn_clone::DynClone;
use indexmap::IndexSet;
use parsy::{CodeRange, Eaten};
use reshell_checker::output::Dependency;
use reshell_parser::{
    ast::{
        Block, FnSignature, RuntimeCodeRange, RuntimeEaten, SingleCmdCall, SingleValueType,
        StructTypeMember, ValueType,
    },
    scope::AstScopeId,
};
use reshell_shared::pretty::{PrettyPrintOptions, PrettyPrintable};

use crate::{
    cmd::CmdSingleArgResult,
    context::{Context, ScopeCmdAlias, ScopeFn, ScopeMethod, ScopeVar},
    errors::{ExecInfoType, ExecResult},
    functions::ValidatedFnCallArg,
    gc::{GcCell, GcOnceCell, GcReadOnlyCell},
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
    pub captured_deps: GcOnceCell<CapturedDependencies>,
}

/// Runtime function signature
#[derive(Debug)]
pub enum RuntimeFnSignature {
    Shared(Arc<Eaten<FnSignature>>),
    Owned(FnSignature),
}

/// Runtime function body
pub enum RuntimeFnBody {
    Block(Arc<Eaten<Block>>),
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
    pub name_declared_at: CodeRange,

    /// Name of the alias
    pub name: String,

    /// Content of the alias
    pub content: Arc<Eaten<SingleCmdCall>>,

    /// Content scope ID
    pub content_scope_id: AstScopeId,

    /// Parent scopes of the alias' declaration
    pub parent_scopes: IndexSet<u64>,

    /// Captured depenencies for evaluation
    pub captured_deps: CapturedDependencies,
}

/// Captured dependencies for a given item
#[derive(Default, Debug, Clone)]
pub struct CapturedDependencies {
    /// Scoped variables
    pub vars: HashMap<Dependency, ScopeVar>,

    /// Scoped functions
    pub fns: HashMap<Dependency, ScopeFn>,

    /// Scoped methods
    pub methods: HashMap<Dependency, ScopeMethod>,

    /// Scoped command aliases
    pub cmd_aliases: HashMap<Dependency, ScopeCmdAlias>,
}

#[derive(Debug, Clone)]
pub enum RuntimeValue {
    // Primitives
    // These can be cloned pretty cheaply
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Range { from: i64, to: i64 },
    Error(Box<ErrorValueContent>),
    CmdCall { content_at: CodeRange },
    CmdArg(Box<CmdSingleArgResult>),

    // Containers
    // These can be cloned cheaply thanks to them using a GcCell
    List(GcCell<Vec<RuntimeValue>>),
    Map(GcCell<HashMap<String, RuntimeValue>>),
    Struct(GcCell<HashMap<String, RuntimeValue>>),
    Function(GcReadOnlyCell<RuntimeFnValue>),

    // Custom value type
    Custom(GcReadOnlyCell<Box<dyn CustomValueType>>),
}

impl RuntimeValue {
    /// Compute the type of a runtime value
    pub fn get_type(&self) -> SingleValueType {
        match self {
            RuntimeValue::Null => SingleValueType::Null,
            RuntimeValue::Bool(_) => SingleValueType::Bool,
            RuntimeValue::Int(_) => SingleValueType::Int,
            RuntimeValue::Float(_) => SingleValueType::Float,
            RuntimeValue::String(_) => SingleValueType::String,
            RuntimeValue::CmdCall { content_at: _ } => SingleValueType::CmdCall,
            RuntimeValue::Range { from: _, to: _ } => SingleValueType::Range,
            RuntimeValue::List(items) => SingleValueType::TypedList(Box::new(
                generate_values_types(items.read_promise_no_write().iter()),
            )),
            RuntimeValue::Map(entries) => {
                SingleValueType::TypedMap(Box::new(generate_values_types(
                    entries
                        .read_promise_no_write()
                        .iter()
                        .map(|(_, value)| value),
                )))
            }
            RuntimeValue::Struct(members) => SingleValueType::TypedStruct(
                members
                    .read_promise_no_write()
                    .iter()
                    .map(|(name, value)| {
                        RuntimeEaten::Internal(
                            StructTypeMember {
                                name: RuntimeEaten::Internal(name.clone(), "type deducer"),
                                typ: RuntimeEaten::Internal(
                                    ValueType::Single(RuntimeEaten::Internal(
                                        value.get_type(),
                                        "type deducer",
                                    )),
                                    "type deducer",
                                ),
                            },
                            "type deducer",
                        )
                    })
                    .collect(),
            ),
            RuntimeValue::Function(content) => {
                // TODO: performance (use already collected data from checker?)
                SingleValueType::Function(match &content.signature {
                    RuntimeFnSignature::Shared(shared) => {
                        RuntimeEaten::Parsed(Eaten::clone(shared))
                    }
                    RuntimeFnSignature::Owned(owned) => {
                        RuntimeEaten::Internal(owned.clone(), "type deducer")
                    }
                })
            }
            RuntimeValue::Error(_) => SingleValueType::Error,
            RuntimeValue::CmdArg(_) => SingleValueType::CmdArg,
            RuntimeValue::Custom(custom) => SingleValueType::Custom(custom.typename()),
        }
    }

    /// Check if a value is a container (e.g. list, map, struct)
    pub fn is_container(&self) -> bool {
        match self {
            RuntimeValue::Null
            | RuntimeValue::Bool(_)
            | RuntimeValue::Int(_)
            | RuntimeValue::Float(_)
            | RuntimeValue::String(_)
            | RuntimeValue::Range { from: _, to: _ }
            | RuntimeValue::Error(_)
            | RuntimeValue::CmdCall { content_at: _ }
            | RuntimeValue::CmdArg(_)
            | RuntimeValue::Function(_)
            | RuntimeValue::Custom(_) => false,

            RuntimeValue::List(_) | RuntimeValue::Map(_) | RuntimeValue::Struct(_) => true,
        }
    }
}

/// Generate types for list of values
fn generate_values_types<'a>(values: impl Iterator<Item = &'a RuntimeValue>) -> ValueType {
    let mut types = vec![];
    let mut types_hash = HashSet::new();

    for item in values {
        let typ = item.get_type();

        let mut hasher = DefaultHasher::new();
        typ.hash(&mut hasher);
        let type_hash = hasher.finish();

        if types_hash.insert(type_hash) {
            types.push(RuntimeEaten::Internal(typ, "internal value type computer"));
        }
    }

    match types.len() {
        0 => ValueType::Single(RuntimeEaten::Internal(
            SingleValueType::Any,
            "internal value type computer",
        )),
        1 => ValueType::Single(types.remove(0)),
        _ => ValueType::Union(types),
    }
}

/// Custom value type (used for e.g. custom builtin types)
pub trait CustomValueType:
    Any + Debug + PrettyPrintable<Context = ()> + DynClone + Send + Sync
{
    fn typename(&self) -> &'static str;

    fn typename_static() -> &'static str
    where
        Self: Sized;
}

/// Content of an error value
#[derive(Debug, Clone)]
pub struct ErrorValueContent {
    /// Error location
    pub at: CodeRange,

    /// Data attached to the error
    pub data: RuntimeValue,
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
    pub fn new(value: RuntimeValue, from: RuntimeCodeRange) -> Self {
        Self { value, from }
    }
}

/// Check if two values are equal
///
/// Nested comparisons will be performed for containers
pub fn are_values_equal(
    left: &RuntimeValue,
    right: &RuntimeValue,
) -> Result<bool, NotComparableTypesErr> {
    match (left, right) {
        (_, RuntimeValue::Null) => Ok(matches!(left, RuntimeValue::Null)),
        (RuntimeValue::Null, _) => Ok(matches!(right, RuntimeValue::Null)),

        (RuntimeValue::Bool(a), RuntimeValue::Bool(b)) => Ok(a == b),
        (RuntimeValue::Bool(_), _) | (_, RuntimeValue::Bool(_)) => Ok(false),

        (RuntimeValue::Int(a), RuntimeValue::Int(b)) => Ok(a == b),
        (RuntimeValue::Int(_), _) | (_, RuntimeValue::Int(_)) => Ok(false),

        (RuntimeValue::Float(a), RuntimeValue::Float(b)) => Ok(a == b),
        (RuntimeValue::Float(_), _) | (_, RuntimeValue::Float(_)) => Ok(false),

        (RuntimeValue::String(a), RuntimeValue::String(b)) => Ok(a == b),
        (RuntimeValue::String(_), _) | (_, RuntimeValue::String(_)) => Ok(false),

        (RuntimeValue::List(a), RuntimeValue::List(b)) => {
            let a = a.read_promise_no_write();
            let b = b.read_promise_no_write();

            Ok(a.len() == b.len()
                && a.iter()
                    .zip(b.iter())
                    .all(|(a, b)| are_values_equal(a, b).unwrap_or(false)))
        }

        (RuntimeValue::List(_), _) | (_, RuntimeValue::List(_)) => Ok(false),

        (
            RuntimeValue::Range {
                from: a_from,
                to: a_to,
            },
            RuntimeValue::Range {
                from: b_from,
                to: b_to,
            },
        ) => Ok(a_from == b_from && a_to == b_to),
        (RuntimeValue::Range { from: _, to: _ }, _)
        | (_, RuntimeValue::Range { from: _, to: _ }) => Ok(false),

        (RuntimeValue::Map(a), RuntimeValue::Map(b)) => {
            let a = a.read_promise_no_write();
            let b = b.read_promise_no_write();

            Ok(a.len() == b.len()
                && a.iter().all(|(a_key, a_value)| match b.get(a_key) {
                    None => false,
                    Some(b_value) => match are_values_equal(a_value, b_value) {
                        Ok(equal) => equal,
                        Err(NotComparableTypesErr { reason: _ }) => false,
                    },
                }))
        }
        (RuntimeValue::Map(_), _) | (_, RuntimeValue::Map(_)) => Ok(false),

        (RuntimeValue::Struct(a), RuntimeValue::Struct(b)) => {
            let a = a.read_promise_no_write();
            let b = b.read_promise_no_write();

            Ok(a.len() == b.len()
                && a.iter().all(|(a_key, a_value)| match b.get(a_key) {
                    None => false,
                    Some(b_value) => match are_values_equal(a_value, b_value) {
                        Ok(equal) => equal,
                        Err(NotComparableTypesErr { reason: _ }) => false,
                    },
                }))
        }
        (RuntimeValue::Struct(_), _) | (_, RuntimeValue::Struct(_)) => Ok(false),

        (RuntimeValue::Error(_), RuntimeValue::Error(_)) => Err(NotComparableTypesErr {
            reason: "cannot compare errors",
        }),
        (RuntimeValue::Error(_), _) | (_, RuntimeValue::Error(_)) => Ok(false),

        (RuntimeValue::CmdCall { content_at: _ }, RuntimeValue::CmdCall { content_at: _ }) => {
            Err(NotComparableTypesErr {
                reason: "cannot compare command calls",
            })
        }
        (RuntimeValue::CmdCall { content_at: _ }, _)
        | (_, RuntimeValue::CmdCall { content_at: _ }) => Ok(false),

        (RuntimeValue::Function(_), RuntimeValue::Function(_)) => Err(NotComparableTypesErr {
            reason: "cannot compare functions",
        }),
        (RuntimeValue::Function(_), _) | (_, RuntimeValue::Function(_)) => Ok(false),

        (RuntimeValue::CmdArg(_), RuntimeValue::CmdArg(_)) => Err(NotComparableTypesErr {
            reason: "cannot compare value arguments",
        }),
        (RuntimeValue::CmdArg(_), _) | (_, RuntimeValue::CmdArg(_)) => Ok(false),

        (RuntimeValue::Custom(_), RuntimeValue::Custom(_)) => Err(NotComparableTypesErr {
            reason: "cannot compare custom types",
        }),
        // (RuntimeValue::Custom(_), _) | (_, RuntimeValue::Custom(_)) => Ok(false),
    }
}

/// Error returned when two values are not comparable
pub struct NotComparableTypesErr {
    pub reason: &'static str,
}

/// Convert a value to a string, when possible
///
/// `value`: the value to convert to a string
/// `err_tip`: tip to provide with the error if the conversion is not possible
/// `at`: where the conversion happens (to locate the error in case of conversion issue)
pub fn value_to_str(
    value: &RuntimeValue,
    err_tip: impl Into<String>,
    at: impl Into<RuntimeCodeRange>,
    ctx: &Context,
) -> ExecResult<String> {
    match value {
        RuntimeValue::Bool(bool) => Ok(bool.to_string()),
        RuntimeValue::Int(num) => Ok(num.to_string()),
        RuntimeValue::Float(num) => Ok(num.to_string()),
        RuntimeValue::String(str) => Ok(str.clone()),
        RuntimeValue::Null
        | RuntimeValue::List(_)
        | RuntimeValue::Range { from: _, to: _ }
        | RuntimeValue::Map(_)
        | RuntimeValue::Struct(_)
        | RuntimeValue::Function(_)
        | RuntimeValue::Error(_)
        | RuntimeValue::CmdCall { content_at: _ }
        | RuntimeValue::CmdArg(_)
        | RuntimeValue::Custom(_) // TODO?
        => Err(ctx
            .error(
                at,
                format!(
                    "cannot convert {} to a string",
                    value
                        .get_type()
                        .render_colored(ctx, PrettyPrintOptions::inline())
                ),
            )
            .with_info(
                ExecInfoType::Tip,
                err_tip,
            )),
    }
}
