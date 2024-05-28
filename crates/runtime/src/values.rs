use std::any::Any;
use std::rc::Rc;
use std::{collections::HashMap, fmt::Debug};

use indexmap::IndexSet;
use parsy::{CodeRange, Eaten};
use reshell_checker::output::Dependency;
use reshell_parser::ast::{
    FnSignature, FunctionBody, RuntimeCodeRange, RuntimeEaten, SingleCmdCall, SingleValueType,
    StructTypeMember, ValueType,
};

use crate::cmd::CmdSingleArgResult;
use crate::context::{ScopeCmdAlias, ScopeFn, ScopeVar};
use crate::functions::ValidatedFnCallArg;
use crate::gc::{GcOnceCell, GcReadOnlyCell};
use crate::pretty::PrettyPrintable;
use crate::{context::Context, errors::ExecResult, gc::GcCell};

#[derive(Debug)]
pub struct RuntimeFnValue {
    pub signature: RuntimeFnSignature,
    pub body: RuntimeFnBody,
    pub parent_scopes: IndexSet<u64>,

    /// Function's captured dependencies
    /// Uninit before the function's actual declaration point
    pub captured_deps: GcOnceCell<CapturedDependencies>,
}

#[derive(Debug)]
pub enum RuntimeFnSignature {
    Shared(Rc<Eaten<FnSignature>>),
    Owned(FnSignature),
}

impl RuntimeFnSignature {
    pub fn inner(&self) -> &FnSignature {
        match &self {
            RuntimeFnSignature::Shared(shared) => &shared.data,
            RuntimeFnSignature::Owned(owned) => owned,
        }
    }
}

#[derive(Debug, Clone)]
pub struct RuntimeCmdAlias {
    pub name_declared_at: CodeRange,
    pub alias_content: Rc<Eaten<SingleCmdCall>>,
    pub parent_scopes: IndexSet<u64>,
    pub captured_deps: CapturedDependencies,
}

#[derive(Default, Debug, Clone)]
pub struct CapturedDependencies {
    pub vars: HashMap<Dependency, ScopeVar>,
    pub fns: HashMap<Dependency, ScopeFn>,
    pub cmd_aliases: HashMap<Dependency, ScopeCmdAlias>,
}

pub enum RuntimeFnBody {
    Block(Rc<Eaten<FunctionBody>>),
    Internal(InternalFnBody),
}

pub struct InternalFnCallData<'c> {
    pub call_at: RuntimeCodeRange,
    pub args: HashMap<String, ValidatedFnCallArg>,
    pub ctx: &'c mut Context,
}

pub type InternalFnBody = fn(InternalFnCallData) -> ExecResult<Option<LocatedValue>>;

impl Debug for RuntimeFnBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Block(block) => f.debug_tuple("Block").field(block).finish(),
            Self::Internal(_) => f.debug_tuple("Internal").finish(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum RuntimeValue {
    // Primitives
    // These can be cloned cheaply
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Range { from: i64, to: i64 },
    Error(Box<ErrorValueContent>),
    CmdCall { content_at: CodeRange },

    // Containers
    // These can be cloned cheaply thanks to them using a GcCell
    List(GcCell<Vec<RuntimeValue>>),
    Map(GcCell<HashMap<String, RuntimeValue>>),
    Struct(GcCell<HashMap<String, RuntimeValue>>),
    Function(GcReadOnlyCell<RuntimeFnValue>),
    ArgSpread(GcReadOnlyCell<Vec<CmdSingleArgResult>>),

    // Custom value type
    Custom(GcReadOnlyCell<dyn CustomValueType>),
}

impl RuntimeValue {
    pub fn get_type(&self) -> SingleValueType {
        match self {
            RuntimeValue::Null => SingleValueType::Null,
            RuntimeValue::Bool(_) => SingleValueType::Bool,
            RuntimeValue::Int(_) => SingleValueType::Int,
            RuntimeValue::Float(_) => SingleValueType::Float,
            RuntimeValue::String(_) => SingleValueType::String,
            RuntimeValue::CmdCall { content_at: _ } => SingleValueType::CmdCall,
            RuntimeValue::List(_) => SingleValueType::List,
            RuntimeValue::Range { from: _, to: _ } => SingleValueType::Range,
            RuntimeValue::Map(_) => SingleValueType::Map,
            RuntimeValue::Struct(members) => SingleValueType::TypedStruct(
                members
                    .read_promise_no_write()
                    .iter()
                    .map(|(name, value)| {
                        RuntimeEaten::Internal(StructTypeMember {
                            name: RuntimeEaten::Internal(name.clone()),
                            typ: RuntimeEaten::Internal(ValueType::Single(RuntimeEaten::Internal(
                                value.get_type(),
                            ))),
                        })
                    })
                    .collect(),
            ),
            RuntimeValue::Function(content) => {
                // TODO: performance (use already collected data from checker?)
                SingleValueType::Function(match &content.signature {
                    RuntimeFnSignature::Shared(shared) => {
                        RuntimeEaten::Parsed(Eaten::clone(shared))
                    }
                    RuntimeFnSignature::Owned(owned) => RuntimeEaten::Internal(owned.clone()),
                })
            }
            RuntimeValue::Error(_) => SingleValueType::Error,
            RuntimeValue::ArgSpread(_) => SingleValueType::ArgSpread,
            RuntimeValue::Custom(custom) => SingleValueType::Custom(custom.typename()),
        }
    }

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
            | RuntimeValue::ArgSpread(_)
            | RuntimeValue::Function(_)
            | RuntimeValue::Custom(_) => false,

            RuntimeValue::List(_) | RuntimeValue::Map(_) | RuntimeValue::Struct(_) => true,
        }
    }
}

/// Custom value type
pub trait CustomValueType: Any + Debug + PrettyPrintable {
    fn typename(&self) -> &'static str;
}

#[derive(Debug, Clone)]
pub struct ErrorValueContent {
    pub at: CodeRange,
    pub msg: String,
}

#[derive(Debug, Clone)]
pub struct LocatedValue {
    pub value: RuntimeValue,
    pub from: RuntimeCodeRange,
}

impl LocatedValue {
    pub fn new(value: RuntimeValue, from: RuntimeCodeRange) -> Self {
        Self { value, from }
    }
}

pub fn are_values_equal(
    left: &RuntimeValue,
    right: &RuntimeValue,
) -> Result<bool, NotComparableTypes> {
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
                        Err(NotComparableTypes { reason: _ }) => false,
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
                        Err(NotComparableTypes { reason: _ }) => false,
                    },
                }))
        }
        (RuntimeValue::Struct(_), _) | (_, RuntimeValue::Struct(_)) => Ok(false),

        (RuntimeValue::Error(_), RuntimeValue::Error(_)) => Err(NotComparableTypes {
            reason: "cannot compare errors",
        }),
        (RuntimeValue::Error(_), _) | (_, RuntimeValue::Error(_)) => Ok(false),

        (RuntimeValue::CmdCall { content_at: _ }, RuntimeValue::CmdCall { content_at: _ }) => {
            Err(NotComparableTypes {
                reason: "cannot compare command calls",
            })
        }
        (RuntimeValue::CmdCall { content_at: _ }, _)
        | (_, RuntimeValue::CmdCall { content_at: _ }) => Ok(false),

        (RuntimeValue::Function(_), RuntimeValue::Function(_)) => Err(NotComparableTypes {
            reason: "cannot compare functions",
        }),
        (RuntimeValue::Function(_), _) | (_, RuntimeValue::Function(_)) => Ok(false),

        (RuntimeValue::ArgSpread(_), RuntimeValue::ArgSpread(_)) => Err(NotComparableTypes {
            reason: "cannot compare arguments spreads",
        }),
        (RuntimeValue::ArgSpread(_), _) | (_, RuntimeValue::ArgSpread(_)) => Ok(false),

        (RuntimeValue::Custom(_), RuntimeValue::Custom(_)) => Err(NotComparableTypes {
            reason: "cannot compare custom types",
        }),
        // (RuntimeValue::Custom(_), _) | (_, RuntimeValue::Custom(_)) => Ok(false),
    }
}

pub struct NotComparableTypes {
    pub reason: &'static str,
}
