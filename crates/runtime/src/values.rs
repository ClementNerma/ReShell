use std::{collections::HashMap, fmt::Debug};

use indexmap::{IndexMap, IndexSet};
use parsy::{CodeRange, Eaten, MaybeEaten};
use reshell_checker::Dependency;
use reshell_parser::ast::{
    Block, FnSignature, SingleCmdCall, SingleValueType, StructTypeMember, ValueType,
};

use crate::context::{ScopeFn, ScopeVar};
use crate::gc::GcReadOnlyCell;
use crate::{context::Context, errors::ExecResult, gc::GcCell};

// TODO: this struct is ultra expensive to clone, put it inside an Arc<> or something?
#[derive(Debug)]
pub struct RuntimeFnValue {
    pub signature: FnSignature,
    pub body: RuntimeFnBody,
    pub parent_scopes: IndexSet<u64>,
    pub captured_deps: CapturedDependencies,
}

// TODO: this struct is pretty expensive to clone, put it inside an Arc<> or something?
#[derive(Debug, Clone)]
pub struct RuntimeCmdAlias {
    pub name_declared_at: CodeRange,
    pub alias_content: GcReadOnlyCell<SingleCmdCall>,
    pub parent_scopes: IndexSet<u64>,
    pub captured_deps: CapturedDependencies,
}

// TODO: this struct is pretty expensive to clone, put it inside an Arc<> or something?
#[derive(Debug, Clone)]
pub struct RuntimeTypeAlias {
    pub name_declared_at: CodeRange,
    pub alias_content: GcReadOnlyCell<ValueType>,
    pub parent_scopes: IndexSet<u64>,
    pub captured_deps: CapturedDependencies,
}

#[derive(Default, Debug, Clone)]
pub struct CapturedDependencies {
    pub vars: HashMap<Dependency, ScopeVar>,
    pub fns: HashMap<Dependency, ScopeFn>,
    pub cmd_aliases: HashMap<Dependency, RuntimeCmdAlias>,
}

#[derive(Clone)]
pub enum RuntimeFnBody {
    // TODO: put a GcCell here
    Block(Eaten<Block>),
    Internal(InternalFnBody),
}

pub struct InternalFnCallData<'c> {
    pub call_at: CodeRange,
    pub args: HashMap<Eaten<String>, LocatedValue>,
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
    Range { from: usize, to: usize },
    Error { at: CodeRange, msg: String },

    // Containers
    // These can be cloned cheaply thanks to them using a GcCell
    List(GcCell<Vec<RuntimeValue>>),
    Map(GcCell<IndexMap<String, RuntimeValue>>),
    Struct(GcCell<IndexMap<String, RuntimeValue>>),
    Function(GcReadOnlyCell<RuntimeFnValue>),
}

impl RuntimeValue {
    pub fn get_type(&self) -> SingleValueType {
        match self {
            RuntimeValue::Null => SingleValueType::Null,
            RuntimeValue::Bool(_) => SingleValueType::Bool,
            RuntimeValue::Int(_) => SingleValueType::Int,
            RuntimeValue::Float(_) => SingleValueType::Float,
            RuntimeValue::String(_) => SingleValueType::String,
            RuntimeValue::List(_) => SingleValueType::List,
            RuntimeValue::Range { from: _, to: _ } => SingleValueType::Range,
            RuntimeValue::Map(_) => SingleValueType::Map,
            RuntimeValue::Struct(members) => SingleValueType::TypedStruct(
                members
                    .read()
                    .iter()
                    .map(|(name, value)| {
                        MaybeEaten::Raw(StructTypeMember {
                            name: MaybeEaten::Raw(name.clone()),
                            typ: MaybeEaten::Raw(ValueType::Single(MaybeEaten::Raw(
                                value.get_type(),
                            ))),
                        })
                    })
                    .collect(),
            ),
            RuntimeValue::Function(content) => {
                // TODO: performance
                SingleValueType::Function(content.signature.clone())
            }
            RuntimeValue::Error { at: _, msg: _ } => SingleValueType::Error,
        }
    }

    pub fn is_primitive(&self) -> bool {
        match self {
            RuntimeValue::Null
            | RuntimeValue::Bool(_)
            | RuntimeValue::Int(_)
            | RuntimeValue::Float(_)
            | RuntimeValue::String(_)
            | RuntimeValue::Range { from: _, to: _ }
            | RuntimeValue::Error { at: _, msg: _ } => true,

            RuntimeValue::List(_)
            | RuntimeValue::Map(_)
            | RuntimeValue::Struct(_)
            | RuntimeValue::Function(_) => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LocatedValue {
    pub value: RuntimeValue,
    pub from: CodeRange,
}

impl LocatedValue {
    pub fn new(value: RuntimeValue, from: CodeRange) -> Self {
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

        (RuntimeValue::Error { at: _, msg: _ }, _) | (_, RuntimeValue::Error { at: _, msg: _ }) => {
            Err(NotComparableTypes)
        }
        (RuntimeValue::Function(_), _) | (_, RuntimeValue::Function(_)) => Err(NotComparableTypes),

        (RuntimeValue::Bool(a), RuntimeValue::Bool(b)) => Ok(a == b),
        (RuntimeValue::Bool(_), _) | (_, RuntimeValue::Bool(_)) => Ok(false),

        (RuntimeValue::Int(a), RuntimeValue::Int(b)) => Ok(a == b),
        (RuntimeValue::Int(_), _) | (_, RuntimeValue::Int(_)) => Ok(false),

        (RuntimeValue::Float(a), RuntimeValue::Float(b)) => Ok(a == b),
        (RuntimeValue::Float(_), _) | (_, RuntimeValue::Float(_)) => Ok(false),

        (RuntimeValue::String(a), RuntimeValue::String(b)) => Ok(a == b),
        (RuntimeValue::String(_), _) | (_, RuntimeValue::String(_)) => Ok(false),

        (RuntimeValue::List(a), RuntimeValue::List(b)) => Ok(a.read().len() == b.read().len()
            && a.read()
                .iter()
                .zip(b.read().iter())
                .all(|(a, b)| are_values_equal(a, b).unwrap_or(false))),
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

        (RuntimeValue::Map(a), RuntimeValue::Map(b)) => Ok(a.read().len() == b.read().len()
            && a.read()
                .iter()
                .zip(b.read().iter())
                .all(|((a_key, a_value), (b_key, b_value))| {
                    a_key == b_key && are_values_equal(a_value, b_value).unwrap_or(false)
                })),
        (RuntimeValue::Map(_), _) | (_, RuntimeValue::Map(_)) => Ok(false),

        (RuntimeValue::Struct(a), RuntimeValue::Struct(b)) => Ok(a.read().len() == b.read().len()
            && a.read()
                .iter()
                .zip(b.read().iter())
                .all(|((a_key, a_value), (b_key, b_value))| {
                    a_key == b_key && are_values_equal(a_value, b_value).unwrap_or(false)
                })),
        // (RuntimeValue::Struct(_), _) | (_, RuntimeValue::Struct(_)) => Ok(false),
    }
}

pub struct NotComparableTypes;
