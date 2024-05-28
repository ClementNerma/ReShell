use std::{collections::HashMap, fmt::Debug};

use parsy::{CodeRange, Eaten, MaybeEaten};
use reshell_parser::ast::{Block, FnSignature, SingleValueType, StructTypeMember, ValueType};

use crate::{context::Context, errors::ExecResult};

#[derive(Debug, Clone)]
pub struct RuntimeFnValue {
    pub signature: FnSignature,
    pub body: RuntimeFnBody,
}

#[derive(Clone)]
pub enum RuntimeFnBody {
    Block(Eaten<Block>),
    Internal(InternalFnBody),
}

pub type InternalFnBody =
    fn(CodeRange, HashMap<String, LocatedValue>, &mut Context) -> ExecResult<Option<LocatedValue>>;

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
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    List(Vec<RuntimeValue>),
    Range { from: usize, to: usize },
    Map(HashMap<String, RuntimeValue>),
    Struct(HashMap<String, RuntimeValue>),
    Function(RuntimeFnValue),
    Error { at: CodeRange, msg: String },
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
            RuntimeValue::Function(RuntimeFnValue { signature, body: _ }) => {
                // TODO: performance
                SingleValueType::Function(signature.clone())
            }
            RuntimeValue::Error { at: _, msg: _ } => SingleValueType::Error,
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
