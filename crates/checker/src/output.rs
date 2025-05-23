use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use parsy::{CodeRange, Span};
use reshell_parser::ast::{AstScopeId, Block, CmdCall, FnSignature, SingleCmdCall, ValueType};

/// Sharing type used to avoid cloning in the runtime
pub type SharingType<T> = Arc<T>;

pub(crate) fn shared<T>(value: T) -> SharingType<T> {
    Arc::new(value)
}

/// Data returned by the checker in case of success
#[derive(Debug, Clone)]
pub struct CheckerOutput {
    /// Dependencies list
    ///
    /// Associates a scope to set of items
    /// Then associates item's content (e.g. a command alias' content or a function's body)
    /// with a set of dependency, which will be captured when encountering this item at runtime
    ///
    /// This is filled by the [`crate::State::register_single_usage`] method
    ///
    /// Capture is required in order to drop scopes safely without losing a reference to the original value
    /// from other values (e.g. functions) when they are returned to an outer scope
    pub deps: HashMap<AstScopeId, HashSet<Dependency>>,

    /// List of type aliases declaration
    ///
    /// Maps the type alias' name token location to its content
    pub type_aliases_decl: HashMap<CodeRange, SharingType<Span<ValueType>>>,

    /// List of all type aliases usage
    ///
    /// Maps the type alias' name usage token to the type alias' location.
    /// The alias can then be retrieved using `type_aliases_decl` in this struct
    ///
    /// This is useful to determine what type alias a type is referring to,
    /// especially when multiple type aliases in different scopes have the same name
    pub type_aliases_usages: HashMap<Span<String>, SharingType<Span<ValueType>>>,

    /// List of all type aliases declaration, by scope
    ///
    /// Associates a scope's ID to a mapping between the type aliases' name and location.
    /// The aliases can then be retrieved using `type_alias_decl` in this struct
    pub type_aliases_decl_by_scope:
        HashMap<AstScopeId, HashMap<String, SharingType<Span<ValueType>>>>,

    /// Signature of all functions and lambdas
    ///
    /// Maps the signature's location to its content
    pub fn_signatures: HashMap<CodeRange, SharingType<Span<FnSignature>>>,

    /// Body of all functions and lambdas
    ///
    /// Maps the body's location to its content
    pub fn_bodies: HashMap<CodeRange, SharingType<Span<Block>>>,

    /// List of command aliases
    ///
    /// Maps the command alias' content token location to its content
    pub cmd_aliases: HashMap<CodeRange, SharingType<Span<SingleCmdCall>>>,

    /// List of command calls
    ///
    /// Used to speed up runtime by collecting informations on command calls
    /// ahead of time
    ///
    /// Maps the single command's token location to the expanded call
    pub cmd_calls: HashMap<CodeRange, SharingType<DevelopedSingleCmdCall>>,

    /// List of command calls as values
    ///
    /// Used to avoid cloning Span<CmdCall> every time a value is used
    pub cmd_call_values: HashMap<CodeRange, SharingType<Span<CmdCall>>>,
}

impl CheckerOutput {
    pub fn empty() -> Self {
        Self {
            deps: HashMap::new(),
            type_aliases_decl: HashMap::new(),
            type_aliases_usages: HashMap::new(),
            type_aliases_decl_by_scope: HashMap::new(),
            fn_signatures: HashMap::new(),
            fn_bodies: HashMap::new(),
            cmd_aliases: HashMap::new(),
            cmd_calls: HashMap::new(),
            cmd_call_values: HashMap::new(),
        }
    }
}

/// Developed command call
#[derive(Debug, Clone)]
pub struct DevelopedSingleCmdCall {
    /// Location that can be found in the related [`Span::<SingleCmdCall>::at`]
    pub at: CodeRange,

    /// Is the target a function?
    pub is_function: bool,

    /// Developed aliases
    pub developed_aliases: Vec<DevelopedCmdAliasCall>,
}

/// Developed command alias call
#[derive(Debug, Clone)]
pub struct DevelopedCmdAliasCall {
    /// Location where the alias was called
    pub alias_called_at: Span<String>,

    /// Location of the alias' content
    pub alias_content_at: CodeRange,
}

/// Description of an item that will require capture at runtime
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Dependency {
    /// Name of the item
    pub name: String,

    /// Scope the item was declared in
    pub declared_in: AstScopeId,

    /// Type of dependency
    pub dep_type: DependencyType,
}

/// Type of dependency
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum DependencyType {
    Variable,
    Function,
    Method,
    CmdAlias,
}

impl std::fmt::Display for DependencyType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DependencyType::Variable => write!(f, "variable"),
            DependencyType::Function => write!(f, "function"),
            DependencyType::Method => write!(f, "method"),
            DependencyType::CmdAlias => write!(f, "command alias"),
        }
    }
}
