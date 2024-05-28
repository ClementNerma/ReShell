use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use parsy::{CodeRange, Eaten};
use reshell_parser::ast::{
    CmdCall, FnSignature, FunctionBody, RuntimeCodeRange, SingleCmdCall, ValueType,
};

/// Sharing type used to avoid cloning in the runtime
pub type SharingType<T> = Rc<T>;

pub(crate) fn shared<T>(value: T) -> SharingType<T> {
    Rc::new(value)
}

/// Data returned by the checker in case of success
#[derive(Debug)]
pub struct CheckerOutput {
    /// Dependencies list
    ///
    /// Associates an item's content (e.g. a command alias' content or a function's body)
    /// with a set of dependency, which will be captured when encountering this item at runtime
    ///
    /// This is filled by the [`State::register_usage`] method
    ///
    /// Capture is required in order to drop scopes safely without losing a reference to the original value
    /// from other values (e.g. functions) when they are returned to an outer scope
    pub deps: HashMap<CodeRange, HashSet<Dependency>>,

    /// List of type aliases declaration
    ///
    /// Maps the type alias' name token location to its content
    pub type_aliases_decl: HashMap<CodeRange, Eaten<ValueType>>,

    /// List of all type aliases usage
    ///
    /// Maps the type alias' name usage token to the type alias' location.
    /// The alias can then be retrieved using `type_aliases_decl` in this struct
    ///
    /// This is useful to determine what type alias a type is referring to,
    /// especially when multiple type aliases in different scopes have the same name
    pub type_aliases_usages: HashMap<Eaten<String>, CodeRange>,

    /// List of all type aliases declaration, by scope
    ///
    /// Associates a scope's code range to a mapping between the type aliases' name and location.
    /// The aliases can then be retrieved using `type_alias_decl` in this struct
    pub type_aliases_decl_by_scope: HashMap<CodeRange, HashMap<String, CodeRange>>,

    /// Signature of all functions and closures
    ///
    /// Maps the signature's location to its content
    pub fn_signatures: HashMap<CodeRange, SharingType<Eaten<FnSignature>>>,

    /// Body of all functions and closures
    ///
    /// Maps the body's location to its content
    pub fn_bodies: HashMap<CodeRange, SharingType<Eaten<FunctionBody>>>,

    /// List of command aliases
    ///
    /// Maps the command alias' content token location to its content
    pub cmd_aliases: HashMap<CodeRange, SharingType<Eaten<SingleCmdCall>>>,

    /// List of command calls
    ///
    /// Used to speed up runtime by collecting informations on command calls
    /// ahead of time
    ///
    /// Maps the single command's token location to the expanded call
    pub cmd_calls: HashMap<CodeRange, SharingType<DevelopedSingleCmdCall>>,

    /// List of command calls as values
    ///
    /// Used to avoid cloning Eaten<CmdCall> every time a value is used
    pub cmd_call_values: HashMap<CodeRange, SharingType<Eaten<CmdCall>>>,
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

    pub fn merge(&mut self, other: CheckerOutput) {
        #[deny(unused_variables)]
        let CheckerOutput {
            deps,
            type_aliases_decl,
            type_aliases_usages,
            type_aliases_decl_by_scope,
            fn_signatures,
            fn_bodies,
            cmd_aliases,
            cmd_calls,
            cmd_call_values,
        } = other;

        self.deps.extend(deps);
        self.type_aliases_decl.extend(type_aliases_decl);
        self.type_aliases_usages.extend(type_aliases_usages);
        self.type_aliases_decl_by_scope
            .extend(type_aliases_decl_by_scope);
        self.cmd_calls.extend(cmd_calls);
        self.fn_signatures.extend(fn_signatures);
        self.fn_bodies.extend(fn_bodies);
        self.cmd_aliases.extend(cmd_aliases);
        self.cmd_call_values.extend(cmd_call_values);
    }

    // TODO: find a way to avoid a massive cloning
    pub fn reuse_in_checker(&self) -> Self {
        Self {
            deps: self.deps.clone(),
            type_aliases_decl: self.type_aliases_decl.clone(),
            type_aliases_usages: self.type_aliases_usages.clone(),
            type_aliases_decl_by_scope: self.type_aliases_decl_by_scope.clone(),
            fn_signatures: self.fn_signatures.clone(),
            fn_bodies: self.fn_bodies.clone(),
            cmd_aliases: self.cmd_aliases.clone(),
            cmd_calls: self.cmd_calls.clone(),
            cmd_call_values: self.cmd_call_values.clone(),
        }
    }
}

/// Developed command call
#[derive(Debug, Clone)]
pub struct DevelopedSingleCmdCall {
    /// Location that can be found in the related [`Eaten::<SingleCmdCall>::at`]
    pub at: CodeRange,

    /// Is the target a function?
    pub is_function: bool,

    /// Developed aliases
    pub developed_aliases: Vec<DevelopedCmdAliasCall>,
}

/// Developed command alias call
#[derive(Debug, Clone)]
pub struct DevelopedCmdAliasCall {
    /// Alias content's location ([`Eaten::<SingleCmdCall>::at`])
    pub content_at: CodeRange,

    /// Alias name
    pub called_alias_name: Eaten<String>,
}

/// Scope in the checker
#[derive(Clone)]
pub struct CheckerScope {
    /// Code range covered by the scope
    pub code_range: RuntimeCodeRange,

    /// Optional special scope type
    pub special_scope_type: Option<SpecialScopeType>,

    /// List of variables declared in this scope
    pub vars: HashMap<String, DeclaredVar>,

    /// List of functions declared in this scope
    pub fns: HashMap<String, DeclaredFn>,

    /// List of command aliases declared in this scope
    pub cmd_aliases: HashMap<String, DeclaredCmdAlias>,

    /// List of type aliases declared in this scope
    pub type_aliases: HashMap<String, CodeRange>,
}

#[derive(Clone, Copy)]
pub enum SpecialScopeType {
    /// Inside the body of a function
    /// Will provoke dependencies collection inside it
    Function { args_at: CodeRange },

    /// Inside the content of a command alias
    /// Will provoke dependencies collection inside it
    CmdAlias,

    /// Inside the body of a loop
    Loop,
}

impl SpecialScopeType {
    pub fn captures(&self) -> bool {
        match self {
            SpecialScopeType::Function { args_at: _ } | SpecialScopeType::CmdAlias => true,
            SpecialScopeType::Loop => false,
        }
    }
}

/// Variable declaration
#[derive(Clone, Copy)]
pub struct DeclaredVar {
    /// Location of the variable's name in its declaration
    pub name_at: RuntimeCodeRange,

    /// Is the variable mutable?
    pub is_mut: bool,
}

/// Function declaration
#[derive(Clone, Copy)]
pub struct DeclaredFn {
    /// Location of the function's name in its declaration
    pub name_at: RuntimeCodeRange,
}

impl DeclaredFn {
    pub fn new(name_at: RuntimeCodeRange) -> Self {
        Self { name_at }
    }
}

/// Command alias declaration
#[derive(Clone, Copy)]
pub struct DeclaredCmdAlias {
    /// Location of the command alias' name in its declaration
    pub name_at: CodeRange,

    /// Location of the command alias' content in its declaration
    pub content_at: CodeRange,

    /// Is the declared alias ready?
    /// See [`Context::mark_cmd_alias_as_ready`]
    pub(crate) is_ready: bool,
}

impl DeclaredCmdAlias {
    pub fn ready(name_at: CodeRange, content_at: CodeRange) -> Self {
        Self {
            name_at,
            content_at,
            is_ready: true,
        }
    }
}

/// Description of an item that will require capture at runtime
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Dependency {
    /// Name of the item
    pub name: String,

    /// Location of the item's name in its original declaration
    pub declared_name_at: CodeRange,

    /// Type of dependency
    pub dep_type: DependencyType,
}

/// Type of dependency
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum DependencyType {
    /// A variable
    Variable,

    /// A function
    Function,

    /// A command alias
    CmdAlias,
}
