use std::{collections::{HashMap, HashSet}, rc::Rc};

use parsy::{CodeRange, Eaten, Location};
use reshell_parser::ast::{ FnSignature, RuntimeCodeRange, ValueType, SingleCmdCall, FunctionBody};

use crate::{errors::CheckerResult, CheckerError};

/// Checker's state
pub struct State {
    scopes: Vec<CheckerScope>,
    pub collected: CheckerOutput,
}

/// Sharing type used to avoid cloning in the runtime
type SharingType<T> = Rc<T>;

fn shared<T>(value: T) -> SharingType<T> {
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
    pub called_alias_name: Eaten<String>
}

impl State {
    /// Create an empty state
    pub fn new() -> Self {
        Self {
            scopes: vec![],
            collected: CheckerOutput::empty(),
        }
    }

    /// Enter a new scope
    pub fn push_scope(&mut self, scope: CheckerScope) {
        self.scopes.push(scope);
    }

    /// Exit the current scope
    pub fn pop_scope(&mut self) {
        self.scopes.pop().unwrap();
    }

    /// Get a reference to the current scope
    pub fn curr_scope(&self) -> &CheckerScope {
        self.scopes.last().unwrap()
    }

    /// Get a mutable reference to the current scope
    pub fn curr_scope_mut(&mut self) -> &mut CheckerScope {
        self.scopes.last_mut().unwrap()
    }

    /// Get the first special scope type in the hierarchy
    /// Used to determine if we're in a loop, in a function, etc.
    pub fn nearest_special_scope_type(&self) -> Option<SpecialScopeType> {
        self.scopes.iter().rev().find_map(|scope| scope.special_scope_type)
    }

    /// Get the first deps-collecting scope in the hierarchy
    /// Used to check if a variable was declared inside that scope or not,
    /// and so if it will require a capture at runtime or not.
    fn current_deps_collecting_scope(&self) -> Option<&CheckerScope> {
        self.scopes.iter().rev().find(|scope| scope.special_scope_type.is_some_and(|typ| typ.captures()))
    }

    /// Get the list of all scopes
    pub fn scopes(&self) -> impl Iterator<Item = &CheckerScope> {
        self.scopes.iter().rev()
    }

    /// Register usage of an item
    /// This function is responsible to check if the item exists and is used correctly,
    /// and if it must be collected.
    /// 
    /// The process of 'collection' is to mark an item as requiring capture at runtime.
    pub fn register_usage(
        &mut self,
        item: &Eaten<String>,
        dep_type: DependencyType,
    ) -> CheckerResult<UsedItem> {
        // Get the item to use
        let item_content = self
            .scopes
            .iter()
            .rev()
            .find_map(|scope| match dep_type {
                DependencyType::Variable => {
                    scope
                        .vars
                        .get(&item.data)
                        .map(|var| Ok(UsedItem::new(var.name_at, var.is_mut)))
                },

                DependencyType::Function => {
                    scope
                        .fns
                        .get(&item.data)
                        .map(|func| Ok(UsedItem::new(func.name_at, false)))
                },

                DependencyType::CmdAlias => {
                    scope
                        .cmd_aliases
                        .get(&item.data)
                        .map(|DeclaredCmdAlias { name_at, content_at: _, is_ready }| if *is_ready {
                            Ok(UsedItem::new(RuntimeCodeRange::Parsed(*name_at), false))
                        } else {
                            Err(CheckerError::new(item.at, "cannot use a command alias before its assignment"))
                        })
                },
            })
            .ok_or_else(|| CheckerError::new(item.at, format!("{dep_type} was not found")))??;

        let UsedItem {
            name_declared_at: declared_at,
            is_mut: _,
        } = item_content;

        // Don't collect items marked as internal
        if matches!(declared_at, RuntimeCodeRange::Internal) {
            return Ok(item_content);
        }

        // Don't collect anything if we're not inside a deps-collecting scope
        let Some(deps_scope) = self.current_deps_collecting_scope() else {
            return Ok(item_content);
        };

        // Don't capture if the value if the item was declared internally (e.g. native library)
        let declared_at = match declared_at {
            RuntimeCodeRange::Parsed(at) => at,
            RuntimeCodeRange::Internal => return Ok(item_content)
        };

        // Get the code range of the dependency-collecting scope
        // It cannot be an internal one, by design
        let deps_scope_range = deps_scope.code_range.parsed_range().unwrap();

        // Determine if the variable was declared in the current deps-collecting scope (or in a descendent scope)
        let var_declared_in_deps_scope = 
                deps_scope_range.contains(declared_at).map_err(|err| {
                CheckerError::new(
                    CodeRange::new(Location { file_id: item.at.start.file_id, offset: 0 }, 0),
                    format!("only source-backed files can be used (detected during var usage analysis): {err:?}"),
                )
            })?;

        // If so, don't capture it
        if var_declared_in_deps_scope {
            return Ok(item_content);
        }

        // If we're inside a function...
        if let Some(SpecialScopeType::Function { args_at }) = deps_scope.special_scope_type {
            // Check if the item is one of its arguments
            let var_declared_in_fn_args = 
                args_at.contains(declared_at).map_err(|err| {
                CheckerError::new(
                    CodeRange::new(Location { file_id: item.at.start.file_id, offset: 0 }, 0),
                    format!("only source-backed files can be used (detected during var usage analysis): {err:?}"),
                )
            })?;

            // If so, don't capture it
            if var_declared_in_fn_args {
                return Ok(item_content);
            }
        }

        // Otherwise, mark the item as a dependency (= will require a capture at runtime)
        self.collected
            .deps
            .get_mut(&deps_scope_range)
            .unwrap()
            .insert(Dependency {
                name: item.data.clone(),
                declared_name_at: declared_at,
                dep_type,
            });

        Ok(item_content)
        
    }

    /// Register usage of a type alias
    /// This function will check if the type alias exists and register the usage globally
    pub fn register_type_alias_usage(&mut self, name: &Eaten<String>) -> CheckerResult {
        let decl_scope = self
            .scopes
            .iter()
            .rev()
            .find(|scope| scope.type_aliases.contains_key(&name.data))
            .ok_or_else(|| {
                CheckerError::new(name.at, format!("type alias {} was not found", name.data))
            })?;

        let Some(decl_scope_at) = decl_scope.code_range.parsed_range() else {
            return Ok(())
        };

        let type_alias = self
            .collected
            .type_aliases_decl_by_scope
            .get(&decl_scope_at)
            .unwrap()
            .get(&name.data)
            .unwrap();

        self.collected
            .type_aliases_usages
            .insert(name.clone(), *type_alias);

        Ok(())
    }

    /// Register a command alias
    pub fn register_cmd_alias(&mut self, alias_content: Eaten<SingleCmdCall>) {
        let dup = self.collected.cmd_aliases.insert(alias_content.at, shared(alias_content));
        assert!(dup.is_none());
    }

    /// Mark a command alias as ready to be used
    /// By default they are not marked as ready to prevent circular references
    /// 
    /// e.g. `alias echo = echo` would result in an infinite loop otherwise
    pub fn mark_cmd_alias_as_ready(&mut self, name: &Eaten<String>) {
        let cmd_alias = self.scopes
            .iter_mut()
            .rev()
            .find_map(|scope| {
                scope.cmd_aliases.get_mut(&name.data)
            }).expect("internal error: command alias to mark as ready was not found");

        cmd_alias.is_ready = true;
    }

    /// Register a function's signature
    pub fn register_function_signature(&mut self, signature: Eaten<FnSignature>) {
        let dup = self.collected.fn_signatures.insert(signature.at, shared(signature));
        assert!(dup.is_none());
    }

    /// Register a function's body
    pub fn register_function_body(&mut self, body: Eaten<FunctionBody>) {
        let dup = self
            .collected
            .fn_bodies
            .insert(body.at, shared(body));

        assert!(dup.is_none());
    }

    /// Register a developed single command call
    pub fn register_developed_single_cmd_call(&mut self, from: &Eaten<SingleCmdCall>, collected: DevelopedSingleCmdCall) {
        assert!(from.at == collected.at);

        let dup = self.collected.cmd_calls.insert(from.at, shared(collected));
        assert!(dup.is_none());
    }
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
    pub(crate) is_ready: bool
}

impl DeclaredCmdAlias {
    pub fn ready( name_at: CodeRange, content_at: CodeRange) -> Self {
        Self { name_at, content_at, is_ready: true }
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

pub struct UsedItem {
    pub name_declared_at: RuntimeCodeRange,
    pub is_mut: bool,
}

impl UsedItem {
    fn new(name_declared_at: RuntimeCodeRange, is_mut: bool) -> Self {
        Self {
            name_declared_at,
            is_mut,
        }
    }
}


impl std::fmt::Display for DependencyType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DependencyType::Variable => write!(f, "variable"),
            DependencyType::Function => write!(f, "function"),
            DependencyType::CmdAlias => write!(f, "command alias"),
        }
    }
}
