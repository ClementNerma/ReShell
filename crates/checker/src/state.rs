use std::collections::{HashMap, HashSet};

use parsy::{CodeRange, Eaten};
use reshell_parser::{
    ast::{Block, CmdCall, FnSignature, FunctionBody, SingleCmdCall, ValueType},
    scope::{AstScopeId, NATIVE_LIB_AST_SCOPE_ID},
};

use crate::{errors::CheckerResult, output::*, CheckerError};

/// Checker's state
pub struct State {
    scopes: Vec<CheckerScope>,
    collected: CheckerOutput,
}

impl State {
    /// Create an empty state
    pub fn new(prev: Option<CheckerOutput>) -> Self {
        Self {
            scopes: vec![],
            collected: prev.unwrap_or_else(CheckerOutput::empty),
        }
    }

    pub fn consume(self) -> CheckerOutput {
        self.collected
    }

    /// Enter a new scope
    pub fn push_scope(&mut self, scope: CheckerScope) {
        assert!(!self.scopes.iter().any(|c| c.id == scope.id));

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
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.special_scope_type)
    }

    /// Get the first deps-collecting scope in the hierarchy
    /// Used to check if a variable was declared inside that scope or not,
    /// and so if it will require a capture at runtime or not.
    fn current_deps_collecting_scope(&self) -> Option<&CheckerScope> {
        self.scopes
            .iter()
            .rev()
            .find(|scope| scope.special_scope_type.is_some_and(|typ| typ.captures()))
    }

    /// Get the list of all scopes
    pub fn scopes(&self) -> impl Iterator<Item = &CheckerScope> {
        self.scopes.iter().rev()
    }

    /// Prepare deps for a block
    pub fn prepare_deps(&mut self, scope_id: AstScopeId) {
        let dup = self.collected.deps.insert(scope_id, HashSet::new());
        assert!(dup.is_none());
    }

    /// Retrieve a registered developed command call
    pub fn get_developed_cmd_call_at(
        &self,
        at: CodeRange,
    ) -> Option<SharingType<DevelopedSingleCmdCall>> {
        self.collected.cmd_calls.get(&at).map(SharingType::clone)
    }

    /// Register usage of an item
    /// This function is responsible to check if the item exists and is used correctly,
    /// and if it must be collected.
    ///
    /// The process of 'collection' is to mark an item as requiring capture at runtime.
    pub fn register_usage(
        &mut self,
        item_usage: &Eaten<String>,
        dep_type: DependencyType,
    ) -> CheckerResult<UsedItem> {
        // Get the item to use
        let item = self
            .scopes
            .iter()
            .rev()
            .find_map(|scope| match dep_type {
                DependencyType::Variable => scope
                    .vars
                    .get(&item_usage.data)
                    .copied()
                    .map(UsedItem::Variable)
                    .map(Ok),

                DependencyType::Function => scope
                    .fns
                    .get(&item_usage.data)
                    .copied()
                    .map(UsedItem::Function)
                    .map(Ok),

                DependencyType::CmdAlias => scope.cmd_aliases.get(&item_usage.data).map(|alias| {
                    if alias.is_ready {
                        Ok(UsedItem::CmdAlias(*alias))
                    } else {
                        Err(CheckerError::new(
                            item_usage.at,
                            "cannot use a command alias before its assignment",
                        ))
                    }
                }),
            })
            .ok_or_else(|| {
                CheckerError::new(item_usage.at, format!("{dep_type} was not found"))
            })??;

        let item_declared_in = match item {
            UsedItem::Variable(var) => var.scope_id,
            UsedItem::Function(func) => func.scope_id,
            UsedItem::CmdAlias(cmd_alias) => cmd_alias.scope_id,
        };

        // Don't capture if the value if the item was declared internally (e.g. native library)
        if item_declared_in == NATIVE_LIB_AST_SCOPE_ID {
            return Ok(item);
        }

        // Don't collect anything if we're not inside a deps-collecting scope
        let Some(deps_scope) = self.current_deps_collecting_scope() else {
            return Ok(item);
        };

        // Determine if the variable was declared in the current deps-collecting scope (or in a descendent scope)
        let deps_scope_index = self
            .scopes
            .iter()
            .position(|scope| scope.id == deps_scope.id)
            .unwrap();

        let var_declared_in_deps_scope = self
            .scopes
            .iter()
            .skip(deps_scope_index)
            .rev()
            .any(|scope| scope.id == item_declared_in);

        // If so, don't capture it
        if var_declared_in_deps_scope {
            return Ok(item);
        }

        // Otherwise, mark the item as a dependency (= will require a capture at runtime)
        let deps_scope_id = deps_scope.id;

        self.collected
            .deps
            .get_mut(&deps_scope_id)
            .unwrap()
            .insert(Dependency {
                name: item_usage.data.clone(),
                declared_in: item_declared_in,
                dep_type,
            });

        Ok(item)
    }

    /// Register a type alias declaration
    pub fn register_type_alias(
        &mut self,
        name: &Eaten<String>,
        content: &Eaten<ValueType>,
        inside_block: &Eaten<Block>,
    ) {
        self.collected
            .type_aliases_decl
            .insert(name.at, content.clone());

        self.collected
            .type_aliases_decl_by_scope
            .entry(inside_block.data.scope_id)
            .or_default()
            .insert(name.data.clone(), name.at);
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

        let decl_scope_id = decl_scope.id;

        if decl_scope_id == NATIVE_LIB_AST_SCOPE_ID {
            return Ok(());
        }

        let type_alias = self
            .collected
            .type_aliases_decl_by_scope
            .get(&decl_scope_id)
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
        let dup = self
            .collected
            .cmd_aliases
            .insert(alias_content.at, shared(alias_content));
        assert!(dup.is_none());
    }

    /// Mark a command alias as ready to be used
    /// By default they are not marked as ready to prevent circular references
    ///
    /// e.g. `alias echo = echo` would result in an infinite loop otherwise
    pub fn mark_cmd_alias_as_ready(&mut self, name: &Eaten<String>) {
        let cmd_alias = self
            .scopes
            .iter_mut()
            .rev()
            .find_map(|scope| scope.cmd_aliases.get_mut(&name.data))
            .expect("internal error: command alias to mark as ready was not found");

        cmd_alias.is_ready = true;
    }

    /// Register a function's signature
    pub fn register_function_signature(&mut self, signature: Eaten<FnSignature>) {
        let dup = self
            .collected
            .fn_signatures
            .insert(signature.at, shared(signature));
        assert!(dup.is_none());
    }

    /// Register a function's body
    pub fn register_function_body(&mut self, body: Eaten<FunctionBody>) {
        let dup = self.collected.fn_bodies.insert(body.at, shared(body));

        assert!(dup.is_none());
    }

    /// Register a developed single command call
    pub fn register_developed_single_cmd_call(
        &mut self,
        from: &Eaten<SingleCmdCall>,
        collected: DevelopedSingleCmdCall,
    ) {
        assert!(from.at == collected.at);

        let dup = self.collected.cmd_calls.insert(from.at, shared(collected));
        assert!(dup.is_none());
    }

    /// Register a command call used as a value
    pub fn register_cmd_call_value(&mut self, from: &Eaten<CmdCall>) {
        let dup = self
            .collected
            .cmd_call_values
            .insert(from.at, shared(from.clone()));
        assert!(dup.is_none());
    }
}

pub enum UsedItem {
    Variable(DeclaredVar),
    Function(DeclaredFn),
    CmdAlias(DeclaredCmdAlias),
}

/// Scope in the checker
#[derive(Clone)]
pub struct CheckerScope {
    /// Scope ID
    pub id: AstScopeId,

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
    /// Scope the variable is defined in
    pub scope_id: AstScopeId,

    /// Is the variable mutable?
    pub is_mut: bool,
}

/// Function declaration
#[derive(Clone, Copy)]
pub struct DeclaredFn {
    /// Scope the function is defined in
    pub scope_id: AstScopeId,

    /// Is it a method?
    pub is_method: bool,
}

/// Command alias declaration
#[derive(Clone, Copy)]
pub struct DeclaredCmdAlias {
    /// Scope the comment alias is defined in
    pub scope_id: AstScopeId,

    /// Location of the command alias' content in its declaration
    pub content_at: CodeRange,

    /// Is the declared alias ready?
    /// See [`Context::mark_cmd_alias_as_ready`]
    pub is_ready: bool,
}
