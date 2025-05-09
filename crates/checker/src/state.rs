use std::collections::{HashMap, HashSet};

use parsy::{CodeRange, Span};
use reshell_parser::{
    NATIVE_LIB_AST_SCOPE_ID,
    ast::{AstScopeId, Block, CmdCall, FnSignature, RuntimeCodeRange, SingleCmdCall, ValueType},
};
use reshell_prettify::TypeAliasStore;

use crate::{CheckerError, errors::CheckerResult, output::*};

/// Checker's state
pub struct State<'a> {
    scopes: Vec<CheckerScope>,
    collected: &'a mut CheckerOutput,
}

impl<'a> State<'a> {
    /// Create a new state
    pub fn new(prev: &'a mut CheckerOutput) -> Self {
        Self {
            scopes: vec![],
            collected: prev,
        }
    }

    /// Get type alias store
    pub fn type_alias_store(&self) -> &TypeAliasStore {
        &self.collected.type_aliases_usages
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
        self.scopes.iter().rev().find(|scope| {
            scope
                .special_scope_type
                .is_some_and(|typ| typ.requires_deps_capture())
        })
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
        #[allow(clippy::map_clone)]
        self.collected.cmd_calls.get(&at).map(SharingType::clone)
    }

    /// Register usage of a variable
    /// This function is responsible to check if the variable exists and is used correctly,
    /// and if it must be collected.
    ///
    /// The process of 'collection' is to mark an item as requiring capture at runtime.
    pub fn register_var_usage(&mut self, var_usage: &Span<String>) -> CheckerResult {
        let var = self
            .scopes
            .iter()
            .rev()
            .find_map(|scope| scope.vars.get(&var_usage.data))
            .ok_or_else(|| CheckerError::new(var_usage.at, "variable was not found"))?;

        if let RuntimeCodeRange::Parsed(_) = var.decl_at {
            self.register_single_usage(var.scope_id, &var_usage.data, DependencyType::Variable)?;
        }

        Ok(())
    }

    /// Register usage of a function
    /// This function is responsible to check if the function exists and is used correctly,
    /// and if it must be collected.
    ///
    /// The process of 'collection' is to mark an item as requiring capture at runtime.
    pub fn register_fn_usage(&mut self, fn_usage: &Span<String>) -> CheckerResult {
        let func = self
            .scopes
            .iter()
            .rev()
            .find_map(|scope| scope.fns.get(&fn_usage.data))
            .ok_or_else(|| CheckerError::new(fn_usage.at, "function was not found"))?;

        if let RuntimeCodeRange::Parsed(_) = func.decl_at {
            self.register_single_usage(func.scope_id, &fn_usage.data, DependencyType::Function)?;
        }

        Ok(())
    }

    /// Register usage of a method
    /// This function is responsible to check if the method exists and is used correctly,
    /// and if it must be collected.
    ///
    /// The process of 'collection' is to mark an item as requiring capture at runtime.
    pub fn register_method_usage(&mut self, method_usage: &Span<String>) -> CheckerResult {
        let methods = self
            .scopes
            .iter()
            .rev()
            .flat_map(|scope| scope.methods.get(&method_usage.data))
            .flat_map(|method| method.iter().map(|(_, method)| method))
            .copied()
            .collect::<Vec<_>>();

        if methods.is_empty() {
            return Err(CheckerError::new(method_usage.at, "method was not found"));
        }

        for method in methods {
            match method.decl_at {
                RuntimeCodeRange::Internal(_) => {}
                RuntimeCodeRange::Parsed(_) => {
                    self.register_single_usage(
                        method.scope_id,
                        &method_usage.data,
                        DependencyType::Method,
                    )?;
                }
            }
        }

        Ok(())
    }

    /// Register usage of a command alias
    /// This function is responsible to check if the command alias exists and is used correctly,
    /// and if it must be collected.
    ///
    /// The process of 'collection' is to mark an item as requiring capture at runtime.
    pub fn register_cmd_alias_usage(&mut self, cmd_alias_usage: &Span<String>) -> CheckerResult {
        let cmd_alias = self
            .scopes
            .iter()
            .rev()
            .find_map(|scope| scope.cmd_aliases.get(&cmd_alias_usage.data).copied())
            .ok_or_else(|| CheckerError::new(cmd_alias_usage.at, "variable was not found"))?;

        if !cmd_alias.is_ready {
            return Err(CheckerError::new(
                cmd_alias_usage.at,
                "cannot use a command alias before its assignment",
            ));
        }

        self.register_single_usage(
            cmd_alias.scope_id,
            &cmd_alias_usage.data,
            DependencyType::CmdAlias,
        )
    }

    fn register_single_usage(
        &mut self,
        item_declared_in: AstScopeId,
        name: &str,
        dep_type: DependencyType,
    ) -> CheckerResult {
        // Don't capture if the value if the item was declared internally (e.g. native library)
        if item_declared_in == NATIVE_LIB_AST_SCOPE_ID {
            return Ok(());
        }

        // Don't collect anything if we're not inside a deps-collecting scope
        let Some(deps_scope) = self.current_deps_collecting_scope() else {
            return Ok(());
        };

        // Determine if the variable was declared in the current deps-collecting scope (or in a descendent scope)
        let deps_scope_index = self
            .scopes
            .iter()
            .position(|scope| scope.id == deps_scope.id)
            .unwrap();

        // If so, don't capture it
        if self
            .scopes
            .iter()
            .skip(deps_scope_index)
            .any(|scope| scope.id == item_declared_in)
        {
            return Ok(());
        }

        // Otherwise, mark the item as a dependency (= will require a capture at runtime)
        // Registered in the deps-collecting scope + all its deps-collecting parent (up to the declaration scope)
        let deps_scope_ids = self
            .scopes
            .iter()
            .take(deps_scope_index + 1)
            .skip_while(|scope| scope.id != item_declared_in)
            .skip(1)
            .filter(|scope| {
                scope
                    .special_scope_type
                    .is_some_and(|typ| typ.requires_deps_capture())
            })
            .map(|scope| scope.id)
            .collect::<Vec<_>>();

        let mut prev_parent = item_declared_in;

        for deps_scope_id in deps_scope_ids {
            self.collected
                .deps
                .get_mut(&deps_scope_id)
                .unwrap()
                .insert(Dependency {
                    name: name.to_owned(),
                    declared_in: prev_parent,
                    dep_type,
                });

            prev_parent = deps_scope_id;
        }

        Ok(())
    }

    /// Register a type alias declaration
    pub fn register_type_alias(
        &mut self,
        name: &Span<String>,
        content: &Span<ValueType>,
        inside_block: &Block,
    ) {
        let content = shared(content.clone());

        self.collected
            .type_aliases_decl
            .insert(name.at, SharingType::clone(&content));

        self.collected
            .type_aliases_decl_by_scope
            .entry(inside_block.scope_id)
            .or_default()
            .insert(name.data.clone(), content);
    }

    /// Register usage of a type alias
    /// This function will check if the type alias exists and register the usage globally
    pub fn register_type_alias_usage(&mut self, name: &Span<String>) -> CheckerResult {
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
            .insert(name.clone(), SharingType::clone(type_alias));

        Ok(())
    }

    /// Register a command alias
    pub fn register_cmd_alias(&mut self, alias_content: Span<SingleCmdCall>) {
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
    pub fn mark_cmd_alias_as_ready(&mut self, name: &Span<String>) {
        let cmd_alias = self
            .scopes
            .iter_mut()
            .rev()
            .find_map(|scope| scope.cmd_aliases.get_mut(&name.data))
            .expect("internal error: command alias to mark as ready was not found");

        cmd_alias.is_ready = true;
    }

    /// Register a function's signature
    pub fn register_function_signature(&mut self, signature: Span<FnSignature>) {
        let dup = self
            .collected
            .fn_signatures
            .insert(signature.at, shared(signature));

        assert!(dup.is_none());
    }

    /// Register a function's body
    pub fn register_function_body(&mut self, body: Span<Block>) {
        let dup = self.collected.fn_bodies.insert(body.at, shared(body));

        assert!(dup.is_none());
    }

    /// Register a developed single command call
    pub fn register_developed_single_cmd_call(
        &mut self,
        from: &Span<SingleCmdCall>,
        collected: DevelopedSingleCmdCall,
    ) {
        assert!(from.at == collected.at);

        let dup = self.collected.cmd_calls.insert(from.at, shared(collected));
        assert!(dup.is_none());
    }

    /// Register a command call used as a value
    pub fn register_cmd_call_value(&mut self, from: &Span<CmdCall>) {
        let dup = self
            .collected
            .cmd_call_values
            .insert(from.at, shared(from.clone()));

        assert!(dup.is_none());
    }
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

    /// List of methods declared in this scope, associated to their appliable types
    pub methods: HashMap<String, Vec<(SharingType<ValueType>, DeclaredMethod)>>,

    /// List of command aliases declared in this scope
    pub cmd_aliases: HashMap<String, DeclaredCmdAlias>,

    /// List of type aliases declared in this scope
    pub type_aliases: HashMap<String, CodeRange>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum SpecialScopeType {
    /// Inside the body of a function
    /// Will provoke dependencies collection inside it
    Function,

    /// Inside the content of a command alias
    /// Will provoke dependencies collection inside it
    CmdAlias,

    /// Inside the body of a loop
    /// Will allow using 'break' and 'continue' inside it
    Loop,
}

impl SpecialScopeType {
    pub fn requires_deps_capture(&self) -> bool {
        match self {
            SpecialScopeType::Function | SpecialScopeType::CmdAlias => true,
            SpecialScopeType::Loop => false,
        }
    }
}

/// Variable declaration
#[derive(Clone, Copy)]
pub struct DeclaredVar {
    /// Name location
    pub decl_at: RuntimeCodeRange,

    /// Scope the variable is defined in
    pub scope_id: AstScopeId,

    /// Is the variable mutable?
    pub is_mut: bool,
}

/// Function declaration
#[derive(Clone, Copy)]
pub struct DeclaredFn {
    /// Name location
    pub decl_at: RuntimeCodeRange,

    /// Scope the function is defined in
    pub scope_id: AstScopeId,
}

/// Method declaration
#[derive(Clone, Copy)]
pub struct DeclaredMethod {
    /// Name location
    pub decl_at: RuntimeCodeRange,

    /// Scope the method is defined in
    pub scope_id: AstScopeId,
}

/// Command alias declaration
#[derive(Clone, Copy)]
pub struct DeclaredCmdAlias {
    /// Name location
    pub decl_at: CodeRange,

    /// Scope the comment alias is defined in
    pub scope_id: AstScopeId,

    /// Location of the command alias' content in its declaration
    pub content_at: CodeRange,

    /// Is the declared alias ready?
    /// See [`Context::mark_cmd_alias_as_ready`]
    pub is_ready: bool,
}

// TODO: internal functions, variables and methods are NOT registered as dependencies
//       (as we don't have a way to tell apart methods with the same name if they don't have a different decl_at for instance)
// This is a problem if a non-builtin scope contains e.g. native functions
