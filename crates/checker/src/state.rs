use std::collections::HashSet;

use parsy::{CodeRange, Eaten, Location};
use reshell_parser::ast::{ FnSignature, RuntimeCodeRange, ValueType, SingleCmdCall, FunctionBody, CmdCall, Block};

use crate::{errors::CheckerResult, CheckerError, output::*};

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

    /// Prepare deps for a block
    pub fn prepare_deps(&mut self, at: CodeRange) {
        let dup = self.collected.deps.insert(at, HashSet::new());
        assert!(dup.is_none());
    }

    /// Retrieve a registered developed command call
    pub fn get_developed_cmd_call_at(&self, at: CodeRange) -> Option<SharingType<DevelopedSingleCmdCall>> {
        self.collected.cmd_calls.get(&at).map(SharingType::clone)
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

    /// Register a type alias declaration
    pub fn register_type_alias(&mut self, name: &Eaten<String>, content: &Eaten<ValueType>, inside_block: &Eaten<Block>) {
        self.collected.type_aliases_decl.insert(name.at, content.clone());

        self.collected.type_aliases_decl_by_scope
            .entry(inside_block.at)
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

    /// Register a command call used as a value
    pub fn register_cmd_call_value(&mut self, from: &Eaten<CmdCall>) {
        let dup = self.collected.cmd_call_values.insert(from.at, shared(from.clone()));
        assert!(dup.is_none());
    }
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
