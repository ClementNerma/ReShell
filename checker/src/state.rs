use std::collections::{HashMap, HashSet};

use indexmap::{IndexMap, IndexSet};
use parsy::{CodeRange, Eaten, FileId, Location};

use crate::{errors::CheckerResult, CheckerError};

pub struct State {
    scopes: Vec<Scope>,
    pub fn_deps: IndexMap<CodeRange, IndexSet<Dependency>>,
}

impl State {
    pub fn new() -> Self {
        Self {
            scopes: vec![],
            fn_deps: IndexMap::new(),
        }
    }

    pub fn push_scope(&mut self, scope: Scope) {
        if scope.fn_args_at.is_some() {
            let dup = self.fn_deps.insert(scope.code_range, IndexSet::new());

            assert!(dup.is_none());
        }

        self.scopes.push(scope);
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop().unwrap();
    }

    pub fn curr_scope(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    pub fn curr_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn current_function_scope(&self) -> Option<(&Scope, CodeRange)> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.fn_args_at.map(|fn_args_at| (scope, fn_args_at)))
    }

    pub fn register_var_usage(&mut self, var: &Eaten<String>) -> CheckerResult {
        self.register_var_usage_and_get(var).map(|_| ())
    }

    pub fn register_var_usage_and_get(
        &mut self,
        var: &Eaten<String>,
    ) -> CheckerResult<DeclaredVar> {
        let declared_var = self
            .scopes
            .iter()
            .rev()
            .find_map(|scope| scope.vars.get(&var.data))
            .copied()
            .ok_or_else(|| CheckerError::new(var.at, "variable was not found"))?;

        if matches!(declared_var.name_at.start.file_id, FileId::Internal) {
            return Ok(declared_var);
        }

        if let Some((fn_scope, fn_args_at)) = self.current_function_scope() {
            let var_declared_in_fn_scope = fn_scope
                .code_range
                .contains(declared_var.name_at)
                .map_err(|err| {
                    CheckerError::new(
                        CodeRange::new(Location { file_id: var.at.start.file_id, offset: 0 }, 0),
                        format!("only source-backed files can be used (detected during var usage analysis): {err:?}"),
                    )
                })?;

            if var_declared_in_fn_scope {
                return Ok(declared_var);
            }

            let var_declared_in_fn_args = fn_args_at
                .contains(declared_var.name_at)
                .map_err(|err| {
                    CheckerError::new(
                        CodeRange::new(Location { file_id: var.at.start.file_id, offset: 0 }, 0),
                        format!("only source-backed files can be used (detected during var usage analysis): {err:?}"),
                    )
                })?;

            if var_declared_in_fn_args {
                return Ok(declared_var);
            }

            let code_range = fn_scope.code_range;

            self.fn_deps
                .get_mut(&code_range)
                .unwrap()
                .insert(Dependency {
                    name: var.data.clone(),
                    name_declared_at: declared_var.name_at,
                    dep_type: DependencyType::Variable,
                });
        }

        Ok(declared_var)
    }

    pub fn register_fn_usage(&mut self, func: &Eaten<String>) -> CheckerResult {
        let declared_fn_at = self
            .scopes
            .iter()
            .rev()
            .find_map(|scope| scope.fns.get(&func.data))
            .copied()
            .ok_or_else(|| CheckerError::new(func.at, "function was not found"))?;

        if matches!(declared_fn_at.start.file_id, FileId::Internal) {
            return Ok(());
        }

        if let Some((fn_scope, fn_args_at)) = self.current_function_scope() {
            let fn_declared_in_fn_scope = fn_scope
                .code_range
                .contains(declared_fn_at)
                .map_err(|err| {
                    CheckerError::new(
                        CodeRange::new(Location { file_id: declared_fn_at.start.file_id, offset: 0 }, 0),
                        format!("only source-backed files can be used (detected during var usage analysis): {err:?}"),
                    )
                })?;

            if fn_declared_in_fn_scope {
                return Ok(());
            }

            let fn_declared_in_fn_args = fn_args_at
                .contains(declared_fn_at)
                .map_err(|err| {
                    CheckerError::new(
                        CodeRange::new(Location { file_id: declared_fn_at.start.file_id, offset: 0 }, 0),
                        format!("only source-backed files can be used (detected during var usage analysis): {err:?}"),
                    )
                })?;

            if fn_declared_in_fn_args {
                return Ok(());
            }

            let code_range = fn_scope.code_range;

            self.fn_deps
                .get_mut(&code_range)
                .unwrap()
                .insert(Dependency {
                    name: func.data.clone(),
                    name_declared_at: declared_fn_at,
                    dep_type: DependencyType::Function,
                });
        }

        Ok(())
    }
}

#[derive(Clone)]
pub struct Scope {
    pub code_range: CodeRange,         /* for fns, body range */
    pub fn_args_at: Option<CodeRange>, /* fns only */
    pub vars: HashMap<String, DeclaredVar>,
    pub fns: HashMap<String, CodeRange /* fn name at */>,
    pub types: HashSet<String>,
    pub cmd_aliases: HashSet<String>,
}

#[derive(Clone, Copy)]
pub struct DeclaredVar {
    pub name_at: CodeRange,
    pub is_mut: bool,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Dependency {
    pub name: String,
    pub name_declared_at: CodeRange,
    pub dep_type: DependencyType,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum DependencyType {
    Variable,
    Function,
}
