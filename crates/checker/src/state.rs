use std::collections::HashMap;

use indexmap::{IndexMap, IndexSet};
use parsy::{CodeRange, Eaten, FileId, Location};

use crate::{errors::CheckerResult, CheckerError};

pub struct State {
    scopes: Vec<CheckerScope>,

    pub deps: IndexMap<CodeRange, IndexSet<Dependency>>,
    pub type_alias_deps: IndexMap<CodeRange, IndexSet<Dependency>>,
    pub cmd_alias_deps: IndexMap<CodeRange, IndexSet<Dependency>>,
}

impl State {
    pub fn new() -> Self {
        Self {
            scopes: vec![],
            deps: IndexMap::new(),
            type_alias_deps: IndexMap::new(),
            cmd_alias_deps: IndexMap::new(),
        }
    }

    pub fn push_scope(&mut self, scope: CheckerScope) {
        assert!(scope.fn_args_at.is_none() || scope.deps);

        self.scopes.push(scope);
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop().unwrap();
    }

    pub fn curr_scope(&self) -> &CheckerScope {
        self.scopes.last().unwrap()
    }

    pub fn curr_scope_mut(&mut self) -> &mut CheckerScope {
        self.scopes.last_mut().unwrap()
    }

    fn current_deps_scope(&self) -> Option<&CheckerScope> {
        self.scopes.iter().rev().find(|scope| scope.deps)
    }

    pub fn is_fn(&self, name: &str) -> bool {
        self.scopes
            .iter()
            .rev()
            .any(|scope| scope.fns.get(name).is_some())
    }

    pub fn is_cmd_alias(&self, name: &str) -> bool {
        self.scopes
            .iter()
            .rev()
            .any(|scope| scope.cmd_aliases.get(name).is_some())
    }

    pub fn register_usage(
        &mut self,
        item: &Eaten<String>,
        dep_type: DependencyType,
    ) -> CheckerResult {
        self.register_usage_and_get(item, dep_type).map(|_| ())
    }

    pub fn register_usage_and_get(
        &mut self,
        item: &Eaten<String>,
        dep_type: DependencyType,
    ) -> CheckerResult {
        let declared_at = self
            .scopes
            .iter()
            .rev()
            .find_map(|scope| match dep_type {
                DependencyType::Variable => scope.vars.get(&item.data).map(|var| var.name_at),
                DependencyType::Function => scope.fns.get(&item.data).copied(),
                DependencyType::CmdAlias => scope.cmd_aliases.get(&item.data).copied(),
                DependencyType::TypeAlias => scope.type_aliases.get(&item.data).copied(),
            })
            .ok_or_else(|| CheckerError::new(item.at, format!("{dep_type} was not found")))?;

        if matches!(declared_at.start.file_id, FileId::Internal) {
            return Ok(());
        }

        if let Some(deps_scope) = self.current_deps_scope() {
            let var_declared_in_deps_scope = deps_scope
                .code_range
                .contains(declared_at)
                .map_err(|err| {
                    CheckerError::new(
                        CodeRange::new(Location { file_id: item.at.start.file_id, offset: 0 }, 0),
                        format!("only source-backed files can be used (detected during var usage analysis): {err:?}"),
                    )
                })?;

            if var_declared_in_deps_scope {
                return Ok(());
            }

            if let Some(fn_args_at) = deps_scope.fn_args_at {
                let var_declared_in_fn_args = fn_args_at
                .contains(declared_at)
                .map_err(|err| {
                    CheckerError::new(
                        CodeRange::new(Location { file_id: item.at.start.file_id, offset: 0 }, 0),
                        format!("only source-backed files can be used (detected during var usage analysis): {err:?}"),
                    )
                })?;

                if var_declared_in_fn_args {
                    return Ok(());
                }
            }

            let code_range = deps_scope.code_range;

            self.deps.get_mut(&code_range).unwrap().insert(Dependency {
                name: item.data.clone(),
                declared_at,
                dep_type,
            });
        }

        Ok(())
    }
}

#[derive(Clone)]
pub struct CheckerScope {
    pub deps: bool,
    pub code_range: CodeRange,         /* for fns, body range */
    pub fn_args_at: Option<CodeRange>, /* fns only */
    pub vars: HashMap<String, DeclaredVar>,
    pub fns: HashMap<String, CodeRange /* fn name at */>,
    pub cmd_aliases: HashMap<String, CodeRange>,
    pub type_aliases: HashMap<String, CodeRange>,
}

#[derive(Clone, Copy)]
pub struct DeclaredVar {
    pub name_at: CodeRange,
    pub is_mut: bool,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Dependency {
    pub name: String,
    pub declared_at: CodeRange,
    pub dep_type: DependencyType,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum DependencyType {
    Variable,
    Function,
    CmdAlias,
    TypeAlias,
}

impl std::fmt::Display for DependencyType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DependencyType::Variable => write!(f, "variable"),
            DependencyType::Function => write!(f, "function"),
            DependencyType::CmdAlias => write!(f, "command alias"),
            DependencyType::TypeAlias => write!(f, "type alias"),
        }
    }
}
