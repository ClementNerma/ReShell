use std::collections::HashMap;

use indexmap::{IndexMap, IndexSet};
use parsy::{CodeRange, Eaten, FileId, Location};
use reshell_parser::ast::ValueType;

use crate::{errors::CheckerResult, CheckerError};

pub struct State {
    scopes: Vec<CheckerScope>,
    pub collected: CheckerOutput,
}

#[derive(Debug)]
pub struct CheckerOutput {
    pub deps: IndexMap<CodeRange, IndexSet<Dependency>>,
    pub type_aliases: IndexMap<CodeRange, Eaten<ValueType>>,
    pub type_aliases_usages: IndexMap<Eaten<String>, CodeRange>,
    pub type_aliases_decl: IndexMap<CodeRange, HashMap<String, CodeRange>>,
}

impl State {
    pub fn new() -> Self {
        Self {
            scopes: vec![],
            collected: CheckerOutput {
                deps: IndexMap::new(),
                type_aliases: IndexMap::new(),
                type_aliases_usages: IndexMap::new(),
                type_aliases_decl: IndexMap::new(),
            },
        }
    }

    pub fn push_scope(&mut self, scope: CheckerScope) {
        assert!(scope.deps || !matches!(scope.typ, Some(ScopeType::Function { args_at: _ })));

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

    pub fn curr_scope_type(&self) -> Option<ScopeType> {
        self.scopes.iter().rev().find_map(|scope| scope.typ)
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
    ) -> CheckerResult<FetchedDependency> {
        let dep = self
            .scopes
            .iter()
            .rev()
            .find_map(|scope| match dep_type {
                DependencyType::Variable => scope
                    .vars
                    .get(&item.data)
                    .map(|var| FetchedDependency::new(var.name_at, true)),
                DependencyType::Function => scope
                    .fns
                    .get(&item.data)
                    .map(|func_at| FetchedDependency::new(*func_at, false)),
                DependencyType::CmdAlias => scope
                    .cmd_aliases
                    .get(&item.data)
                    .map(|alias_at| FetchedDependency::new(*alias_at, false)),
            })
            .ok_or_else(|| CheckerError::new(item.at, format!("{dep_type} was not found")))?;

        let FetchedDependency {
            declared_at,
            is_mut: _,
        } = dep;

        if matches!(declared_at.start.file_id, FileId::Internal) {
            return Ok(dep);
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
                return Ok(dep);
            }

            if let Some(ScopeType::Function { args_at }) = deps_scope.typ {
                let var_declared_in_fn_args = args_at
                .contains(declared_at)
                .map_err(|err| {
                    CheckerError::new(
                        CodeRange::new(Location { file_id: item.at.start.file_id, offset: 0 }, 0),
                        format!("only source-backed files can be used (detected during var usage analysis): {err:?}"),
                    )
                })?;

                if var_declared_in_fn_args {
                    return Ok(dep);
                }
            }

            let code_range = deps_scope.code_range;

            self.collected
                .deps
                .get_mut(&code_range)
                .unwrap()
                .insert(Dependency {
                    name: item.data.clone(),
                    declared_at,
                    dep_type,
                });
        }

        Ok(dep)
    }

    pub fn register_type_alias_usage(&mut self, name: &Eaten<String>) -> CheckerResult {
        let decl_scope = self
            .scopes
            .iter()
            .rev()
            .find(|scope| scope.type_aliases.contains_key(&name.data))
            .ok_or_else(|| {
                CheckerError::new(name.at, format!("type alias {} was not found", name.data))
            })?;

        let type_alias = self
            .collected
            .type_aliases_decl
            .get(&decl_scope.code_range)
            .unwrap()
            .get(&name.data)
            .unwrap();

        self.collected
            .type_aliases_usages
            .insert(name.clone(), *type_alias);

        Ok(())
    }
}

#[derive(Clone)]
pub struct CheckerScope {
    pub deps: bool,
    pub code_range: CodeRange,
    pub typ: Option<ScopeType>,
    pub vars: HashMap<String, DeclaredVar>,
    pub fns: HashMap<String, CodeRange>,
    pub cmd_aliases: HashMap<String, CodeRange>,
    pub type_aliases: HashMap<String, CodeRange>,
}

#[derive(Clone, Copy)]
pub enum ScopeType {
    Function { args_at: CodeRange },
    Loop,
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

pub struct FetchedDependency {
    pub declared_at: CodeRange,
    pub is_mut: bool,
}

impl FetchedDependency {
    fn new(declared_at: CodeRange, is_mut: bool) -> Self {
        Self {
            declared_at,
            is_mut,
        }
    }
}
