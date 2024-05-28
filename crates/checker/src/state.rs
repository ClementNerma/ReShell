use std::collections::{HashMap, HashSet};

use parsy::{CodeRange, Eaten, Location};
use reshell_parser::ast::{Block, FnSignature, RuntimeCodeRange, ValueType, SingleCmdCall};

use crate::{errors::CheckerResult, CheckerError};

pub struct State {
    scopes: Vec<CheckerScope>,
    pub collected: CheckerOutput,
}

#[derive(Debug)]
pub struct CheckerOutput {
    pub deps: HashMap<CodeRange, HashSet<Dependency>>,
    pub type_aliases: HashMap<CodeRange, Eaten<ValueType>>,
    pub type_aliases_usages: HashMap<Eaten<String>, CodeRange>,
    pub type_aliases_decl: HashMap<CodeRange, HashMap<String, CodeRange>>,
    pub fn_signatures: HashMap<CodeRange, Eaten<FnSignature>>,
    pub fn_bodies: HashMap<CodeRange, Eaten<Block>>,
    pub cmd_aliases: HashMap<CodeRange, Eaten<SingleCmdCall>>
}

impl State {
    pub fn new() -> Self {
        Self {
            scopes: vec![],
            collected: CheckerOutput {
                deps: HashMap::new(),
                type_aliases: HashMap::new(),
                type_aliases_usages: HashMap::new(),
                type_aliases_decl: HashMap::new(),
                fn_signatures: HashMap::new(),
                fn_bodies: HashMap::new(),
                cmd_aliases: HashMap::new()
            },
        }
    }

    pub fn push_scope(&mut self, scope: CheckerScope) {
        assert!(scope.deps || !matches!(scope.scope_type, Some(ScopeType::Function { args_at: _ })));

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
        self.scopes.iter().rev().find_map(|scope| scope.scope_type)
    }

    fn current_deps_scope(&self) -> Option<&CheckerScope> {
        self.scopes.iter().rev().find(|scope| scope.deps)
    }

    pub fn scopes(&self) -> impl Iterator<Item = &CheckerScope> {
        self.scopes.iter().rev()
    }

    pub fn mark_cmd_alias_ready(&mut self, name: &Eaten<String>) {
        let cmd_alias = self.scopes
            .iter_mut()
            .rev()
            .find_map(|scope| {
                scope.cmd_aliases.get_mut(&name.data)
            }).expect("internal error: command alias to mark as ready was not found");

        // assert!(cmd_alias.name_at == name.at);

        cmd_alias.is_ready = true;
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
                    .map(|var| Ok(FetchedDependency::new(var.name_at, var.is_mut))),

                    DependencyType::Function => scope
                    .fns
                    .get(&item.data)
                    .map(|func| Ok(FetchedDependency::new(func.name_at, false))),

                    DependencyType::CmdAlias => scope
                    .cmd_aliases
                    .get(&item.data)
                    .map(|DeclaredCmdAlias { name_at, is_ready }| if *is_ready {
                        Ok(FetchedDependency::new(RuntimeCodeRange::Parsed(*name_at), false))
                    } else {
                        Err(CheckerError::new(item.at, "cannot use a command alias before its assignment"))
                    }),
            })
            .ok_or_else(|| CheckerError::new(item.at, format!("{dep_type} was not found")))??;

        let FetchedDependency {
            declared_at,
            is_mut: _,
        } = dep;

        if matches!(declared_at, RuntimeCodeRange::Internal) {
            return Ok(dep);
        }

        let Some(deps_scope) = self.current_deps_scope() else {
            return Ok(dep);
        };
    
        let Some(declared_at) = declared_at.real() else {
            return Ok(dep);
        };

        let var_declared_in_deps_scope = 
                deps_scope.code_range.real().unwrap().contains(declared_at).map_err(|err| {
                CheckerError::new(
                    CodeRange::new(Location { file_id: item.at.start.file_id, offset: 0 }, 0),
                    format!("only source-backed files can be used (detected during var usage analysis): {err:?}"),
                )
            })?;

        if var_declared_in_deps_scope {
            return Ok(dep);
        }

        if let Some(ScopeType::Function { args_at }) = deps_scope.scope_type {
            let var_declared_in_fn_args = 
                args_at.contains(declared_at).map_err(|err| {
                CheckerError::new(
                    CodeRange::new(Location { file_id: item.at.start.file_id, offset: 0 }, 0),
                    format!("only source-backed files can be used (detected during var usage analysis): {err:?}"),
                )
            })?;

            if var_declared_in_fn_args {
                return Ok(dep);
            }
        }

        let code_range = deps_scope.code_range.real().unwrap();

        self.collected
            .deps
            .get_mut(&code_range)
            .unwrap()
            .insert(Dependency {
                name: item.data.clone(),
                declared_at,
                dep_type,
            });

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

        let Some(decl_scope_at) = decl_scope.code_range.real() else {
            return Ok(())
        };

        let type_alias = self
            .collected
            .type_aliases_decl
            .get(&decl_scope_at)
            .unwrap()
            .get(&name.data)
            .unwrap();

        self.collected
            .type_aliases_usages
            .insert(name.clone(), *type_alias);

        Ok(())
    }

    pub fn register_cmd_alias(&mut self, alias_content: Eaten<SingleCmdCall>) {
        let dup = self.collected.cmd_aliases.insert(alias_content.at, alias_content);

        assert!(dup.is_none());
    }

    pub fn register_function_body(&mut self, body: Eaten<Block>) {
        let dup = self
            .collected
            .fn_bodies
            .insert(body.at, body);

        assert!(dup.is_none());
    }

    pub fn register_function_signature(&mut self, signature: Eaten<FnSignature>) {
        let dup = self.collected.fn_signatures.insert(signature.at, signature);
        assert!(dup.is_none());
    }
}

#[derive(Clone)]
pub struct CheckerScope {
    pub deps: bool,
    pub code_range: RuntimeCodeRange,
    pub scope_type: Option<ScopeType>,
    pub vars: HashMap<String, DeclaredVar>,
    pub fns: HashMap<String, DeclaredFn>,
    pub cmd_aliases: HashMap<String, DeclaredCmdAlias>,
    pub type_aliases: HashMap<String, CodeRange>,
}

#[derive(Clone, Copy)]
pub enum ScopeType {
    Normal,
    Function { args_at: CodeRange },
    Loop,
}

#[derive(Clone, Copy)]
pub struct DeclaredVar {
    pub name_at: RuntimeCodeRange,
    pub is_mut: bool,
}

#[derive(Clone, Copy)]
pub struct DeclaredFn {
    pub name_at: RuntimeCodeRange,
}

impl DeclaredFn {
    pub fn new(name_at: RuntimeCodeRange) -> Self {
        Self { name_at }
    }
}

#[derive(Clone, Copy)]
pub struct DeclaredCmdAlias {
    pub name_at: CodeRange,
    pub(crate) is_ready: bool
}

impl DeclaredCmdAlias {
    pub fn ready(name_at: CodeRange) -> Self {
        Self {name_at, is_ready: true}
    }
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
    pub declared_at: RuntimeCodeRange,
    pub is_mut: bool,
}

impl FetchedDependency {
    fn new(declared_at: RuntimeCodeRange, is_mut: bool) -> Self {
        Self {
            declared_at,
            is_mut,
        }
    }
}
