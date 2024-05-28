use std::{collections::HashMap, path::PathBuf};

use parsy::{CodeRange, Eaten};

use crate::{
    errors::{ExecError, ExecErrorContent, ExecResult},
    files_map::{FilesMap, ScopableFilePath, SourceFile},
    native_lib::generate_native_lib,
    values::{LocatedValue, RuntimeFnValue, RuntimeValue},
};

#[derive(Debug)]
pub struct Context {
    scopes: Vec<Scope>,
    files_map: FilesMap,
    in_fork: bool,
}

impl Context {
    pub fn new() -> Self {
        Self {
            scopes: vec![generate_native_lib()],
            files_map: FilesMap::new(),
            in_fork: false,
        }
    }

    pub fn error<S: Into<ExecErrorContent>>(&self, at: CodeRange, content: S) -> ExecError {
        ExecError {
            at,
            file: self.current_file().clone(),
            content: content.into(),
            in_fork: self.in_fork,
        }
    }

    pub fn scopes(&self) -> &[Scope] {
        &self.scopes
    }

    pub fn files_map(&self) -> &FilesMap {
        &self.files_map
    }

    pub fn create_scope(&self) -> Scope {
        Scope {
            vars: HashMap::new(),
            fns: HashMap::new(),
            in_file_id: self.scopes.last().unwrap().in_file_id,
        }
    }

    pub fn push_scope(&mut self, scope: Scope) {
        assert!(
            self.files_map.has_file(scope.in_file_id),
            "Provided scope is associated to an unregistered file"
        );

        self.scopes.push(scope);
    }

    pub fn push_file_scope(&mut self, path: ScopableFilePath, content: String) -> u64 {
        let scope = self.create_file_scope(path, content);
        let id = scope.in_file_id;

        self.scopes.push(scope);

        id
    }

    #[must_use]
    pub fn create_file_scope(&mut self, path: ScopableFilePath, content: String) -> Scope {
        Scope {
            vars: HashMap::new(),
            fns: HashMap::new(),
            in_file_id: self.files_map.register_file(path, content),
        }
    }

    // TODO: check if this is *really* safe
    pub fn update_current_scope_file(&mut self, path: ScopableFilePath, content: String) {
        self.current_scope_mut().in_file_id = self.files_map.register_file(path, content);
    }

    pub fn mark_as_forked(&mut self) {
        self.in_fork = true;

        self.scopes
            .iter_mut()
            .rev()
            .flat_map(|scope| scope.vars.iter_mut())
            .for_each(|(_, var)| var.forked = true);
    }

    pub fn current_scope(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    pub fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    pub fn pop_scope(&mut self) -> Scope {
        assert!(self.scopes.len() > 1);
        self.scopes.pop().unwrap()
    }

    pub fn current_file_id(&self) -> u64 {
        self.current_scope().in_file_id
    }

    pub fn current_file(&self) -> &SourceFile {
        self.files_map.get_file(self.current_file_id()).unwrap()
    }

    pub fn current_file_path(&self) -> Option<&PathBuf> {
        self.files_map.get_file_path(self.current_file_id())
    }

    pub fn all_vars(&self) -> impl Iterator<Item = (&String, &ScopeVar)> {
        self.scopes.iter().rev().flat_map(|scope| scope.vars.iter())
    }

    pub fn all_fns(&self) -> impl Iterator<Item = (&String, &ScopeFn)> {
        self.scopes.iter().rev().flat_map(|scope| scope.fns.iter())
    }

    pub fn get_fn<'s>(&'s self, name: &Eaten<String>) -> Option<&'s ScopeFn> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.fns.get(&name.data))
    }

    pub fn get_fn_value<'s>(&'s self, name: &Eaten<String>) -> ExecResult<&'s RuntimeFnValue> {
        let Some(func) = self.get_fn(name) else {
            return Err(self.error(name.at, "function not found"));
        };

        Ok(&func.value)
    }

    pub fn get_var<'s>(&'s self, name: &Eaten<String>) -> Option<&'s ScopeVar> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.vars.get(&name.data))
    }

    pub fn get_var_mut<'s>(&'s mut self, name: &Eaten<String>) -> Option<&'s mut ScopeVar> {
        self.scopes
            .iter_mut()
            .rev()
            .find_map(|scope| scope.vars.get_mut(&name.data))
    }

    pub fn get_var_value(&self, name: &Eaten<String>) -> ExecResult<&RuntimeValue> {
        let var = self
            .get_var(name)
            .ok_or_else(|| self.error(name.at, "variable not found"))?;

        let located_val = var
            .value
            .as_ref()
            .ok_or_else(|| self.error(name.at, "variable does not have a value yet"))?;

        Ok(&located_val.value)
    }
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub vars: HashMap<String, ScopeVar>,
    pub fns: HashMap<String, ScopeFn>,
    pub in_file_id: u64,
}

impl Scope {
    pub fn new(in_file_id: u64) -> Self {
        Self {
            vars: HashMap::new(),
            fns: HashMap::new(),
            in_file_id,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ScopeVar {
    pub declared_at: CodeRange,
    pub is_mut: bool,
    pub value: Option<LocatedValue>,
    pub forked: bool,
}

#[derive(Debug, Clone)]
pub struct ScopeFn {
    pub declared_at: CodeRange,
    pub value: RuntimeFnValue,
}