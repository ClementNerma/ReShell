use std::{collections::HashMap, path::PathBuf};

use parsy::{CodeRange, Eaten, FileId};
use reshell_parser::ast::{SingleCmdCall, ValueType};

use crate::{
    errors::{ExecError, ExecErrorContent, ExecResult, StackTrace, StackTraceEntry},
    files_map::{FilesMap, ScopableFilePath, SourceFile},
    native_lib::generate_native_lib,
    values::{LocatedValue, RuntimeFnValue, RuntimeValue},
};

#[derive(Debug)]
pub struct Context {
    scopes: Vec<Scope>,
    files_map: FilesMap,
    in_fork: bool,
    home_dir: Option<PathBuf>,
}

impl Context {
    pub fn new(home_dir: Option<PathBuf>) -> Self {
        Self {
            scopes: vec![generate_native_lib()],
            files_map: FilesMap::new(),
            in_fork: false,
            home_dir,
        }
    }

    pub fn set_home_dir(&mut self, home_dir: PathBuf) {
        self.home_dir = Some(home_dir);
    }

    pub fn error(&self, at: CodeRange, content: impl Into<ExecErrorContent>) -> ExecError {
        ExecError {
            at,
            in_file: self.current_file_id(),
            source_file: self.current_file().cloned(),
            content: content.into(),
            in_fork: self.in_fork,
            stack_trace: self.stack_trace(),
        }
    }

    pub fn scopes(&self) -> &[Scope] {
        &self.scopes
    }

    pub fn files_map(&self) -> &FilesMap {
        &self.files_map
    }

    pub fn home_dir(&self) -> Option<&PathBuf> {
        self.home_dir.as_ref()
    }

    pub fn stack_trace(&self) -> StackTrace {
        let history = self
            .scopes
            .iter()
            .filter_map(|scope| scope.history_entry.clone())
            .collect();

        StackTrace { history }
    }

    pub fn push_scope(&mut self, scope: Scope) {
        if let Some(file_id) = scope.in_real_file_id() {
            assert!(
                self.files_map.has_file(file_id),
                "Provided scope is associated to an unregistered file"
            );
        }

        self.scopes.push(scope);
    }

    pub fn mark_as_forked(&mut self) {
        self.in_fork = true;

        self.scopes
            .iter_mut()
            .rev()
            .flat_map(|scope| scope.content.vars.iter_mut())
            .for_each(|(_, var)| var.forked = true);
    }

    pub fn current_scope(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    pub fn current_scope_content_mut(&mut self) -> &mut ScopeContent {
        &mut self.scopes.last_mut().unwrap().content
    }

    pub fn pop_scope(&mut self) -> Scope {
        assert!(self.scopes.len() > 1);
        self.scopes.pop().unwrap()
    }

    pub fn current_file_id(&self) -> Option<FileId> {
        self.current_scope().in_file_id()
    }

    pub fn current_source_file_id(&self) -> Option<u64> {
        self.current_scope().in_real_file_id()
    }

    pub fn current_file(&self) -> Option<&SourceFile> {
        self.current_source_file_id()
            .and_then(|file_id| self.files_map.get_file(file_id))
    }

    pub fn current_file_path(&self) -> Option<&PathBuf> {
        self.current_source_file_id()
            .and_then(|file_id| self.files_map.get_file_path(file_id))
    }

    pub fn register_file(&mut self, path: ScopableFilePath, content: String) -> u64 {
        self.files_map.register_file(path, content)
    }

    // TODO: accelerate this (cache available ranges when creating scope?)
    pub fn visible_scopes(&self) -> impl DoubleEndedIterator<Item = &Scope> {
        let current_scope_range = self.current_scope().range;

        self.scopes
            .iter()
            .filter(move |scope| scope.covers_scope(current_scope_range))
    }

    // // TODO: accelerate this (cache available ranges when creating scope?)
    pub fn visible_scopes_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut Scope> {
        let current_scope_range = self.current_scope().range;

        self.scopes
            .iter_mut()
            .filter(move |scope| scope.covers_scope(current_scope_range))
    }

    pub fn all_vars(&self) -> impl Iterator<Item = (&String, &ScopeVar)> {
        self.visible_scopes()
            .rev()
            .flat_map(|scope| scope.content.vars.iter())
    }

    pub fn all_fns(&self) -> impl Iterator<Item = (&String, &ScopeFn)> {
        self.visible_scopes()
            .rev()
            .flat_map(|scope| scope.content.fns.iter())
    }

    pub fn all_cmd_aliases(&self) -> impl Iterator<Item = (&String, &SingleCmdCall)> {
        self.visible_scopes()
            .rev()
            .flat_map(|scope| scope.content.aliases.iter())
    }

    // pub fn all_type_aliases(&self) -> impl Iterator<Item = (&String, &ValueType)> {
    //     self.visible_scopes()
    //         .rev()
    //         .flat_map(|scope| scope.content.types.iter())
    // }

    pub fn get_exact_type_alias<'s>(&'s self, name: &Eaten<String>) -> Option<&'s ValueType> {
        self.visible_scopes()
            .rev()
            .find_map(|scope| scope.content.types.get(&name.data))
    }

    pub fn get_visible_fn<'s>(&'s self, name: &Eaten<String>) -> Option<&'s ScopeFn> {
        self.visible_scopes()
            .rev()
            .find_map(|scope| scope.content.fns.get(&name.data))
    }

    pub fn get_visible_fn_value<'s>(
        &'s self,
        name: &Eaten<String>,
    ) -> ExecResult<&'s RuntimeFnValue> {
        let Some(func) = self.get_visible_fn(name) else {
            return Err(self.error(name.at, "function not found"));
        };

        Ok(&func.value)
    }

    pub fn get_visible_var<'s>(&'s self, name: &Eaten<String>) -> Option<&'s ScopeVar> {
        self.visible_scopes()
            .find_map(|scope| scope.content.vars.get(&name.data))
    }

    pub fn get_visible_var_mut<'s>(&'s mut self, name: &Eaten<String>) -> Option<&'s mut ScopeVar> {
        self.visible_scopes_mut()
            .find_map(|scope| scope.content.vars.get_mut(&name.data))
    }

    pub fn get_var_value(&self, name: &Eaten<String>) -> ExecResult<&RuntimeValue> {
        let var = self
            .get_visible_var(name)
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
    pub range: ScopeRange,
    pub history_entry: Option<StackTraceEntry>,
    pub content: ScopeContent,
}

impl Scope {
    fn in_file_id(&self) -> Option<FileId> {
        match self.range {
            ScopeRange::Global => None,
            ScopeRange::CodeRange(range) => Some(range.start.file_id),
        }
    }

    fn in_real_file_id(&self) -> Option<u64> {
        match self.range {
            ScopeRange::Global => None,
            ScopeRange::CodeRange(range) => match range.start.file_id {
                FileId::None | FileId::Internal | FileId::Custom(_) => None,
                FileId::Id(id) => Some(id),
            },
        }
    }

    fn covers(&self, range: CodeRange) -> bool {
        match self.range {
            ScopeRange::Global => true,
            ScopeRange::CodeRange(scope_range) => match scope_range.start.file_id {
                FileId::None | FileId::Internal | FileId::Custom(_) => true,
                FileId::Id(id) => match range.start.file_id {
                    FileId::None => unreachable!(),
                    FileId::Id(file_id) => {
                        id == file_id
                            && range.start.offset >= scope_range.start.offset
                            && range.start.offset + range.len
                                <= scope_range.start.offset + scope_range.len
                    }
                    FileId::Internal | FileId::Custom(_) => false,
                },
            },
        }
    }

    fn covers_scope(&self, range: ScopeRange) -> bool {
        match range {
            ScopeRange::Global => match self.range {
                ScopeRange::Global => true,
                ScopeRange::CodeRange(_) => false,
            },
            ScopeRange::CodeRange(range) => self.covers(range),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ScopeRange {
    Global,
    CodeRange(CodeRange),
}

#[derive(Debug, Clone)]
pub struct ScopeContent {
    pub vars: HashMap<String, ScopeVar>,
    pub fns: HashMap<String, ScopeFn>,
    pub aliases: HashMap<String, SingleCmdCall>,
    pub types: HashMap<String, ValueType>,
}

impl ScopeContent {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            fns: HashMap::new(),
            aliases: HashMap::new(),
            types: HashMap::new(),
        }
    }
}

impl Default for ScopeContent {
    fn default() -> Self {
        Self::new()
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
