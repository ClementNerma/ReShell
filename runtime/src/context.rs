use std::{collections::HashMap, path::PathBuf};

use indexmap::IndexSet;
use parsy::{CodeRange, Eaten, FileId};
use reshell_parser::ast::{SingleCmdCall, ValueType};

use crate::{
    errors::{ExecError, ExecErrorContent, ExecResult},
    files_map::{FilesMap, ScopableFilePath, SourceFile},
    gc::GcCell,
    native_lib::generate_native_lib,
    values::{LocatedValue, RuntimeFnValue},
};

#[derive(Debug)]
pub struct Context {
    scopes_id_counter: u64,
    scopes: HashMap<u64, Scope>,
    current_scope: u64,
    files_map: FilesMap,
    home_dir: Option<PathBuf>,
}

impl Context {
    pub fn new(home_dir: Option<PathBuf>) -> Self {
        Self {
            scopes_id_counter: 0,
            scopes: HashMap::from([(0, generate_native_lib())]),
            current_scope: 0,
            files_map: FilesMap::new(),
            home_dir,
        }
    }

    pub fn generate_scope_id(&mut self) -> u64 {
        self.scopes_id_counter += 1;
        self.scopes_id_counter
    }

    pub fn set_home_dir(&mut self, home_dir: PathBuf) {
        self.home_dir = Some(home_dir);
    }

    pub fn error(&self, at: CodeRange, content: impl Into<ExecErrorContent>) -> ExecError {
        let current_scope = self.current_scope();

        ExecError {
            at,
            in_file: current_scope.in_file_id(),
            source_file: self.current_source_file().cloned(),
            content: content.into(),
            call_stack: current_scope.call_stack.clone(),
            scope_range: current_scope.range,
        }
    }

    pub fn scopes(&self) -> &HashMap<u64, Scope> {
        &self.scopes
    }

    pub fn files_map(&self) -> &FilesMap {
        &self.files_map
    }

    pub fn home_dir(&self) -> Option<&PathBuf> {
        self.home_dir.as_ref()
    }

    pub fn create_and_push_scope(&mut self, range: ScopeRange, content: ScopeContent) {
        let id = self.generate_scope_id();

        let scope = Scope {
            id,
            range,
            parent_scopes: self.generate_parent_scopes(),
            content,
            call_stack_entry: None,
            call_stack: self.current_scope().call_stack.clone(),
        };

        self.push_custom_scope(scope);
    }

    pub fn create_and_push_called_scope(
        &mut self,
        range: ScopeRange,
        content: ScopeContent,
        parent_scopes: IndexSet<u64>,
        call_stack_entry: CallStackEntry,
    ) {
        let id = self.generate_scope_id();

        let mut call_stack = self.current_scope().call_stack.clone();
        call_stack.append(call_stack_entry);

        let scope = Scope {
            id,
            range,
            parent_scopes,
            content,
            call_stack_entry: Some(call_stack_entry),
            call_stack,
        };

        self.push_custom_scope(scope);
    }

    pub fn push_custom_scope(&mut self, scope: Scope) {
        if let Some(file_id) = scope.source_file_id() {
            assert!(
                self.files_map.has_file(file_id),
                "Provided scope is associated to an unregistered file"
            );
        }

        self.current_scope = scope.id;
        self.scopes.insert(scope.id, scope);
    }

    pub fn generate_parent_scopes(&self) -> IndexSet<u64> {
        let current_scope = self.current_scope();

        let mut parent_scopes = current_scope.parent_scopes.clone();
        let no_dup = parent_scopes.insert(self.current_scope);

        assert!(no_dup);

        parent_scopes
    }

    pub fn current_scope(&self) -> &Scope {
        self.scopes.get(&self.current_scope).unwrap()
    }

    pub fn current_scope_content_mut(&mut self) -> &mut ScopeContent {
        &mut self.scopes.get_mut(&self.current_scope).unwrap().content
    }

    pub fn pop_scope(&mut self) {
        let current_scope = self.current_scope();

        // TODO: garbage collection!

        let prev_scope = current_scope
            .call_stack_entry
            .as_ref()
            .map(|entry| entry.previous_scope);

        self.current_scope = match prev_scope {
            Some(prev_scope) => prev_scope,
            None => current_scope
                .parent_scopes
                .last()
                .copied()
                .expect("unexpected: cannot pop a scope without parents"),
        }

        // self.current_scope = match current_scope.parent_scopes.last() {
        //     Some(parent_scope_id) => *parent_scope_id,
        //     None => {
        //         assert!(current_scope.id != 0);
        //         0
        //     }
        // };
    }

    pub fn current_source_file(&self) -> Option<&SourceFile> {
        self.current_scope()
            .source_file_id()
            .and_then(|file_id| self.files_map.get_file(file_id))
    }

    pub fn current_file_path(&self) -> Option<&PathBuf> {
        self.current_scope()
            .source_file_id()
            .and_then(|file_id| self.files_map.get_file_path(file_id))
    }

    pub fn register_file(&mut self, path: ScopableFilePath, content: String) -> u64 {
        self.files_map.register_file(path, content)
    }

    pub fn visible_scopes(&self) -> impl DoubleEndedIterator<Item = &Scope> {
        let current_scope = self.current_scope();

        current_scope
            .parent_scopes
            .iter()
            .filter_map(|scope_id| self.scopes.get(scope_id))
            .chain([current_scope])
            .rev()
    }

    pub fn get_visible_fn<'s>(&'s self, name: &Eaten<String>) -> Option<&'s ScopeFn> {
        self.visible_scopes()
            .find_map(|scope| scope.content.fns.get(&name.data))
    }

    pub fn get_visible_fn_value<'s>(
        &'s self,
        name: &Eaten<String>,
    ) -> ExecResult<&'s GcCell<RuntimeFnValue>> {
        let Some(func) = self.get_visible_fn(name) else {
            return Err(self.error(name.at, "function not found"));
        };

        Ok(&func.value)
    }

    pub fn get_visible_var<'s>(&'s self, name: &Eaten<String>) -> Option<&'s GcCell<ScopeVar>> {
        self.visible_scopes()
            .find_map(|scope| scope.content.vars.get(&name.data))
    }

    // pub fn get_var_value<'s>(&'s self, name: &Eaten<String>) -> ExecResult<RwLockReadGuard<'s, RuntimeValue>> {
    //     let var = self
    //         .get_visible_var(name)
    //         .ok_or_else(|| self.error(name.at, "variable not found"))?;

    //     let located_val = var
    //         .read()
    //         .value
    //         .as_ref()
    //         .ok_or_else(|| self.error(name.at, "variable does not have a value yet"))?;

    //     // TODO: unsolvable problem?
    //     todo!()
    // }
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub id: u64,
    pub range: ScopeRange,
    pub content: ScopeContent,
    pub parent_scopes: IndexSet<u64>,
    pub call_stack_entry: Option<CallStackEntry>,
    pub call_stack: CallStack,
}

impl Scope {
    pub fn in_file_id(&self) -> Option<FileId> {
        match self.range {
            ScopeRange::Global => None,
            ScopeRange::CodeRange(range) => Some(range.start.file_id),
        }
    }

    pub fn source_file_id(&self) -> Option<u64> {
        match self.range {
            ScopeRange::Global => None,
            ScopeRange::CodeRange(range) => match range.start.file_id {
                FileId::None | FileId::Internal | FileId::Custom(_) => None,
                FileId::Id(id) => Some(id),
            },
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
    pub vars: HashMap<String, GcCell<ScopeVar>>,
    pub fns: HashMap<String, ScopeFn>,
    pub cmd_aliases: HashMap<String, SingleCmdCall>,
    pub types: HashMap<String, ValueType>,
}

impl ScopeContent {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            fns: HashMap::new(),
            cmd_aliases: HashMap::new(),
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
}

#[derive(Debug, Clone)]
pub struct ScopeFn {
    pub declared_at: CodeRange,
    pub value: GcCell<RuntimeFnValue>,
}

#[derive(Debug, Clone)]
pub struct CallStack {
    history: Vec<CallStackEntry>,
}

impl CallStack {
    pub fn empty() -> Self {
        Self { history: vec![] }
    }

    pub fn append(&mut self, entry: CallStackEntry) {
        self.history.push(entry);
    }

    pub fn history(&self) -> &[CallStackEntry] {
        &self.history
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CallStackEntry {
    pub fn_called_at: CodeRange,
    pub previous_scope: u64,
    // pub args: Vec<RuntimeValue>,
}
