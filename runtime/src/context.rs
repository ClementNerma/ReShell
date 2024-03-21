use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

use indexmap::{IndexMap, IndexSet};
use parsy::{CodeRange, Eaten, FileId, Location};
use reshell_checker::{CheckerOutput, CheckerScope, Dependency, DependencyType};
use reshell_parser::ast::{Block, SingleCmdCall, ValueType};

use crate::{
    errors::{ExecError, ExecErrorContent, ExecResult},
    files_map::{FilesMap, ScopableFilePath, SourceFile},
    gc::GcCell,
    native_lib::generate_native_lib,
    values::{CapturedDependencies, LocatedValue, RuntimeFnValue},
};

#[derive(Debug)]
pub struct Context {
    scopes_id_counter: u64,
    scopes: HashMap<u64, Scope>,
    current_scope: u64,
    files_map: FilesMap,
    home_dir: Option<PathBuf>,
    fn_deps: IndexMap<CodeRange, IndexSet<Dependency>>,
    fn_deps_cleanup: HashMap<u64, u64>,
}

impl Context {
    pub fn new(home_dir: Option<PathBuf>) -> Self {
        Self {
            scopes_id_counter: 1,
            scopes: HashMap::from([
                (0, generate_native_lib()),
                (
                    1,
                    Scope {
                        id: 1,
                        call_stack: CallStack::empty(),
                        call_stack_entry: None,
                        content: ScopeContent::new(),
                        parent_scopes: IndexSet::from([1]),
                        range: ScopeRange::SourceLess,
                    },
                ),
            ]),
            current_scope: 1,
            files_map: FilesMap::new(),
            home_dir,
            fn_deps: IndexMap::new(),
            fn_deps_cleanup: HashMap::new(),
        }
    }

    pub fn set_home_dir(&mut self, home_dir: PathBuf) {
        self.home_dir = Some(home_dir);
    }

    pub fn register_file(&mut self, path: ScopableFilePath, content: String) -> u64 {
        self.files_map.register_file(path, content)
    }

    pub fn files_map(&self) -> &FilesMap {
        &self.files_map
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

    pub fn first_scope(&self) -> &Scope {
        self.scopes.get(&1).unwrap()
    }

    // =============== Non-public functions =============== //

    pub(crate) fn append_checker_output(&mut self, checker_output: CheckerOutput) {
        let CheckerOutput { fn_deps } = checker_output;

        for (fn_decl, deps) in fn_deps {
            let dup = self.fn_deps.insert(fn_decl, deps);
            assert!(dup.is_none());
        }
    }

    pub(crate) fn generate_scope_id(&mut self) -> u64 {
        self.scopes_id_counter += 1;
        self.scopes_id_counter
    }

    pub(crate) fn set_curr_scope_range(&mut self, range: CodeRange) {
        self.scopes.get_mut(&self.current_scope).unwrap().range = ScopeRange::CodeRange(range);
    }

    pub(crate) fn reset_to_first_scope(&mut self) {
        self.current_scope = 1;
    }

    pub(crate) fn error(&self, at: CodeRange, content: impl Into<ExecErrorContent>) -> ExecError {
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

    pub(crate) fn scopes(&self) -> &HashMap<u64, Scope> {
        &self.scopes
    }

    pub(crate) fn home_dir(&self) -> Option<&PathBuf> {
        self.home_dir.as_ref()
    }

    pub(crate) fn create_and_push_scope(
        &mut self,
        range: ScopeRange,
        content: ScopeContent,
    ) -> u64 {
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

        id
    }

    pub(crate) fn create_and_push_fn_scope(
        &mut self,
        range: ScopeRange,
        captured_deps: &CapturedDependencies,
        content: ScopeContent,
        mut parent_scopes: IndexSet<u64>,
        call_stack_entry: CallStackEntry,
    ) {
        let CapturedDependencies { vars, fns } = captured_deps;

        let deps_scope_id = self.create_and_push_scope(
            range,
            ScopeContent {
                vars: vars
                    .iter()
                    .map(|(dep, value)| (dep.name.clone(), value.clone()))
                    .collect(),

                fns: fns
                    .iter()
                    .map(|(dep, value)| (dep.name.clone(), value.clone()))
                    .collect(),

                // TODO?
                cmd_aliases: HashMap::new(),
                // TODO?
                types: HashMap::new(),
            },
        );

        parent_scopes.insert(deps_scope_id);

        let id = self.generate_scope_id();

        self.fn_deps_cleanup.insert(id, deps_scope_id);

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

    pub(crate) fn push_custom_scope(&mut self, scope: Scope) {
        if let Some(file_id) = scope.source_file_id() {
            assert!(
                self.files_map.has_file(file_id),
                "Provided scope is associated to an unregistered file"
            );
        }

        self.current_scope = scope.id;
        self.scopes.insert(scope.id, scope);
    }

    pub(crate) fn generate_parent_scopes(&self) -> IndexSet<u64> {
        let current_scope = self.current_scope();

        let mut parent_scopes = current_scope.parent_scopes.clone();
        let no_dup = parent_scopes.insert(self.current_scope);

        assert!(no_dup);

        parent_scopes
    }

    pub(crate) fn current_scope(&self) -> &Scope {
        self.scopes.get(&self.current_scope).unwrap()
    }

    pub(crate) fn current_scope_content_mut(&mut self) -> &mut ScopeContent {
        &mut self.scopes.get_mut(&self.current_scope).unwrap().content
    }

    pub(crate) fn pop_scope(&mut self) {
        assert!(self.current_scope > 1);

        let current_scope = self.scopes.remove(&self.current_scope).unwrap();

        if current_scope.call_stack_entry.is_some() {
            let fn_deps_scope = self.fn_deps_cleanup.remove(&current_scope.id).unwrap();
            self.scopes.remove(&fn_deps_scope).unwrap();
        }

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
        };
    }

    pub(crate) fn current_source_file(&self) -> Option<&SourceFile> {
        self.current_scope()
            .source_file_id()
            .and_then(|file_id| self.files_map.get_file(file_id))
    }

    pub(crate) fn current_file_path(&self) -> Option<&PathBuf> {
        self.current_scope()
            .source_file_id()
            .and_then(|file_id| self.files_map.get_file_path(file_id))
    }

    pub(crate) fn get_visible_fn<'s>(&'s self, name: &Eaten<String>) -> Option<&'s ScopeFn> {
        self.visible_scopes()
            .find_map(|scope| scope.content.fns.get(&name.data))
    }

    pub(crate) fn get_visible_fn_value<'s>(
        &'s self,
        name: &Eaten<String>,
    ) -> ExecResult<&'s GcCell<RuntimeFnValue>> {
        let Some(func) = self.get_visible_fn(name) else {
            return Err(self.error(name.at, "function not found"));
        };

        Ok(&func.value)
    }

    pub(crate) fn get_visible_var<'s>(&'s self, name: &Eaten<String>) -> Option<&'s ScopeVar> {
        self.visible_scopes()
            .find_map(|scope| scope.content.vars.get(&name.data))
    }

    pub(crate) fn capture_deps(&self, fn_body: &Eaten<Block>) -> CapturedDependencies {
        let mut captured_deps = CapturedDependencies {
            vars: HashMap::new(),
            fns: HashMap::new(),
        };

        let fn_body_at = fn_body.data.code_range;

        let deps_list = self.fn_deps.get(&fn_body_at).expect(
            "internal error: dependencies informations not found while constructing function value (this is a bug in the checker)"
        );

        for dep in deps_list {
            let Dependency {
                name,
                name_declared_at,
                dep_type,
            } = dep;

            match dep_type {
                DependencyType::Variable => {
                    let var = self
                        .visible_scopes()
                        .find_map(|scope| {
                            scope
                                .content
                                .vars
                                .get(name)
                                .filter(|var| var.declared_at == *name_declared_at)
                        })
                        .unwrap_or_else(|| panic!("internal error: cannot find variable to capture (this is a bug in the checker)\nDetails:\n> {name} (declared at {name_declared_at:?})"));

                    captured_deps.vars.insert(dep.clone(), var.clone());
                }

                DependencyType::Function => {
                    let func = self
                        .visible_scopes()
                        .find_map(|scope| {
                            scope
                                .content
                                .fns
                                .get(name)
                                .filter(|func| func.declared_at == *name_declared_at)
                        })
                        .expect("internal error: cannot find variable to capture (this is a bug in the checker)");

                    captured_deps.fns.insert(dep.clone(), func.clone());
                }
            };
        }

        captured_deps
    }
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
    pub(crate) fn in_file_id(&self) -> Option<FileId> {
        match self.range {
            ScopeRange::SourceLess => None,
            ScopeRange::CodeRange(range) => Some(range.start.file_id),
        }
    }

    pub(crate) fn source_file_id(&self) -> Option<u64> {
        match self.range {
            ScopeRange::SourceLess => None,
            ScopeRange::CodeRange(range) => match range.start.file_id {
                FileId::None | FileId::Internal | FileId::Custom(_) => None,
                FileId::SourceFile(id) => Some(id),
            },
        }
    }

    pub fn to_checker_scope(&self) -> CheckerScope {
        let Scope { content, range, .. } = self;

        let ScopeContent {
            vars,
            fns,
            cmd_aliases,
            types,
        } = content;

        // For now
        assert!(cmd_aliases.is_empty());
        assert!(types.is_empty());
        //////////

        CheckerScope {
            code_range: match range {
                ScopeRange::SourceLess => {
                    // TODO: ugly hack
                    CodeRange::new(
                        Location {
                            file_id: FileId::Internal,
                            offset: 0,
                        },
                        0,
                    )
                }
                ScopeRange::CodeRange(range) => *range,
            },

            fn_args_at: None,

            cmd_aliases: HashSet::new(), // TODO
            types: HashSet::new(),       // TODO

            fns: fns
                .iter()
                .map(|(name, scope_fn)| (name.clone(), scope_fn.declared_at))
                .collect(),

            vars: vars
                .iter()
                .map(|(name, decl)| {
                    (
                        name.clone(),
                        reshell_checker::DeclaredVar {
                            is_mut: decl.is_mut,
                            name_at: decl.declared_at,
                        },
                    )
                })
                .collect(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ScopeRange {
    SourceLess,
    CodeRange(CodeRange),
}

#[derive(Debug, Clone)]
pub struct ScopeContent {
    pub vars: HashMap<String, ScopeVar>,
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
    pub value: GcCell<Option<LocatedValue>>,
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
    pub(crate) fn empty() -> Self {
        Self { history: vec![] }
    }

    pub(crate) fn append(&mut self, entry: CallStackEntry) {
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
