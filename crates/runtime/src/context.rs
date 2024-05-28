use std::{collections::HashMap, path::PathBuf};

use indexmap::{IndexMap, IndexSet};
use once_cell::sync::Lazy;
use parsy::{CodeRange, Eaten, FileId, Location};
use reshell_checker::{CheckerOutput, CheckerScope, Dependency, DependencyType};

use crate::{
    conf::RuntimeConf,
    display::dbg_loc,
    errors::{ExecError, ExecErrorContent, ExecResult},
    files_map::{FilesMap, ScopableFilePath, SourceFile},
    gc::GcCell,
    native_lib::generate_native_lib,
    values::{
        CapturedDependencies, LocatedValue, RuntimeCmdAlias, RuntimeFnValue, RuntimeTypeAlias,
    },
};

pub static NATIVE_LIB_SCOPE_ID: u64 = 0;
pub static FIRST_SCOPE_ID: u64 = 1;

static INITIAL_NATIVE_LIB_SCOPE: Lazy<Scope> = Lazy::new(generate_native_lib);
static INITIAL_FIRST_SCOPE: Lazy<Scope> = Lazy::new(|| Scope {
    id: FIRST_SCOPE_ID,
    call_stack: CallStack::empty(),
    call_stack_entry: None,
    content: ScopeContent::new(),
    parent_scopes: IndexSet::from([NATIVE_LIB_SCOPE_ID]),
    range: ScopeRange::SourceLess,
    previous_scope: None,
    deps_scope: None,
});

#[derive(Debug)]
pub struct Context {
    conf: RuntimeConf,
    scopes_id_counter: u64,
    scopes: HashMap<u64, Scope>,
    deps_scopes: HashMap<u64, ScopeContent>,
    current_scope: u64,
    files_map: FilesMap,
    home_dir: Option<PathBuf>,
    deps: IndexMap<CodeRange, IndexSet<Dependency>>,
}

impl Context {
    pub fn new(conf: RuntimeConf) -> Self {
        let scopes_to_add = [
            INITIAL_NATIVE_LIB_SCOPE.clone(),
            INITIAL_FIRST_SCOPE.clone(),
        ];

        let mut scopes = HashMap::new();

        for scope in scopes_to_add {
            scopes.insert(scope.id, scope);
        }

        Self {
            scopes_id_counter: FIRST_SCOPE_ID,
            scopes,
            current_scope: FIRST_SCOPE_ID,
            files_map: FilesMap::new(),
            home_dir: conf.initial_home_dir.clone(),
            deps: IndexMap::new(),
            deps_scopes: HashMap::new(),
            conf,
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

    pub fn visible_scopes(&self) -> impl DoubleEndedIterator<Item = &ScopeContent> {
        let current_scope = self.current_scope();

        current_scope
            .parent_scopes
            .iter()
            .filter_map(|scope_id| self.scopes.get(scope_id))
            .chain([current_scope])
            .flat_map(|scope| {
                [
                    scope
                        .deps_scope
                        .map(|deps_scope_id| self.deps_scopes.get(&deps_scope_id).unwrap()),
                    Some(&scope.content),
                ]
            })
            .flatten()
            .rev()
    }

    pub fn first_scope(&self) -> &Scope {
        self.scopes.get(&FIRST_SCOPE_ID).unwrap()
    }

    pub fn deps_for_debug(&self, from: &CodeRange) -> Option<&IndexSet<Dependency>> {
        self.deps.get(from)
    }

    // =============== Non-public functions =============== //

    pub(crate) fn append_checker_output(&mut self, checker_output: CheckerOutput) {
        let CheckerOutput { deps } = checker_output;
        self.deps = deps;
    }

    pub(crate) fn generate_scope_id(&mut self) -> u64 {
        self.scopes_id_counter += 1;
        self.scopes_id_counter
    }

    pub(crate) fn set_curr_scope_range(&mut self, range: CodeRange) {
        self.scopes.get_mut(&self.current_scope).unwrap().range = ScopeRange::CodeRange(range);
    }

    pub(crate) fn reset_to_first_scope(&mut self) {
        self.current_scope = FIRST_SCOPE_ID;
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
            previous_scope: Some(self.current_scope),
            deps_scope: None,
        };

        self.push_scope(scope);

        id
    }

    pub(crate) fn create_and_push_scope_with_deps(
        &mut self,
        range: ScopeRange,
        creation_data: DepsScopeCreationData,
        content: ScopeContent,
        parent_scopes: IndexSet<u64>,
        call_stack_entry: Option<CallStackEntry>,
    ) {
        let deps_scope_content = match creation_data {
            DepsScopeCreationData::Retrieved(content) => content,
            DepsScopeCreationData::CapturedDeps(captured_deps) => {
                let CapturedDependencies {
                    vars,
                    fns,
                    cmd_aliases,
                    type_aliases,
                } = captured_deps;
        
                ScopeContent {
                    vars: vars
                        .iter()
                        .map(|(dep, value)| (dep.name.clone(), value.clone()))
                        .collect(),
    
                    fns: fns
                        .iter()
                        .map(|(dep, value)| (dep.name.clone(), value.clone()))
                        .collect(),
    
                    type_aliases: type_aliases
                        .iter()
                        .map(|(dep, value)| (dep.name.clone(), value.clone()))
                        .collect(),
    
                    cmd_aliases: cmd_aliases
                        .iter()
                        .map(|(dep, value)| (dep.name.clone(), value.clone()))
                        .collect(),
                }
            },
        };
        
        let deps_scope_id = self.generate_scope_id();

        self.deps_scopes.insert(
            deps_scope_id,
            deps_scope_content
        );

        let mut call_stack = self.current_scope().call_stack.clone();

        let id = self.generate_scope_id();

        if let Some(call_stack_entry) = call_stack_entry {
            call_stack.append(call_stack_entry);
        }

        let scope = Scope {
            id,
            range,
            parent_scopes,
            content,
            call_stack_entry,
            call_stack,
            previous_scope: Some(self.current_scope),
            deps_scope: Some(deps_scope_id),
        };

        self.push_scope(scope);
    }

    pub(crate) fn push_scope(&mut self, scope: Scope) {
        if let Some(file_id) = scope.source_file_id() {
            assert!(
                self.files_map.has_file(file_id),
                "Provided scope is associated to an unregistered file"
            );
        }

        if scope.call_stack.history.len() > self.conf.call_stack_limit {
            panic!(
                "Maximum call stack size ({}) exceeded",
                self.conf.call_stack_limit
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
        self.pop_scope_and_get_deps();
    }

    pub(crate) fn pop_scope_and_get_deps(&mut self) -> Option<ScopeContent> {
        assert!(self.current_scope > FIRST_SCOPE_ID);

        let current_scope = self.scopes.remove(&self.current_scope).unwrap();

        let deps_scope = current_scope
            .deps_scope
            .map(|scope_id| self.deps_scopes.remove(&scope_id).unwrap());

        // assert!(ENSURE scopes with stack entry (= fn calls) have a deps scope);

        self.current_scope = current_scope.previous_scope.unwrap();

        assert!(self.scopes.contains_key(&self.current_scope));

        deps_scope
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
            .find_map(|scope| scope.fns.get(&name.data))
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
            .find_map(|scope| scope.vars.get(&name.data))
    }

    pub(crate) fn capture_deps(&self, body_content_at: CodeRange) -> CapturedDependencies {
        let mut captured_deps = CapturedDependencies {
            vars: HashMap::new(),
            fns: HashMap::new(),
            type_aliases: HashMap::new(),
            cmd_aliases: HashMap::new(),
        };

        let deps_list = self.deps.get(&body_content_at).expect(
            "internal error: dependencies informations not found while constructing value (this is a bug in the checker)"
        );

        for dep in deps_list {
            let Dependency {
                name,
                declared_at,
                dep_type,
            } = dep;

            match dep_type {
                DependencyType::Variable => {
                    let var = self
                        .visible_scopes()
                        .find_map(|scope| {
                            scope
                                .vars
                                .get(name)
                                .filter(|var| var.declared_at == *declared_at)
                        })
                        .unwrap_or_else(|| panic!(
                            "internal error: cannot find variable to capture (this is a bug in the checker)\nDetails:\n> {name} (declared at {})",
                            dbg_loc(*declared_at, self.files_map()
                        )));

                    captured_deps.vars.insert(dep.clone(), var.clone());
                }

                DependencyType::Function => {
                    let func = self
                        .visible_scopes()
                        .find_map(|scope| {
                            scope
                                
                                .fns
                                .get(name)
                                .filter(|func| func.declared_at == *declared_at)
                        })
                        .expect("internal error: cannot find function to capture (this is a bug in the checker)");

                    captured_deps.fns.insert(dep.clone(), func.clone());
                }

                DependencyType::CmdAlias => {
                    let cmd_alias = self
                        .visible_scopes()
                        .find_map(|scope| {
                            scope
                                
                                .cmd_aliases
                                .get(name)
                                .filter(|alias| alias.name_declared_at == *declared_at)
                        })
                        .expect("internal error: cannot find command alias to capture (this is a bug in the checker)");

                    captured_deps
                        .cmd_aliases
                        .insert(dep.clone(), cmd_alias.clone());
                }

                DependencyType::TypeAlias => {
                    let type_alias = self
                        .visible_scopes()
                        .find_map(|scope| {
                            scope
                                
                                .type_aliases
                                .get(name)
                                .filter(|alias| alias.name_declared_at == *declared_at)
                        })
                        .expect("internal error: cannot find type alias to capture (this is a bug in the checker)");

                    captured_deps
                        .type_aliases
                        .insert(dep.clone(), type_alias.clone());
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
    pub deps_scope: Option<u64>,
    pub previous_scope: Option<u64>,
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
            type_aliases,
        } = content;

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

            deps: false, // TODO: isn't that incorrect?
            fn_args_at: None,

            cmd_aliases: cmd_aliases
                .iter()
                .map(|(key, value)| (key.clone(), value.name_declared_at))
                .collect(),

            type_aliases: type_aliases
                .iter()
                .map(|(key, value)| (key.clone(), value.name_declared_at))
                .collect(),

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
    pub cmd_aliases: HashMap<String, RuntimeCmdAlias>,
    pub type_aliases: HashMap<String, RuntimeTypeAlias>,
}

impl ScopeContent {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            fns: HashMap::new(),
            cmd_aliases: HashMap::new(),
            type_aliases: HashMap::new(),
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
}

pub enum DepsScopeCreationData {
    CapturedDeps(CapturedDependencies),
    Retrieved(ScopeContent)
}