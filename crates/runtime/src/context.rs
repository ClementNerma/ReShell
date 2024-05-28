use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
    rc::Rc,
};

use indexmap::IndexSet;
use parsy::{CodeRange, Eaten, FileId};
use reshell_checker::{CheckerOutput, CheckerScope, DeclaredVar, Dependency, DependencyType};
use reshell_parser::ast::{
    Block, FnSignature, Program, RuntimeCodeRange, SingleCmdCall, ValueType,
};

use crate::{
    conf::RuntimeConf,
    display::dbg_loc,
    errors::{ExecError, ExecErrorNature, ExecResult},
    files_map::{FilesMap, ScopableFilePath, SourceFile},
    gc::{GcCell, GcReadOnlyCell},
    values::{CapturedDependencies, LocatedValue, RuntimeCmdAlias, RuntimeFnValue, RuntimeValue},
};

/// Scope ID of the native library
pub static NATIVE_LIB_SCOPE_ID: u64 = 0;

/// Scope ID of the very first user scope (which is the only scope to never be deleted)
pub static FIRST_SCOPE_ID: u64 = 1;

/// This structure represents the state of the runtime
/// It contains runtime configuration as well as real-time runtime data
/// It is designed to be reusable in order to run multiple programs in the same base scope
/// (e.g. REPL scenario)
#[derive(Debug)]
pub struct Context {
    /// Runtime configuration
    conf: RuntimeConf,

    /// Auto-incremented scopes ID counter
    /// When a counter is created, this is increased and assigned to the new scope
    scopes_id_counter: u64,

    /// All alive scopes
    /// When a scope ends, it is removed from this map,
    /// except for the very first user scope as well as the native library
    scopes: HashMap<u64, Scope>,

    /// Dependency scopes
    /// Contains all items a scope depends on in order to run
    /// Example: variables referenced by a function returned by its initial scope
    /// For more informations, see [`Context::capture_deps`]
    deps_scopes: HashMap<u64, ScopeContent>,

    /// ID of the current scope
    current_scope: u64,

    /// Map of all files, used for reporting
    files_map: FilesMap,

    /// Path to the current user's home directory
    /// Used for tilde '~' expansion
    home_dir: Option<PathBuf>,

    /// Map of items with their dependencies
    /// For more informations, see [`Context::capture_deps`]
    deps: HashMap<CodeRange, HashSet<Dependency>>,

    /// List of type aliases with their content
    type_aliases: HashMap<CodeRange, Eaten<ValueType>>,

    /// List of type aliases usage
    /// Used to know which type alias is referenced at a given point,
    /// necessary when multiple type aliases with the same name exist
    /// in the program but in different scopes
    type_aliases_usages: HashMap<Eaten<String>, CodeRange>,

    /// List of declared type aliases, used to build a checker scope
    /// See [`ScopeContent::to_checker_scope`] and [`Scope::to_checker_scope`]
    type_aliases_decl: HashMap<CodeRange, HashMap<String, CodeRange>>,

    /// List of function signatures
    /// Used to avoid cloning the whole signature object (which is heavy)
    /// everytime we encounter the same function during runtime (e.g. in a loop)
    fn_signatures: HashMap<CodeRange, Rc<Eaten<FnSignature>>>,

    /// List of function bodies
    /// Used to avoid cloning the whole signature object (which is heavy)
    /// everytime we encounter the same function during runtime (e.g. in a loop)
    fn_bodies: HashMap<CodeRange, Rc<Eaten<Block>>>,

    /// List of command aliases
    /// Used to avoid cloning the whole alias' content (which is heavy)
    /// everytime we encounter the same command alias during runtime (e.g. in a loop)
    cmd_aliases: HashMap<CodeRange, Rc<Eaten<SingleCmdCall>>>,

    /// Value returned by the very last function or command call in a program
    /// that was not assigned or used as an argument
    /// Reset at each new instruction, set by the last function or command call,
    /// retrieved and erased with [`Context::take_wandering_value`]
    wandering_value: Option<RuntimeValue>,
}

impl Context {
    /// Create a new context (runtime state)
    /// The native library's content can be generated using the dedicated crate
    pub fn new(conf: RuntimeConf, native_lib_content: ScopeContent) -> Self {
        let scopes_to_add = [
            Scope {
                id: NATIVE_LIB_SCOPE_ID,
                call_stack: CallStack::empty(),
                content: native_lib_content,
                parent_scopes: IndexSet::new(),
                range: RuntimeCodeRange::Internal,
                previous_scope: None,
                deps_scope: None,
            },
            Scope {
                id: FIRST_SCOPE_ID,
                call_stack: CallStack::empty(),
                content: ScopeContent::new(),
                parent_scopes: IndexSet::from([NATIVE_LIB_SCOPE_ID]),
                range: RuntimeCodeRange::Internal,
                previous_scope: None,
                deps_scope: None,
            },
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
            deps: HashMap::new(),
            deps_scopes: HashMap::new(),
            type_aliases: HashMap::new(),
            type_aliases_usages: HashMap::new(),
            type_aliases_decl: HashMap::new(),
            fn_signatures: HashMap::new(),
            fn_bodies: HashMap::new(),
            cmd_aliases: HashMap::new(),
            wandering_value: None,
            conf,
        }
    }

    /// (Internal) Generate a new scope ID
    fn generate_scope_id(&mut self) -> u64 {
        self.scopes_id_counter += 1;
        self.scopes_id_counter
    }

    /// Get the runtime configuration
    pub fn conf(&self) -> &RuntimeConf {
        &self.conf
    }

    /// Set of change path to the current user's home directory
    /// Used for tilde '~' expansion
    pub fn set_home_dir(&mut self, home_dir: PathBuf) {
        self.home_dir = Some(home_dir);
    }

    /// Get path of the current user's home directory
    /// Used for tilde '~' expansion
    pub fn home_dir(&self) -> Option<&PathBuf> {
        self.home_dir.as_ref()
    }

    /// Register a file
    pub fn register_file(&mut self, path: ScopableFilePath, content: String) -> u64 {
        self.files_map.register_file(path, content)
    }

    /// Get the map of all files
    pub fn files_map(&self) -> &FilesMap {
        &self.files_map
    }

    /// Get the  list of all scopes visible by the current one
    /// Iterating in visibility order
    pub fn visible_scopes(&self) -> impl DoubleEndedIterator<Item = &ScopeContent> {
        let current_scope = self.current_scope();

        current_scope
            .parent_scopes
            // Iterate over all parent scopes
            .iter()
            // Remove scopes that are already dropped (= not referenced anymore)
            .filter_map(|scope_id| self.scopes.get(scope_id))
            // Add the current scope
            .chain([current_scope])
            // Inject scope dependencies
            .flat_map(|scope| {
                [
                    // Scope content
                    Some(&scope.content),
                    // Scope's dependencies
                    scope
                        .deps_scope
                        .map(|deps_scope_id| self.deps_scopes.get(&deps_scope_id).unwrap()),
                ]
            })
            .flatten()
            // Latest scopes in history are the first one to see
            .rev()
    }

    /// (Internal) Generate a checker scope from a runtime scope
    fn generate_checker_scope(&self, scope: &Scope) -> CheckerScope {
        assert!(scope.id == NATIVE_LIB_SCOPE_ID || scope.id == FIRST_SCOPE_ID);

        let Scope { range, content, .. } = &scope;

        let ScopeContent {
            vars,
            fns,
            cmd_aliases,
        } = &content;

        CheckerScope {
            code_range: *range,

            deps: false,
            typ: None,

            cmd_aliases: cmd_aliases
                .iter()
                .map(|(key, value)| (key.clone(), value.name_at))
                .collect(),

            type_aliases: match range {
                RuntimeCodeRange::Internal => Default::default(),
                RuntimeCodeRange::CodeRange(range) => self
                    .type_aliases_decl
                    .get(range)
                    .cloned()
                    .unwrap_or_default(),
            },

            fns: fns
                .iter()
                .map(|(name, scope_fn)| (name.clone(), scope_fn.name_at))
                .collect(),

            vars: vars
                .iter()
                .map(|(name, decl)| {
                    (
                        name.clone(),
                        DeclaredVar {
                            is_mut: decl.is_mut,
                            name_at: decl.name_at,
                        },
                    )
                })
                .collect(),
        }
    }

    /// Get the native library scope (only scope to never be removed) for checker
    pub fn native_lib_scope_for_checker(&self) -> CheckerScope {
        self.generate_checker_scope(self.scopes.get(&NATIVE_LIB_SCOPE_ID).unwrap())
    }

    /// Get the very first user scope (only scope to never be removed) for checker
    pub fn first_scope_for_checker(&self) -> CheckerScope {
        self.generate_checker_scope(self.scopes.get(&FIRST_SCOPE_ID).unwrap())
    }

    /// Get content of the (immutable) native library scope
    pub fn native_lib_scope_content(&self) -> &ScopeContent {
        &self.scopes.get(&NATIVE_LIB_SCOPE_ID).unwrap().content
    }

    /// Prepare the current context to run a new program
    /// Requires the program to have already been checked
    pub fn prepare_for_new_program(&mut self, program: &Program, checker_output: CheckerOutput) {
        // Ensure current scope is correct
        assert_eq!(
            self.current_scope, FIRST_SCOPE_ID,
            "Failed to prepare for new program: context should be set to the first scope's ID"
        );

        // Clear the previous wandering value
        self.clear_wandering_value();

        // Update the current scope's range
        self.scopes.get_mut(&self.current_scope).unwrap().range =
            RuntimeCodeRange::CodeRange(program.content.at);

        // Merge the checker's output
        let CheckerOutput {
            deps,
            type_aliases,
            type_aliases_usages,
            type_aliases_decl,
            fn_signatures,
            fn_bodies,
            cmd_aliases,
        } = checker_output;

        self.deps.extend(deps);
        self.type_aliases.extend(type_aliases);
        self.type_aliases_usages.extend(type_aliases_usages);
        self.type_aliases_decl.extend(type_aliases_decl);

        self.fn_signatures.extend(
            fn_signatures
                .into_iter()
                .map(|(at, signature)| (at, Rc::new(signature))),
        );

        self.fn_bodies
            .extend(fn_bodies.into_iter().map(|(at, body)| (at, Rc::new(body))));

        self.cmd_aliases.extend(
            cmd_aliases
                .into_iter()
                .map(|(at, body)| (at, Rc::new(body))),
        );
    }

    /// Generate an error object
    /// Errors are always wrapped in a [`Box`] to avoid moving very large [`ExecError`] values around
    /// Given an error is either critical or related to a function throwing a value, the allocation
    /// overhead is acceptable here
    pub fn error(
        &self,
        at: impl Into<RuntimeCodeRange>,
        nature: impl Into<ExecErrorNature>,
    ) -> Box<ExecError> {
        let current_scope = self.current_scope();

        Box::new(ExecError {
            at: at.into(),
            in_file: current_scope.in_file_id(),
            source_file: self.current_source_file().cloned(),
            nature: nature.into(),
            call_stack: current_scope.call_stack.clone(),
            scope_range: current_scope.range,
            note: None, // can be changed with .with_note()
        })
    }

    /// Generate an exit value
    pub fn exit(&self, at: impl Into<RuntimeCodeRange>, code: Option<u8>) -> Box<ExecError> {
        self.error(at, ExecErrorNature::Exit { code })
    }

    /// Create and push a new scope above the current one
    pub fn create_and_push_scope(&mut self, range: RuntimeCodeRange, content: ScopeContent) -> u64 {
        let id = self.generate_scope_id();

        let scope = Scope {
            id,
            range,
            parent_scopes: self.generate_parent_scopes_list(),
            content,
            call_stack: self.current_scope().call_stack.clone(),
            previous_scope: Some(self.current_scope),
            deps_scope: None,
        };

        self.push_scope(scope);

        id
    }

    /// Create and push a new scope with dependencies above the current one
    pub fn create_and_push_scope_with_deps(
        &mut self,
        range: RuntimeCodeRange,
        creation_data: DepsScopeCreationData,
        content: ScopeContent,
        parent_scopes: IndexSet<u64>,
        call_stack_entry: Option<CallStackEntry>,
    ) {
        // Fetch all dependencies and build the dependency scope's content
        let deps_scope_content = match creation_data {
            DepsScopeCreationData::Retrieved(content) => content,
            DepsScopeCreationData::CapturedDeps(captured_deps) => {
                let CapturedDependencies {
                    vars,
                    fns,
                    cmd_aliases,
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

                    cmd_aliases: cmd_aliases
                        .iter()
                        .map(|(dep, value)| (dep.name.clone(), value.clone()))
                        .collect(),
                }
            }
        };

        let deps_scope_id = self.generate_scope_id();

        // Insert the dependency scope
        self.deps_scopes.insert(deps_scope_id, deps_scope_content);

        // Update the call stack if necessary
        let mut call_stack = self.current_scope().call_stack.clone();

        if let Some(call_stack_entry) = call_stack_entry {
            call_stack.append(call_stack_entry);
        }

        // Create the new csope
        let scope = Scope {
            id: self.generate_scope_id(),
            range,
            parent_scopes,
            content,
            call_stack,
            previous_scope: Some(self.current_scope),
            deps_scope: Some(deps_scope_id),
        };

        self.push_scope(scope);
    }

    /// Push an already-created scope above the current one
    pub fn push_scope(&mut self, scope: Scope) {
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

    /// Generate the list of all scopes parent ot the current one
    pub fn generate_parent_scopes_list(&self) -> IndexSet<u64> {
        let current_scope = self.current_scope();

        let mut parent_scopes = current_scope.parent_scopes.clone();
        let no_dup = parent_scopes.insert(self.current_scope);

        assert!(no_dup);

        parent_scopes
    }

    /// Get the current scope
    pub fn current_scope(&self) -> &Scope {
        self.scopes.get(&self.current_scope).unwrap()
    }

    /// (Crate-private) Get mutable access to the current scope
    pub(crate) fn current_scope_content_mut(&mut self) -> &mut ScopeContent {
        &mut self.scopes.get_mut(&self.current_scope).unwrap().content
    }

    /// (Crate-private) Remove the current scope
    pub(crate) fn pop_scope(&mut self) {
        self.pop_scope_and_get_deps();
    }

    /// (Crate-private) Remove the current scope and get its dependency scope's content
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

    /// Get the current scope's source file
    pub fn current_source_file(&self) -> Option<&SourceFile> {
        self.current_scope()
            .source_file_id()
            .and_then(|file_id| self.files_map.get_file(file_id))
    }

    /// Get the current scope's source file path
    pub fn current_file_path(&self) -> Option<&PathBuf> {
        self.current_scope()
            .source_file_id()
            .and_then(|file_id| self.files_map.get_file_path(file_id))
    }

    /// Get a specific function
    /// It is guaranteed to be the one referenced at that point in time
    /// as the scopes building ensures this will automatically return the correct one
    pub fn get_visible_fn<'s>(&'s self, name: &Eaten<String>) -> Option<&'s ScopeFn> {
        self.visible_scopes()
            .find_map(|scope| scope.fns.get(&name.data))
    }

    /// Get the value of a specific function
    /// It is guaranteed to be the one referenced at that point in time
    /// as the scopes building ensures this will automatically return the correct one
    pub fn get_visible_fn_value<'s>(
        &'s self,
        name: &Eaten<String>,
    ) -> ExecResult<&'s GcReadOnlyCell<RuntimeFnValue>> {
        let Some(func) = self.get_visible_fn(name) else {
            return Err(self.error(name.at, "function not found"));
        };

        Ok(&func.value)
    }

    /// Get a specific variable
    /// It is guaranteed to be the one referenced at that point in time
    /// as the scopes building ensures this will automatically return the correct one
    pub fn get_visible_var<'c>(&'c self, name: &Eaten<String>) -> Option<&'c ScopeVar> {
        self.visible_scopes()
            .find_map(|scope| scope.vars.get(&name.data))
    }

    /// Get a specific type alias
    /// It is guaranteed to be the one referenced at that point in time
    /// as type alias usages are collected before runtime
    pub fn get_type_alias<'c>(&'c self, name: &Eaten<String>) -> Option<&'c Eaten<ValueType>> {
        let type_alias_at = self.type_aliases_usages.get(name)?;

        Some(self.type_aliases.get(type_alias_at).unwrap())
    }

    /// Get a specific type signature from its location
    /// Avoids cloning the entire (heavy) [`FnSignature`] value
    pub fn get_fn_signature(&self, from: &Eaten<FnSignature>) -> Option<Rc<Eaten<FnSignature>>> {
        self.fn_signatures.get(&from.at).map(Rc::clone)
    }

    /// Get a specific function's body from its location
    /// Avoids cloning the entire (heavy) [`Eaten<Block>`]
    pub fn get_fn_body(&self, from: &Eaten<Block>) -> Option<Rc<Eaten<Block>>> {
        self.fn_bodies.get(&from.at).map(Rc::clone)
    }

    /// Get a specific command alias' content from its location
    /// Avoids cloning the entire (heavy) [`Eaten<SingleCmdCall>`]
    pub fn get_cmd_alias_content(
        &self,
        from: &Eaten<SingleCmdCall>,
    ) -> Option<Rc<Eaten<SingleCmdCall>>> {
        self.cmd_aliases.get(&from.at).map(Rc::clone)
    }

    /// (Crate-private)
    ///
    /// Capture all dependencies for an item used at a point in time
    /// The dependencies list is built before runtime and used here for capture
    ///
    /// For instance, a function referencing a local variable could be returned to the parent scope,
    /// or assigned to a variable belonging to another scope (e.g. a parent one)
    ///
    /// In such case, when the function is called, it must have access to a reference pointing to said variable
    /// But as scopes are dropped whenever they end (in order to free memory), references must be collected inside
    /// a "dependency scope", which will be stored in the context.
    ///
    /// This dependency scope will be dropped (= freed from memory) whenever the related value (e.g. the function)
    /// is dropped.
    ///
    /// References work through [`GcCell`] values which allow to share access to multiple items at the same time
    pub(crate) fn capture_deps(&self, body_content_at: CodeRange) -> CapturedDependencies {
        let mut captured_deps = CapturedDependencies::default();

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
                                .filter(|var| match var.name_at {
                                    RuntimeCodeRange::CodeRange(var_name_at) => var_name_at == *declared_at,
                                    RuntimeCodeRange::Internal => false,
                                })
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
                                .filter(|func| match func.name_at {
                                    RuntimeCodeRange::CodeRange(func_name_at) => func_name_at == *declared_at,
                                    RuntimeCodeRange::Internal => false,
                                })
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
                                .filter(|alias| alias.name_at == *declared_at)
                        })
                        .expect("internal error: cannot find command alias to capture (this is a bug in the checker)");

                    captured_deps
                        .cmd_aliases
                        .insert(dep.clone(), cmd_alias.clone());
                }
            };
        }

        captured_deps
    }

    /// Set the wandering value
    pub fn set_wandering_value(&mut self, value: RuntimeValue) {
        self.wandering_value = Some(value);
    }

    /// (Crate-private) Clear the wandering value
    /// Called at each new instruction
    pub(super) fn clear_wandering_value(&mut self) {
        self.wandering_value = None;
    }

    /// Move the wandering value out of the context
    /// Use cases include e.g. REPL to display the wandering value after execution
    pub fn take_wandering_value(&mut self) -> Option<RuntimeValue> {
        self.wandering_value.take()
    }
}

/// Runtime scope
#[derive(Debug, Clone)]
pub struct Scope {
    /// Unique ID of the scope (not two scopes must have the same ID)
    pub id: u64,
    /// Range of the scope
    pub range: RuntimeCodeRange,
    /// Content of the scope
    pub content: ScopeContent,
    /// List of parent scopes, from farthest to the nearest
    pub parent_scopes: IndexSet<u64>,
    /// Dependencies scope (if any)
    /// See [`Context::capture_deps`] for more informations
    pub deps_scope: Option<u64>,
    /// Previous scope
    /// Used e.g. when calling a function, the function's body will
    /// get a new scope but we must return to the callee's scope afterwards
    pub previous_scope: Option<u64>,
    /// Entire call stack
    pub call_stack: CallStack,
}

impl Scope {
    /// Get this scope's file ID
    pub fn in_file_id(&self) -> Option<FileId> {
        match self.range {
            RuntimeCodeRange::Internal => None,
            RuntimeCodeRange::CodeRange(range) => Some(range.start.file_id),
        }
    }

    /// Get this scope's source file ID
    pub fn source_file_id(&self) -> Option<u64> {
        match self.range {
            RuntimeCodeRange::Internal => None,
            RuntimeCodeRange::CodeRange(range) => match range.start.file_id {
                FileId::None | FileId::Internal | FileId::Custom(_) => None,
                FileId::SourceFile(id) => Some(id),
            },
        }
    }
}

/// Content of a scope
#[derive(Debug, Clone)]
pub struct ScopeContent {
    /// Variables (map keys are variable names)
    pub vars: HashMap<String, ScopeVar>,

    /// Functions (map keys are function names)
    pub fns: HashMap<String, ScopeFn>,

    /// Command aliases (map keys are command alias names)
    pub cmd_aliases: HashMap<String, ScopeCmdAlias>,
}

impl ScopeContent {
    /// Create a new (empty) scope content
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            fns: HashMap::new(),
            cmd_aliases: HashMap::new(),
        }
    }
}

/// Scoped variable
#[derive(Debug, Clone)]
pub struct ScopeVar {
    /// Location of the variable's name in its declaration
    pub name_at: RuntimeCodeRange,
    /// Is the variable mutable?
    pub is_mut: bool,
    /// Value of the variable
    /// It is backed by a [`GcCell`] in order to be sharable in captured dependencies
    /// See [`Context::capture_deps`]
    pub value: GcCell<Option<LocatedValue>>,
}

/// Scoped function
#[derive(Debug, Clone)]
pub struct ScopeFn {
    /// Location of the function's name in its declaration
    pub name_at: RuntimeCodeRange,
    /// Value of the function
    /// It is backed by an immutable [`GcReadOnlyCell`] in order to avoid needless cloning
    pub value: GcReadOnlyCell<RuntimeFnValue>,
}

/// Scoped command alias
#[derive(Debug, Clone)]
pub struct ScopeCmdAlias {
    /// Location of the alias' name in its declaration
    pub name_at: CodeRange,
    /// Content of the alias
    /// It is backed by an immutable [`GcReadOnlyCell`] in order to avoid needless cloning
    pub value: GcReadOnlyCell<RuntimeCmdAlias>,
}

/// A scope's call stack
#[derive(Debug, Clone)]
pub struct CallStack {
    /// All call stack's entries, in chronological ascending order
    history: Vec<CallStackEntry>,
}

impl CallStack {
    /// Create a new (empty) call stack
    pub fn empty() -> Self {
        Self { history: vec![] }
    }

    /// Append a new call stack entry
    pub fn append(&mut self, entry: CallStackEntry) {
        self.history.push(entry);
    }

    /// Get the list in chronological, ascending order of all entries
    pub fn history(&self) -> &[CallStackEntry] {
        &self.history
    }
}

/// A call stack entry
#[derive(Debug, Clone, Copy)]
pub struct CallStackEntry {
    /// Location of a function call
    pub fn_called_at: RuntimeCodeRange,
}

/// Dependency scope creation data
pub enum DepsScopeCreationData {
    /// Use already-captured dependencies
    CapturedDeps(CapturedDependencies),

    /// Use already-built dependency scope
    Retrieved(ScopeContent),
}
