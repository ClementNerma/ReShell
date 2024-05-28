use std::{collections::HashMap, path::PathBuf, rc::Rc};

use indexmap::IndexSet;
use parsy::{CodeRange, Eaten, FileId, SourceFileID};
use reshell_checker::{
    long_flag_var_name,
    output::{
        CheckerOutput, Dependency, DependencyType, DevelopedCmdAliasCall, DevelopedSingleCmdCall,
    },
    CheckerScope, DeclaredCmdAlias, DeclaredFn, DeclaredVar,
};
use reshell_parser::{
    ast::{
        CmdCall, FnSignature, FunctionBody, Program, RuntimeCodeRange, RuntimeEaten, SingleCmdCall,
        ValueType,
    },
    files::FilesMap,
};

use crate::{
    bin_resolver::BinariesResolver,
    conf::RuntimeConf,
    display::dbg_loc,
    errors::{ExecError, ExecErrorNature, ExecResult},
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
pub struct Context {
    /// Context configuration
    conf: ContextCreationParams,

    /// Binaries resolver
    bin_resolver: BinariesResolver,

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

    /// ID of the current program's main scope
    program_main_scope: Option<u64>,

    /// Data collected from the checker
    /// Whenever a new program is run, the new program's data is merged with the existing one
    collected: CheckerOutput,

    /// Value returned by the very last function or command call in a program
    /// that was not assigned or used as an argument
    /// Reset at each new instruction, set by the last function or command call,
    /// retrieved and erased with [`Context::take_wandering_value`]
    wandering_value: Option<RuntimeValue>,

    /// Converted long flag names
    ///
    /// Used to avoid having to compute camel case version of each long flag at runtime
    long_flags_var_name: HashMap<String, String>,
}

impl Context {
    /// Create a new context (runtime state)
    /// The native library's content can be generated using the dedicated crate
    pub fn new(
        conf: ContextCreationParams,
        bin_resolver: BinariesResolver,
        native_lib_content: ScopeContent,
    ) -> Self {
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
            bin_resolver,
            deps_scopes: HashMap::new(),
            current_scope: FIRST_SCOPE_ID,
            program_main_scope: None,
            collected: CheckerOutput::empty(),
            wandering_value: None,
            long_flags_var_name: HashMap::new(),
            conf,
        }
    }

    /// (Internal) Generate a new scope ID
    fn generate_scope_id(&mut self) -> u64 {
        self.scopes_id_counter += 1;
        self.scopes_id_counter
    }

    /// Get the runtime configuration
    pub fn runtime_conf(&self) -> &RuntimeConf {
        &self.conf.runtime_conf
    }

    /// (Semi-Private) Clear Ctrl+C press indicator
    pub(crate) fn reset_ctrl_c_press_indicator(&self) {
        (self.conf.take_ctrl_c_indicator)();
    }

    /// (Semi-Private) Ensure Ctrl+C was not pressed
    /// Otherwise, return a Ctrl+C error
    pub(crate) fn ensure_no_ctrl_c_press(&self, at: impl Into<RuntimeCodeRange>) -> ExecResult<()> {
        if (self.conf.take_ctrl_c_indicator)() {
            Err(self.error(at.into(), ExecErrorNature::CtrlC))
        } else {
            Ok(())
        }
    }

    /// Get path of the current user's home directory
    /// Used for tilde '~' expansion
    pub fn home_dir(&self) -> Option<&PathBuf> {
        self.conf.home_dir.as_ref()
    }

    /// Get read-only view of the files map
    pub fn files_map(&self) -> &FilesMap {
        &self.conf.files_map
    }

    /// Get the current program's main scope ID
    pub fn program_main_scope(&self) -> Option<u64> {
        self.program_main_scope
    }

    /// Access the binaries resolver
    pub fn binaries_resolver(&mut self) -> &mut BinariesResolver {
        &mut self.bin_resolver
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

    /// Generate checker scopes from current runtime scope hierarchy
    pub fn generate_checker_scopes(&self) -> Vec<CheckerScope> {
        self.generate_parent_scopes_list()
            .iter()
            .map(|scope_id| {
                let scope = self.scopes.get(scope_id).unwrap();

                let Scope { range, content, .. } = &scope;

                let ScopeContent {
                    vars,
                    fns,
                    cmd_aliases,
                } = &content;

                CheckerScope {
                    code_range: *range,

                    special_scope_type: None,

                    cmd_aliases: cmd_aliases
                        .iter()
                        .map(|(key, value)| {
                            (
                                key.clone(),
                                DeclaredCmdAlias::ready(
                                    value.name_at,
                                    value.value.alias_content.at,
                                ),
                            )
                        })
                        .collect(),

                    type_aliases: match range {
                        RuntimeCodeRange::Internal => Default::default(),
                        RuntimeCodeRange::Parsed(range) => self
                            .collected
                            .type_aliases_decl_by_scope
                            .get(range)
                            .cloned()
                            .unwrap_or_default(),
                    },

                    fns: fns
                        .iter()
                        .map(|(name, scope_fn)| {
                            (
                                name.clone(),
                                DeclaredFn::new(
                                    scope_fn.name_at,
                                    scope_fn.value.signature.inner().is_method(),
                                ),
                            )
                        })
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
            })
            .collect()
    }

    /// Get content of the (immutable) native library scope
    pub fn native_lib_scope_content(&self) -> &ScopeContent {
        &self.scopes.get(&NATIVE_LIB_SCOPE_ID).unwrap().content
    }

    /// Prepare the current context to run a new program
    /// Requires the program to have already been checked
    pub fn prepare_for_new_program(
        &mut self,
        program: &Eaten<Program>,
        checker_output: CheckerOutput,
    ) {
        // Create a new scope for this program
        let scope_id =
            self.create_and_push_scope(RuntimeCodeRange::Parsed(program.at), ScopeContent::new());

        // Reset scope
        self.current_scope = scope_id;

        // Remember the program's main scope
        self.program_main_scope = Some(scope_id);

        // Clear the previous wandering value
        self.clear_wandering_value();

        // Merge the checker's output
        self.collected.merge(checker_output);
    }

    /// (Internal) Reset to the program's main scope after execution
    pub(crate) fn reset_to_program_main_scope(&mut self) {
        let scope_id = self.program_main_scope.unwrap();
        assert!(self.scopes.contains_key(&scope_id));

        self.current_scope = scope_id;
        // self.program_main_scope = None;
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
            nature: nature.into(),
            call_stack: current_scope.call_stack.clone(),
            infos: vec![],
        })
    }

    /// Generate an exit value
    pub fn exit(&self, at: impl Into<RuntimeCodeRange>, code: Option<u8>) -> Box<ExecError> {
        self.error(at, ExecErrorNature::Exit { code })
    }

    /// Panic because of an internal error
    pub fn panic(&self, at: impl Into<RuntimeCodeRange>, message: impl AsRef<str>) -> ! {
        panic!(
            "\n| An internal error occured.\n| This is not supposed to happen and is the result of a bug in the shell itself (not your program).\n|\n| Details : {}\n| Location: {}\n",
            message.as_ref(),
            dbg_loc(at.into(), self.files_map())
        );
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
        content: Option<ScopeContent>,
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

        // Create the new scope
        let scope = Scope {
            id: self.generate_scope_id(),
            range,
            parent_scopes,
            content: content.unwrap_or_else(ScopeContent::new),
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
                self.conf.files_map.get_file(file_id).is_some(),
                "Provided scope is associated to an unregistered file"
            );
        }

        if scope.call_stack.history.len() > self.conf.runtime_conf.call_stack_limit {
            panic!(
                "Maximum call stack size ({}) exceeded",
                self.conf.runtime_conf.call_stack_limit
            );
        }

        self.current_scope = scope.id;
        self.scopes.insert(scope.id, scope);
    }

    /// Run an imported program
    pub fn inside_imported_program<T>(
        &mut self,
        program: &Eaten<Program>,
        run: impl FnOnce(&mut Self) -> T,
    ) -> T {
        fn current_scope_mut(ctx: &mut Context) -> &mut Scope {
            ctx.scopes.get_mut(&ctx.current_scope).unwrap()
        }

        let prev = current_scope_mut(self).range;

        current_scope_mut(self).range = RuntimeCodeRange::Parsed(program.at);

        let result = run(self);

        current_scope_mut(self).range = prev;

        result
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
        // assert!(self.current_scope > FIRST_SCOPE_ID);
        assert!(self.current_scope > self.program_main_scope.unwrap());

        let current_scope = self.scopes.remove(&self.current_scope).unwrap();

        let deps_scope = current_scope
            .deps_scope
            .map(|scope_id| self.deps_scopes.remove(&scope_id).unwrap());

        // assert!(ENSURE scopes with stack entry (= fn calls) have a deps scope);

        self.current_scope = current_scope.previous_scope.unwrap();

        assert!(self.scopes.contains_key(&self.current_scope));

        deps_scope
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
            self.panic(name.at, "function not found (= bug in checker)");
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
        let type_alias_at = self.collected.type_aliases_usages.get(name)?;

        Some(self.collected.type_aliases_decl.get(type_alias_at).unwrap())
    }

    /// Get a specific type signature from its location
    /// Avoids cloning the entire (heavy) [`FnSignature`] value
    pub fn get_fn_signature(&self, from: &Eaten<FnSignature>) -> Option<Rc<Eaten<FnSignature>>> {
        self.collected.fn_signatures.get(&from.at).map(Rc::clone)
    }

    /// Get a specific function's body from its location
    /// Avoids cloning the entire (heavy) [`Eaten<Block>`]
    pub fn get_fn_body(&self, from: &Eaten<FunctionBody>) -> Option<Rc<Eaten<FunctionBody>>> {
        self.collected.fn_bodies.get(&from.at).map(Rc::clone)
    }

    /// Get a specific command alias' content from its location
    /// Avoids cloning the entire (heavy) [`Eaten<SingleCmdCall>`]
    pub fn get_cmd_alias_content(
        &self,
        from: &Eaten<SingleCmdCall>,
    ) -> Option<Rc<Eaten<SingleCmdCall>>> {
        self.collected.cmd_aliases.get(&from.at).map(Rc::clone)
    }

    /// Get a specific command alias' content from a developed version
    /// Avoids cloning the entire (heavy) [`Eaten<SingleCmdCall>`]
    pub fn get_developed_cmd_alias_content(
        &self,
        from: &DevelopedCmdAliasCall,
    ) -> Rc<Eaten<SingleCmdCall>> {
        match self.collected.cmd_aliases.get(&from.content_at) {
            Some(developed) => Rc::clone(developed),

            None => self.panic(
                from.content_at,
                "command alias is missing (= bug in checker)",
            ),
        }
    }

    /// Get a specific command call's informations
    /// Avoids cloning the entire (potentially heavy) [`DevelopedSingleCmdCall`]
    pub fn get_developed_cmd_call(
        &self,
        from: &Eaten<SingleCmdCall>,
    ) -> Rc<DevelopedSingleCmdCall> {
        match self.collected.cmd_calls.get(&from.at) {
            Some(developed) => Rc::clone(developed),

            None => self.panic(
                from.at,
                "developed command call data is missing (= bug in checker)",
            ),
        }
    }

    /// Get a specific command call used as a value
    /// Avoids cloning the entire (heavy) [`Eaten<CmdCall>`]
    pub fn get_cmd_call_used_as_value(&self, at: CodeRange) -> Rc<Eaten<CmdCall>> {
        match self.collected.cmd_call_values.get(&at) {
            Some(cmd_call) => Rc::clone(cmd_call),

            None => self.panic(at, "command call data is missing (= bug in checker)"),
        }
    }

    /// Get (ideally cached) variable name for the given long flag
    pub fn get_long_flag_var_name(&mut self, from: &RuntimeEaten<String>) -> String {
        self.long_flags_var_name
            .entry(from.data().clone())
            .or_insert_with(|| long_flag_var_name(from.data()))
            .clone()
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

        let deps_list = self.collected.deps.get(&body_content_at).unwrap_or_else(||
            self.panic(body_content_at, "dependencies informations not found while constructing value (this is a bug in the checker)")
        );

        for dep in deps_list {
            let Dependency {
                name,
                declared_name_at: declared_at,
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
                                    RuntimeCodeRange::Parsed(var_name_at) => var_name_at == *declared_at,
                                    RuntimeCodeRange::Internal => false,
                                })
                        })
                        .unwrap_or_else(|| self.panic(
                            body_content_at,
                            format!(
                                "cannot find variable to capture (= bug in checker) '{name}' (declared at {})",
                                dbg_loc(*declared_at, &self.conf.files_map)
                            )
                        ));

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
                                    RuntimeCodeRange::Parsed(func_name_at) => func_name_at == *declared_at,
                                    RuntimeCodeRange::Internal => false,
                                })
                        })
                        .unwrap_or_else(|| self.panic(
                            body_content_at,
                            format!(
                                "cannot find function to capture (= bug in checker) '{name}' (declared at {})",
                                dbg_loc(*declared_at, &self.conf.files_map)
                            )
                        ));

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
                        .unwrap_or_else(|| self.panic(
                            body_content_at,
                            format!(
                                "cannot find command alias to capture (= bug in checker) '{name}' (declared at {})",
                                dbg_loc(*declared_at, &self.conf.files_map)
                            )
                        ));

                    captured_deps
                        .cmd_aliases
                        .insert(dep.clone(), cmd_alias.clone());
                }
            };
        }

        captured_deps
    }

    /// Trigger a directory jump event
    pub fn trigger_directory_jump_event(&mut self, at: RuntimeCodeRange) -> ExecResult<()> {
        (self.conf.on_dir_jump)(self, at)
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

    /// Generate a [`CheckerOutput`] for running a new program
    pub fn generate_checker_output(&self) -> CheckerOutput {
        self.collected.reuse_in_checker()
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

/// Context creation parameters
pub struct ContextCreationParams {
    /// Runtime configuration
    pub runtime_conf: RuntimeConf,

    /// Function to check if Ctrl+C was pressed
    /// Indicator must be reset before the boolean is actually returned
    pub take_ctrl_c_indicator: fn() -> bool,

    /// Callback triggered when the current directory is changed by the shell
    pub on_dir_jump: fn(&mut Context, RuntimeCodeRange) -> ExecResult<()>,

    /// Map of all files, used for reporting
    pub files_map: FilesMap,

    /// Path to the current user's home directory
    /// Used for tilde '~' expansion
    pub home_dir: Option<PathBuf>,
}

impl Scope {
    /// Get this scope's file ID
    pub fn in_file_id(&self) -> Option<FileId> {
        match self.range {
            RuntimeCodeRange::Internal => None,
            RuntimeCodeRange::Parsed(range) => Some(range.start.file_id),
        }
    }

    /// Get this scope's source file ID
    pub fn source_file_id(&self) -> Option<SourceFileID> {
        match self.range {
            RuntimeCodeRange::Internal => None,
            RuntimeCodeRange::Parsed(range) => match range.start.file_id {
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
    /// It is backed by a [`GcReadOnlyCell`] in order to avoid needless cloning
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
