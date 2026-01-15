use std::{collections::HashMap, ffi::OsString, num::NonZero, path::PathBuf, sync::Arc};

use indexmap::IndexSet;
use parsy::{CodeRange, Span};
use reshell_checker::{
    CheckerError, CheckerScope, DeclaredCmdAlias, DeclaredFn, DeclaredMethod, DeclaredVar,
    TypeAliasStore,
    output::{
        CheckerOutput, Dependency, DependencyType, DevelopedCmdAliasCall, DevelopedSingleCmdCall,
    },
};
use reshell_parser::{
    NATIVE_LIB_AST_SCOPE_ID,
    ast::{
        AstScopeId, Block, CmdCall, FnSignature, Program, RuntimeCodeRange, SingleCmdCall,
        ValueType,
    },
    files_map::FilesMap,
};
use reshell_prettify::{PrettyPrintOptions, PrettyPrintable};

use crate::{
    bin_resolver::BinariesResolver,
    conf::RuntimeConf,
    errors::{
        ExecActualError, ExecActualErrorNature, ExecError, ExecInfoType, ExecResult,
        ExecTopPropagation,
    },
    gc::GcCell,
    pretty_impl::pretty_printable_runtime_code_range,
    typechecking::check_if_value_fits_type,
    values::{LocatedValue, RuntimeCmdAlias, RuntimeFnValue, RuntimeValue},
};

/// Scope ID of the native library
pub static NATIVE_LIB_SCOPE_ID: u64 = 0;

/// Scope ID of the very first user scope (which is the only scope to never be deleted)
pub static FIRST_SCOPE_ID: u64 = 1;

/// This structure represents the state of the runtime.
///
/// It contains runtime configuration as well as real-time runtime data
///
///  It is designed to be reusable in order to run multiple programs in the same base scope
/// (e.g. REPL scenario)
#[derive(Clone)]
pub struct Context {
    /// Context configuration
    conf: ContextCreationParams,

    /// Binaries resolver
    bin_resolver: BinariesResolver,

    /// Auto-incremented scopes ID counter
    ///
    /// When a counter is created, this is increased and assigned to the new scope
    scopes_id_counter: u64,

    /// All alive scopes
    ///
    /// When a scope ends, it is removed from this map,
    /// except for the very first user scope as well as the native library
    scopes: HashMap<u64, Scope>,

    /// ID of the current scope
    current_scope: u64,

    /// ID of the current program's main scope
    program_main_scope: Option<u64>,

    /// Data collected from the checker
    ///
    /// Whenever a new program is run, the new program's data is merged with the existing one
    collected: CheckerOutput,
}

impl Context {
    /// Create a new context (runtime state)
    /// The native library's content can be generated using the dedicated crate
    pub fn new(
        conf: ContextCreationParams,
        bin_resolver: BinariesResolver,
        native_lib_content: ScopeContent,
    ) -> Self {
        let scopes_to_add = [Scope {
            id: NATIVE_LIB_SCOPE_ID,
            ast_scope_id: NATIVE_LIB_AST_SCOPE_ID,
            call_stack: CallStack::empty(),
            content: native_lib_content,
            parent_scopes: IndexSet::new(),
            previous_scope: None,
        }];

        let mut scopes = HashMap::new();

        for scope in scopes_to_add {
            scopes.insert(scope.id, scope);
        }

        Self {
            scopes_id_counter: NATIVE_LIB_SCOPE_ID,
            scopes,
            bin_resolver,
            current_scope: NATIVE_LIB_SCOPE_ID,
            program_main_scope: None,
            collected: CheckerOutput::empty(),
            conf,
        }
    }

    /// Generate a new scope ID
    fn generate_scope_id(&mut self) -> u64 {
        self.scopes_id_counter += 1;
        self.scopes_id_counter
    }

    /// Get the runtime configuration
    pub fn runtime_conf(&self) -> &RuntimeConf {
        &self.conf.runtime_conf
    }

    /// Clear Ctrl+C press indicator
    pub(crate) fn reset_ctrl_c_press_indicator(&self) {
        (self.conf.take_ctrl_c_indicator)();
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

    /// Mutable access the binaries resolver
    pub fn binaries_resolver(&mut self) -> &mut BinariesResolver {
        &mut self.bin_resolver
    }

    /// Get a reference to the type alias store
    pub fn type_alias_store(&self) -> &TypeAliasStore {
        &self.collected.type_aliases_usages
    }

    /// Get a reference to the collected checker's output
    pub fn checker_output(&self) -> &CheckerOutput {
        &self.collected
    }

    /// Trigger a directory jump event
    pub fn trigger_directory_jump_event(&mut self, at: RuntimeCodeRange) -> ExecResult<()> {
        (self.conf.on_dir_jump)(self, at)
    }

    /// Ensure Ctrl+C was not pressed
    /// Otherwise, return a Ctrl+C error
    pub(crate) fn ensure_no_ctrl_c_press(&self, at: impl Into<RuntimeCodeRange>) -> ExecResult<()> {
        if (self.conf.take_ctrl_c_indicator)() {
            Err(self.hard_error(at.into(), ExecActualErrorNature::CtrlC))
        } else {
            Ok(())
        }
    }

    /// Get the list of all scopes visible by a given one
    /// Iterating in visibility order
    fn visible_scopes_for<'a, 'b: 'a>(
        &'a self,
        scope: &'b Scope,
    ) -> impl DoubleEndedIterator<Item = &'a Scope> {
        scope
            .parent_scopes
            // Iterate over all parent scopes
            .iter()
            // Remove scopes that are already dropped (= not referenced anymore)
            .filter_map(|scope_id| self.scopes.get(scope_id))
            // Add the current scope
            .chain([scope])
            // Latest scopes in history are the first one to see
            .rev()
    }

    /// Get the list of all scopes visible by the current one
    /// Iterating in visibility order
    pub fn visible_scopes(&self) -> impl DoubleEndedIterator<Item = &Scope> {
        self.visible_scopes_for(self.current_scope())
    }

    /// Get the  list of all scopes' content visible by the current one
    /// Iterating in visibility order
    pub fn visible_scopes_content(&self) -> impl DoubleEndedIterator<Item = &ScopeContent> {
        self.visible_scopes().map(|scope| &scope.content)
    }

    /// Generate checker scopes from current runtime scope hierarchy
    pub fn generate_checker_scopes(&self) -> Vec<CheckerScope> {
        self.generate_parent_scopes_list()
            .iter()
            .filter_map(|scope_id| self.scopes.get(scope_id))
            .map(|scope| {
                let Scope {
                    content,
                    ast_scope_id,
                    ..
                } = &scope;

                let ScopeContent {
                    vars,
                    fns,
                    methods,
                    cmd_aliases,
                } = &content;

                CheckerScope {
                    id: *ast_scope_id,

                    special_scope_type: None,

                    cmd_aliases: cmd_aliases
                        .iter()
                        .map(|(key, value)| {
                            (
                                key.clone(),
                                DeclaredCmdAlias {
                                    decl_at: value.name_at,
                                    scope_id: value.decl_scope_id,
                                    content_at: value.value.content.at,
                                    is_ready: true,
                                },
                            )
                        })
                        .collect(),

                    type_aliases: self
                        .collected
                        .type_aliases_decl_by_scope
                        .get(ast_scope_id)
                        .map(|map| {
                            map.iter()
                                .map(|(key, value)| (key.clone(), value.at))
                                .collect()
                        })
                        .unwrap_or_default(),

                    fns: fns
                        .iter()
                        .map(|(name, func)| {
                            (
                                name.clone(),
                                DeclaredFn {
                                    decl_at: func.name_at,
                                    scope_id: func.decl_scope_id,
                                },
                            )
                        })
                        .collect(),

                    methods: methods
                        .iter()
                        .map(|(name, with_types)| {
                            (
                                name.to_owned(),
                                with_types
                                    .iter()
                                    .map(|method| {
                                        (
                                            Arc::clone(&method.on_type),
                                            DeclaredMethod {
                                                decl_at: method.name_at,
                                                scope_id: method.decl_scope_id,
                                            },
                                        )
                                    })
                                    .collect(),
                            )
                        })
                        .collect(),

                    vars: vars
                        .iter()
                        .map(|(name, var)| {
                            (
                                name.clone(),
                                DeclaredVar {
                                    decl_at: var.name_at,
                                    scope_id: var.decl_scope_id,
                                    is_mut: var.is_mut,
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
    pub(crate) fn prepare_for_new_program(
        &mut self,
        program: &Span<Program>,
    ) -> Result<(), CheckerError> {
        // Check the program
        reshell_checker::check(
            &program.data,
            self.generate_checker_scopes(),
            &mut self.collected,
        )?;

        // Create a new scope for this program
        let scope_id =
            self.create_and_push_scope(program.data.content.data.scope_id, ScopeContent::new());

        // Reset scope
        self.current_scope = scope_id;

        // Remember the program's main scope
        self.program_main_scope = Some(scope_id);

        Ok(())
    }

    /// (Internal) Reset to the program's main scope after execution
    pub(crate) fn reset_to_program_main_scope(&mut self) {
        let scope_id = self.program_main_scope.unwrap();
        assert!(self.scopes.contains_key(&scope_id));

        self.current_scope = scope_id;
        // self.program_main_scope = None;
    }

    /// Generate a "hard" (unrecoverable) error information
    pub fn hard_error(
        &self,
        at: impl Into<RuntimeCodeRange>,
        nature: impl Into<ExecActualErrorNature>,
    ) -> ExecError {
        let current_scope = self.current_scope();

        ExecError::ActualError(Box::new(ExecActualError {
            at: at.into(),
            nature: nature.into(),
            call_stack: current_scope.call_stack.clone(),
            infos: vec![],
        }))
    }

    /// Generate an error object and attach additional informations to it
    pub fn hard_error_with_infos<const N: usize>(
        &self,
        at: impl Into<RuntimeCodeRange>,
        nature: impl Into<ExecActualErrorNature>,
        infos: [(ExecInfoType, impl Into<String>); N],
    ) -> ExecError {
        let current_scope = self.current_scope();

        ExecError::ActualError(Box::new(ExecActualError {
            at: at.into(),
            nature: nature.into(),
            call_stack: current_scope.call_stack.clone(),
            infos: infos
                .into_iter()
                .map(|(info_type, msg)| (info_type, msg.into()))
                .collect(),
        }))
    }

    /// Generate an error object and attach additional informations to it
    pub fn hard_error_with(
        &self,
        at: impl Into<RuntimeCodeRange>,
        nature: impl Into<ExecActualErrorNature>,
        with: impl FnOnce(&mut ExecActualError),
    ) -> ExecError {
        let current_scope = self.current_scope();

        let mut err = ExecActualError {
            at: at.into(),
            nature: nature.into(),
            call_stack: current_scope.call_stack.clone(),
            infos: vec![],
        };

        with(&mut err);

        ExecError::ActualError(Box::new(err))
    }

    /// Generate a "successful exit" value
    pub fn successful_exit(&self) -> ExecError {
        ExecError::TopPropagation(ExecTopPropagation::SuccessfulExit)
    }

    /// Generate a "failure exit" value
    pub fn failure_exit(&self, code: NonZero<u8>) -> ExecError {
        ExecError::TopPropagation(ExecTopPropagation::FailureExit { code })
    }

    /// Generate a throw error
    pub fn throw(
        &self,
        at: impl Into<RuntimeCodeRange> + Copy,
        message: impl Into<String>,
    ) -> ExecError {
        self.hard_error(
            at,
            ExecActualErrorNature::Thrown {
                at: at.into(),
                message: message.into(),
            },
        )
    }

    /// Generate a throw error, with additional informations
    pub fn throw_with_infos<const N: usize>(
        &self,
        at: impl Into<RuntimeCodeRange> + Copy,
        message: impl Into<String>,
        infos: [(ExecInfoType, impl Into<String>); N],
    ) -> ExecError {
        self.hard_error_with_infos(
            at,
            ExecActualErrorNature::Thrown {
                at: at.into(),
                message: message.into(),
            },
            infos,
        )
    }

    /// Panic because of an internal error
    pub fn panic(&self, at: impl Into<RuntimeCodeRange>, message: impl AsRef<str>) -> ! {
        panic!(
            "\n| An internal error occured.\n| This is not supposed to happen and is the result of a bug in the shell itself (not your program).\n|\n| Details : {}\n| Location: {}\n",
            message.as_ref(),
            pretty_printable_runtime_code_range(at.into(), self.files_map())
                .display(PrettyPrintOptions::inline())
                .no_colors()
        );
    }

    /// Create and push a new scope above the current one
    /// // TODO: factorize
    pub(crate) fn create_and_push_scope(
        &mut self,
        ast_scope_id: AstScopeId,
        content: ScopeContent,
    ) -> u64 {
        let id = self.generate_scope_id();

        let scope = Scope {
            id,
            ast_scope_id,
            parent_scopes: self.generate_parent_scopes_list(),
            content,
            call_stack: self.current_scope().call_stack.clone(),
            previous_scope: Some(self.current_scope),
        };

        self.push_scope(scope);

        id
    }

    /// Create and push a new scope with initialized scope above the current one
    pub(crate) fn create_and_push_scope_detailed(
        &mut self,
        ast_scope_id: AstScopeId,
        content: ScopeContent,
        parent_scopes: IndexSet<u64>,
        call_stack_entry: Option<CallStackEntry>,
    ) {
        // Update the call stack if necessary
        let mut call_stack = self.current_scope().call_stack.clone();

        if let Some(call_stack_entry) = call_stack_entry {
            call_stack.append(call_stack_entry);
        }

        // Create the new scope
        let scope = Scope {
            id: self.generate_scope_id(),
            ast_scope_id,
            parent_scopes,
            content,
            call_stack,
            previous_scope: Some(self.current_scope),
        };

        self.push_scope(scope);
    }

    /// Push an already-created scope above the current one
    pub(crate) fn push_scope(&mut self, scope: Scope) {
        if scope.call_stack.history.len() > self.conf.runtime_conf.call_stack_limit {
            panic!(
                "Maximum call stack size ({}) exceeded",
                self.conf.runtime_conf.call_stack_limit
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
        // assert!(self.current_scope > FIRST_SCOPE_ID);
        assert!(self.current_scope > self.program_main_scope.unwrap());

        let current_scope = self.scopes.remove(&self.current_scope).unwrap();

        // assert!(ENSURE scopes with stack entry (= fn calls) have a deps scope);

        self.current_scope = current_scope.previous_scope.unwrap();

        assert!(self.scopes.contains_key(&self.current_scope));
    }

    /// Get a specific function
    /// It is guaranteed to be the one referenced at that point in time
    /// as the scopes building ensures this will automatically return the correct one
    pub(crate) fn get_visible_fn<'s>(&'s self, name: &Span<String>) -> Option<&'s ScopeFn> {
        self.visible_scopes_content()
            .find_map(|scope| scope.fns.get(&name.data))
    }

    /// Get the value of a specific function
    /// It is guaranteed to be the one referenced at that point in time
    /// as the scopes building ensures this will automatically return the correct one
    pub(crate) fn get_visible_fn_value<'s>(
        &'s self,
        name: &Span<String>,
    ) -> ExecResult<&'s Arc<RuntimeFnValue>> {
        let Some(func) = self.get_visible_fn(name) else {
            self.panic(name.at, "function not found");
        };

        Ok(&func.value)
    }

    /// Get all methods with a given name
    /// It is guaranteed to be the one referenced at that point in time
    /// as the scopes building ensures this will automatically return the correct one
    pub(crate) fn find_applicable_method<'s>(
        &'s self,
        name: &Span<String>,
        for_value: &RuntimeValue,
    ) -> Result<&'s ScopeMethod, Vec<&'s ScopeMethod>> {
        let mut not_matching = vec![];

        for method in self
            .visible_scopes_content()
            .filter_map(|scope| scope.methods.get(&name.data))
            .flatten()
        {
            if check_if_value_fits_type(for_value, &method.on_type, self) {
                return Ok(method);
            }

            not_matching.push(method);
        }

        Err(not_matching)
    }

    /// Get a specific variable
    /// It is guaranteed to be the one referenced at that point in time
    /// as the scopes building ensures this will automatically return the correct one
    pub(crate) fn get_visible_var<'c>(&'c self, name: &Span<String>) -> &'c ScopeVar {
        self.visible_scopes_content()
            .find_map(|scope| scope.vars.get(&name.data))
            .unwrap_or_else(|| self.panic(name.at, "variable was not found"))
    }

    /// Get a specific type alias
    /// It is guaranteed to be the one referenced at that point in time
    /// as type alias usages are collected before runtime
    pub(crate) fn get_type_alias<'c>(&'c self, name: &Span<String>) -> &'c Span<ValueType> {
        self.collected
            .type_aliases_usages
            .get(name)
            .unwrap_or_else(|| {
                self.panic(
                    name.at,
                    format!(
                        "type alias '{}' was not found (this is a bug in the checker)",
                        name.data
                    ),
                );
            })
    }

    /// Get a specific type signature from its location
    /// Avoids cloning the entire (heavy) [`FnSignature`] value
    pub(crate) fn get_fn_signature(&self, from: &Span<FnSignature>) -> Arc<Span<FnSignature>> {
        match self.collected.fn_signatures.get(&from.at) {
            Some(signature) => Arc::clone(signature),
            None => self.panic(from.at, "function signature is not registered"),
        }
    }

    /// Get a specific function's body from its location
    /// Avoids cloning the entire (heavy) [`Span<Block>`]
    pub fn get_fn_body(&self, from: &Span<Block>) -> Arc<Span<Block>> {
        match self.collected.fn_bodies.get(&from.at) {
            Some(fn_body) => Arc::clone(fn_body),
            None => self.panic(from.at, "function body is not registered"),
        }
    }

    /// Get a specific command alias' content from its location
    /// Avoids cloning the entire (heavy) [`Span<SingleCmdCall>`]
    pub(crate) fn get_cmd_alias_content(
        &self,
        from: &Span<SingleCmdCall>,
    ) -> Arc<Span<SingleCmdCall>> {
        match self.collected.cmd_aliases.get(&from.at) {
            Some(cmd_alias) => Arc::clone(cmd_alias),
            None => self.panic(from.at, "command alias content is not registered"),
        }
    }

    /// Get a specific command alias' content from a developed version
    /// Avoids cloning the entire (heavy) [`Span<SingleCmdCall>`]
    pub(crate) fn get_developed_cmd_alias_content(
        &self,
        from: &DevelopedCmdAliasCall,
    ) -> Arc<Span<SingleCmdCall>> {
        match self.collected.cmd_aliases.get(&from.alias_content_at) {
            Some(developed) => Arc::clone(developed),
            None => self.panic(from.alias_content_at, "command alias is missing"),
        }
    }

    /// Get a specific command call's informations
    /// Avoids cloning the entire (potentially heavy) [`DevelopedSingleCmdCall`]
    pub(crate) fn get_developed_cmd_call(
        &self,
        from: &Span<SingleCmdCall>,
    ) -> Arc<DevelopedSingleCmdCall> {
        match self.collected.cmd_calls.get(&from.at) {
            Some(developed) => Arc::clone(developed),
            None => self.panic(from.at, "developed command call data is missing"),
        }
    }

    /// Get a specific command call used as a value
    /// Avoids cloning the entire (heavy) [`Span<CmdCall>`]
    pub fn get_cmd_call_used_as_value(&self, at: CodeRange) -> Arc<Span<CmdCall>> {
        match self.collected.cmd_call_values.get(&at) {
            Some(cmd_call) => Arc::clone(cmd_call),
            None => self.panic(at, "command call data is missing"),
        }
    }

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
    pub(crate) fn capture_deps(
        &self,
        body_content_at: CodeRange,
        body_ast_scope_id: AstScopeId,
    ) -> ScopeContent {
        let mut captured_deps = ScopeContent::new();

        let deps_list = self.collected.deps.get(&body_ast_scope_id).unwrap_or_else(||
            self.panic(body_content_at, "dependencies informations not found while constructing value (this is a bug in the checker)")
        );

        for dep in deps_list {
            let Dependency {
                name,
                declared_in,
                dep_type,
            } = dep;

            let Some(decl_scope) = self
                .visible_scopes()
                .find(|scope| scope.ast_scope_id == *declared_in)
            else {
                self.panic(
                    body_content_at,
                    format!("cannot find scope containing {dep_type} '{name}' to capture"),
                );
            };

            match dep_type {
                DependencyType::Variable => {
                    let var = decl_scope.content.vars.get(name).unwrap_or_else(|| {
                        self.panic(
                            body_content_at,
                            format!("cannot find variable '{name}' to capture"),
                        )
                    });

                    captured_deps.vars.insert(dep.name.clone(), var.clone());
                }

                DependencyType::Function => {
                    let func = decl_scope.content.fns.get(name).unwrap_or_else(|| {
                        self.panic(
                            body_content_at,
                            format!("cannot find function '{name}' to capture"),
                        )
                    });

                    captured_deps.fns.insert(dep.name.clone(), func.clone());
                }

                DependencyType::Method => {
                    for scope in self.visible_scopes_for(decl_scope) {
                        for (method_name, methods) in scope.content.methods.iter() {
                            for method in methods {
                                if name == method_name {
                                    match method.name_at {
                                        RuntimeCodeRange::Internal(_) => unreachable!(),
                                        RuntimeCodeRange::Parsed(_) => {
                                            captured_deps
                                                .methods
                                                .entry(name.clone())
                                                .or_default()
                                                .push(method.clone());
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                DependencyType::CmdAlias => {
                    let cmd_alias = decl_scope.content.cmd_aliases.get(name).unwrap_or_else(|| {
                        self.panic(
                            body_content_at,
                            format!("cannot find command alias '{name}' to capture"),
                        )
                    });

                    captured_deps
                        .cmd_aliases
                        .insert(name.clone(), cmd_alias.clone());
                }
            };
        }

        captured_deps
    }
}

/// Runtime scope
#[derive(Debug, Clone)]
pub struct Scope {
    /// Unique ID of the scope (not two scopes must have the same ID)
    pub id: u64,

    /// AST scope ID
    pub ast_scope_id: AstScopeId,

    /// Content of the scope
    pub content: ScopeContent,

    /// List of parent scopes, from farthest to the nearest
    pub parent_scopes: IndexSet<u64>,

    /// Previous scope
    /// Used e.g. when calling a function, the function's body will
    /// get a new scope but we must return to the callee's scope afterwards
    pub previous_scope: Option<u64>,

    /// Entire call stack
    pub call_stack: CallStack,
}

/// Context creation parameters
#[derive(Clone)]
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

    /// Shell arguments
    pub script_args: Vec<OsString>,
}

/// Content of a scope
#[derive(Debug, Clone)]
pub struct ScopeContent {
    /// Variables (map keys are variable names)
    pub vars: HashMap<String, ScopeVar>,

    /// Functions (map keys are function names)
    pub fns: HashMap<String, ScopeFn>,

    /// Methods (map keys are a combinator of method name + appliable type)
    pub methods: HashMap<String, Vec<ScopeMethod>>,

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
            methods: HashMap::new(),
            cmd_aliases: HashMap::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        let Self {
            vars,
            fns,
            methods,
            cmd_aliases,
        } = &self;

        vars.is_empty() && fns.is_empty() && methods.is_empty() && cmd_aliases.is_empty()
    }

    pub fn extend(&mut self, other: ScopeContent) -> Result<(), String> {
        let ScopeContent {
            vars,
            fns,
            methods,
            cmd_aliases,
        } = other;

        for var_name in vars.keys() {
            if self.vars.contains_key(var_name) {
                return Err(format!(
                    "Variable '{var_name}' exists in both base scope and to-merge scope"
                ));
            }
        }

        for fn_name in fns.keys() {
            if self.fns.contains_key(fn_name) {
                return Err(format!(
                    "Function '{fn_name}' exists in both base scope and to-merge scope"
                ));
            }
        }

        for method_name in methods.keys() {
            if self.methods.contains_key(method_name) {
                return Err(format!(
                    "Method '{method_name}' exists in both base scope and to-merge scope"
                ));
            }
        }

        for cmd_alias_name in cmd_aliases.keys() {
            if self.cmd_aliases.contains_key(cmd_alias_name) {
                return Err(format!(
                    "Command alias '{cmd_alias_name}' exists in both base scope and to-merge scope"
                ));
            }
        }

        self.vars.extend(vars);
        self.fns.extend(fns);
        self.methods.extend(methods);
        self.cmd_aliases.extend(cmd_aliases);

        Ok(())
    }
}

/// Scoped variable
#[derive(Debug, Clone)]
pub struct ScopeVar {
    /// Name declaration location
    pub name_at: RuntimeCodeRange,

    /// Declaration scope ID
    pub decl_scope_id: AstScopeId,

    /// Is the variable mutable?
    pub is_mut: bool,

    /// Enforced type
    pub enforced_type: Option<ValueType>,

    /// Value of the variable
    ///
    /// It is backed by a [`GcCell`] in order to be sharable in captured dependencies
    ///
    /// See [`Context::capture_deps`]
    pub value: GcCell<LocatedValue>,
}

/// Scoped function
#[derive(Debug, Clone)]
pub struct ScopeFn {
    /// Name declaration location
    pub name_at: RuntimeCodeRange,

    /// Declaration scope ID
    pub decl_scope_id: AstScopeId,

    /// Value of the function
    ///
    /// It is backed by a [`Arc`] in order to avoid needless cloning
    pub value: Arc<RuntimeFnValue>,
}

/// Scoped method
#[derive(Debug, Clone)]
pub struct ScopeMethod {
    /// Name declaration location
    pub name_at: RuntimeCodeRange,

    /// Declaration scope ID
    pub decl_scope_id: AstScopeId,

    /// Type the method can be applied on
    ///
    /// It is backed by a [`Arc`] in order to avoid needless cloning
    pub on_type: Arc<ValueType>,

    /// Value of the method
    ///
    /// It is backed by a [`Arc`] in order to avoid needless cloning
    pub value: Arc<RuntimeFnValue>,
}

/// Scoped command alias
#[derive(Debug, Clone)]
pub struct ScopeCmdAlias {
    /// Declaration scope ID
    pub decl_scope_id: AstScopeId,

    /// Location of the alias' name in its declaration
    pub name_at: CodeRange,

    /// Content of the alias
    ///
    /// It is backed by an [`Arc`] in order to avoid needless cloning
    pub value: Arc<RuntimeCmdAlias>,
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
