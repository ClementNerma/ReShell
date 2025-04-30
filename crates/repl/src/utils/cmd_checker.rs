use std::{
    collections::{HashMap, HashSet},
    sync::{Arc, LazyLock, Mutex},
};

use reshell_runtime::{
    bin_resolver::BinariesResolver, cmd::try_replace_home_dir_tilde, context::Context,
};
use reshell_syntax_highlighter::CheckCmdType;

pub struct CommandsChecker {
    for_path: Vec<String>,
    methods_exist: HashSet<String>,
    functions_exist: HashSet<String>,
    aliases_exist: HashSet<String>,
    external_cmds_exist: HashMap<String, bool>,
}

impl CommandsChecker {
    pub fn new() -> Self {
        Self {
            for_path: vec![],
            methods_exist: HashSet::new(),
            functions_exist: HashSet::new(),
            aliases_exist: HashSet::new(),
            external_cmds_exist: HashMap::new(),
        }
    }

    pub fn update_path(&mut self, bin_resolver: &mut BinariesResolver) {
        if &self.for_path != bin_resolver.path_dirs() {
            self.for_path.clone_from(bin_resolver.path_dirs());
        }
    }

    pub fn refresh(&mut self, ctx: &mut Context) {
        self.functions_exist = ctx
            .visible_scopes_content()
            .flat_map(|scope| scope.fns.keys())
            .cloned()
            .collect();

        self.methods_exist = ctx
            .visible_scopes_content()
            .flat_map(|scope| scope.methods.keys())
            .cloned()
            .collect();

        self.aliases_exist = ctx
            .visible_scopes_content()
            .flat_map(|scope| scope.cmd_aliases.keys())
            .cloned()
            .collect();

        ctx.binaries_resolver().clear();

        self.external_cmds_exist.clear();
    }

    pub fn check(&mut self, ctx: &mut Context, name: &str, typ: CheckCmdType) -> bool {
        match typ {
            CheckCmdType::Method => self.methods_exist.contains(name),
            CheckCmdType::Function => self.functions_exist.contains(name),
            CheckCmdType::ExternalCmd | CheckCmdType::BroadCmd => {
                if matches!(typ, CheckCmdType::BroadCmd)
                    && (self.aliases_exist.contains(name) || self.functions_exist.contains(name))
                {
                    return true;
                }

                if self.external_cmds_exist.get(name) == Some(&true) {
                    return true;
                }

                self.update_path(ctx.binaries_resolver());

                // Check for actual file
                let Ok(name) = try_replace_home_dir_tilde(name, ctx) else {
                    return false;
                };

                let exists = ctx.binaries_resolver().resolve_binary_path(&name).is_ok();

                self.external_cmds_exist.insert(name.to_owned(), exists);
                exists
            }
        }
    }
}

pub static COMMANDS_CHECKER: LazyLock<Arc<Mutex<CommandsChecker>>> =
    LazyLock::new(|| Arc::new(Mutex::new(CommandsChecker::new())));
