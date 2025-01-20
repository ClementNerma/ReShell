use std::path::PathBuf;

use clap::Parser;

/// Command line arguments of the shell's binary
#[derive(Parser)]
#[clap(name = "ReShell", version, about = "ReShell, a modern shell program")]
pub struct Args {
    #[clap(help = "Execute a script file", conflicts_with = "eval")]
    pub exec_file: Option<PathBuf>,

    #[clap(
        short = 'c',
        long,
        help = "Run a command and exit",
        conflicts_with = "exec_file"
    )]
    pub eval: Option<String>,

    #[clap(
        long,
        help = "Skip the initialization script file (in REPL mode)",
        conflicts_with = "exec_file",
        conflicts_with = "eval"
    )]
    pub skip_init_script: bool,

    #[clap(short, long, help = "Display timings")]
    pub timings: bool,

    #[clap(flatten)]
    pub exec_args: ExecArgs,

    #[clap(flatten)]
    pub runtime_conf_args: RuntimeConfArgs,
}

#[derive(clap::Args, Clone, Copy)]
pub struct ExecArgs {
    #[clap(long, help = "Print the AST")]
    pub print_ast: bool,

    #[clap(long, help = "Don't actually execute the program")]
    pub only_check: bool,
}

#[derive(clap::Args, Clone)]
pub struct RuntimeConfArgs {
    #[clap(long, help = "Maximum call stack size")]
    pub call_stack_limit: Option<usize>,

    #[clap(long, help = "Disable history")]
    pub disable_history: bool,

    #[clap(
        long,
        help = "Use a custom history file path",
        conflicts_with = "disable_history"
    )]
    pub custom_history_path: Option<PathBuf>,
}
