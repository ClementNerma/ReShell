use std::path::PathBuf;

use clap::Parser;

/// Command line arguments of the shell's binary
#[derive(Parser)]
#[clap(
    name = "ReShell",
    version,
    about,
    about = "ReShell, a modern shell program"
)]
pub struct Args {
    #[clap(help = "Execute a script file")]
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
        help = "Skip the initialization script file",
        conflicts_with = "exec_file"
    )]
    pub skip_init_script: bool,

    #[clap(short, long, help = "Display timings")]
    pub timings: bool,

    #[clap(flatten)]
    pub exec_args: ExecArgs,
}

#[derive(clap::Args, Clone, Copy)]
pub struct ExecArgs {
    #[clap(long, help = "Print the AST")]
    pub print_ast: bool,

    #[clap(long, help = "Don't actually execute the program")]
    pub only_check: bool,
}
