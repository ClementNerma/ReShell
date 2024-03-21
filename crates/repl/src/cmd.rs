use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
pub struct Args {
    #[clap(help = "Execute a script file")]
    pub exec_file: Option<PathBuf>,

    #[clap(
        short,
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
}
