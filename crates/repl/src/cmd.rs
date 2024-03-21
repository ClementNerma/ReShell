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
    // #[clap(long, help = "Only perform code checking", requires = "eval")]
    // pub check_only: bool,

    // #[clap(long, help = "Debug code checking informations", requires = "eval")]
    // pub dbg_code_checking: bool,
    #[clap(long, help = "Skip the initialization script file")]
    pub skip_init_script: bool,
}
