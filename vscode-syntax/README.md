# reshell-syntax

A work-in-progress syntax highlighting extension for ReShell's scripting language.

# Project structure

The extension is built using TypeScript, source files can be found in the [`src/`](./src/) directory. The code acts as a thin wrapper around the actual syntax highlighter, which is built using Rust and compiled to WASM. Source files are located in the [`rust-wasm/`](./rust-wasm/) directory.

The WASM module embeds ReShell's syntax highlighting engine and converts it to a simplified representation for VSCode.
