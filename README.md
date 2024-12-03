# The ReShell Project

> [!WARNING]
> This project is under heavy development. The syntax, builtin library and REPL features are constantly being updated and may encounter breaking changes without notice. **Please use this project at your own risks!**

The ReShell project aims to provide a complete alternative to popular Linux shells such as the *sh* family (sh, bash, zsh, ksh, ...), as well as Windows' PowerShell and Batch (COMMAND.COM).

It is designed to be as easy to use as possible, and feature many functions that are not available in most shells: functions returning values, lambdas, optional typechecking, nested arrays, complete error handling, etc.

Everything is designed to work out-of-the-box: completion, syntax highlighting, history handling, etc.

Compared to [NuShell](https://www.nushell.sh/), the latter is designed as an alternative way to interact with data. As it is mostly based on tables and functional data processing, it may be more suited to your needs depending on what you want. ReShell's scripting language provides some functional programming features as well, but remains an imperative language at core.

## Tutorila

You can find a guide to learn about the language [here](docs/Tutorial.md).

## Features

* Fits in a single binary with zero dependency
* Cross-platform (currently tested on Linux and Windows)
* Powerful scripting language
* Exhaustive native library
* Complete REPL with prompt customization, pretty error handling, syntax highlighting and completions

## Limitations

* Typing is dynamic, meaning there isn't optional ahead-of-time typechecking like NuShell
* Strings are always valid UTF-8, which brings lots of advantages but will make dealing with some non-UTF-8 filenames more complicated or straight up impossible

## Installing

Start by [installing Rust](https://rustup.rs/). Then, run:

```shell
cargo install --git https://github.com/ClementNerma/ReShell
```

This will install a `reshell` binary globally in your system.

You can also build from scratch:

```shell
# Clone the repository
git clone https://github.com/ClementNerma/ReShell

# Build with optimizations
cargo build --release

# Run the produced binary
./target/release/reshell
```

## Code sample

```
# Compute Fibonacci sequence
# Note that the type annotations are purely optional

fn fib(n: int) -> int {
    return if $n <= 1 {
        $n
    } else {
        fib($n - 1) + fib($n - 2)
    }
}

fib 3 # Prints: 2
fib 7 # Prints: 13
```

## Code structure

The code is divided into several crates:

* [`parser`](crates/parser): complete parser based on [Parsy's combinators](https://github.com/ClementNerma/Parsy)
* [`checker`](crates/checker): performs code checking and collect data to accelerate runtime
* [`runtime`](crates/runtime): executes a parsed and checked program
* [`builtins`](crates/builtins): contains all of the native library
* [`repl`](crates/repl): interactive REPL with support for scope re-use
