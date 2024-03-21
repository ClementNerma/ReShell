# The ReShell Project

**WARNING:** This project is under heavy development and many things are either non-functional or may produce unexpected results. **Please use at your own risks!**

The ReShell project aims to provide a complete alternative to popular shells such as *sh (sh, bash, zsh, ksh, ...), PowerShell or Batch.

It is designed to be as easy to use as possible, and feature many functions that are not available in most shells: functions returning values, lambdas, optional typechecking, nested arrays, complete error handling, etc.

_Versus NuShell:_ NuShell is a powerful shell designed as an alternative way to interact with your system. As it is mostly based on specific data types such as tables and more towards functional programming, you may prefer it over ReShell depending on your needs and tastes.

## Project's content

* A complete shell that fits in a single binary
* Multi-platform compatibility: Linux, Windows and macOS as first-class citizens
* A powerful scripting language
* A complete native library
* A REPL
* Prompt customization
* Pretty error handling

## Code sample

```
# Compute Fibonacci sequence
# NOTE: type annotations are not required

fn fibo(n: int) -> int {
    return if n <= 1 {
        n
    } else {
        fib(n - 1) + fib(n - 2)
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
