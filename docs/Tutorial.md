# Learning ReShell

> [!WARNING]
> **This tutorial is still being written and is incomplete as of now.**

Welcome to ReShell's tutorial! In this guide, you will learn how to use the shell and its programming language.

Everything you need to master it is written in this document, so take your time and enjoy!

## Table of contents

- [Table of contents](#table-of-contents)
- [Installing](#installing)
- [Differences with Unix-like shells (Bash, ...)](#differences-with-unix-like-shells-bash-)
- [The basics](#the-basics)
  - [Writing commands](#writing-commands)
  - [Variables](#variables)
  - [User input](#user-input)
  - [String interpolation](#string-interpolation)

## Installing

The easiest way to install it currently it to download the [latest release](https://github.com/ClementNerma/ReShell/releases/latest). ReShell fits in a single binary, and it's available on Linux and Windows (macOS support is planned).

## Differences with Unix-like shells (Bash, ...)

Unlike traditional Unix-like shells like Bash or ZSH, ReShell is heavily geared towards scripting. What this means is, while basic commands will look more or less the same than in Bash, you will get a lot more features out of ReShell.

There are too many differences to count, but will you discover them along this tutorial.

## The basics

### Writing commands

A command is made of two things: the command's _path_, and its _arguments_.

```shell
echo Hello!
```

The command's path (or name) is `echo`. This is a command that displays the argument you throw at it, which here is `Hello!`.

If you run it, it will display `Hello!` in your terminal.

Now let's add a little bit of color:

```shell
echo Hello! -c yellow
```

This will print the same message, but in yellow this time. We have three arguments: `Hello!`, `-c` and `yellow`. Arguments starting with a dash are called _flags_, and the argument that follows them is usually their _value_. Here, it's the color's name.

There are lots of available commands, to see them all, start from an empty prompt and using the `<TAB>` key to list all of them (warning: there may be thousands of them).

If you try to write a command that doesn't exist, like `echoo Hello!`, the command's name will be highlighted in red instead of blue. This is to show you're trying to use an unknown command.

### Variables

Sometimes we need to store informations, and we do that by using _variables_:

```shell
# Declare a variable called 'name'
let message = 'Hello!';

# Display its content
echo $name
```

What we write after the `=` assignment operator is called the variable's _value_. Normally, strings are wrapped into quotes. For simplicity reasons, when you write commands, you don't need to put these quotes if they don't contain special characters like spaces. But when using the values anywhere else, you need to use quotes.

This means that `echo 'Hello!'` is functionally equivalent to `echo Hello!`.

### User input

We can ask for input from the user using the builtin `ask` _command_:

```shell
ask 'Please enter your name: '
```

Enter your name, and it will print your answer again. This is because `ask` is actually a _function_, and functions can return values. We'll learn more about them later. For now, we just need a way to get the result and put it in a variable:

```shell
let name = ask('Please enter your name:')

echo $name
```

Why did we use parenthesis here? Because traditionally, functions are called that way. For simplicity reasons, when we call them as commands, we can omit them. But we _could_ technically always use parenthesis if we wanted to.

### String interpolation

Sometimes we want to use a variable inside a string. This is called _interpolation_, and uses double quotes:

```shell
let name = ask('Please enter your name:')

echo "Your name is: $name"
```
