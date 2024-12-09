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
  - [Chaining commands](#chaining-commands)
  - [Variables](#variables)
  - [User input](#user-input)
  - [String interpolation](#string-interpolation)
  - [External commands](#external-commands)
  - [Aliases](#aliases)
- [Scripting](#scripting)
  - [Conditionals](#conditionals)
  - [Loops](#loops)
  - [Matching](#matching)
  - [Splitting into multiple files](#splitting-into-multiple-files)
- [Functions](#functions)
  - [Fundamentals](#fundamentals)
  - [Returning values](#returning-values)
  - [Optional arguments](#optional-arguments)
  - [Flags](#flags)
  - [Presence flags](#presence-flags)
  - [Methods](#methods)
  - [Lambdas](#lambdas)
  - [Error handling](#error-handling)
- [Types](#types)
  - [Common types](#common-types)
  - [Debugging a value's type](#debugging-a-values-type)
  - [Typing variables](#typing-variables)
  - [Typing functions](#typing-functions)
  - [Structures](#structures)
  - [Lists](#lists)
  - [Maps](#maps)
  - [Type aliases](#type-aliases)
  - [Function signatures](#function-signatures)
  - [Sub-typing](#sub-typing)
  - [Union types](#union-types)
  - [Matching on types](#matching-on-types)
- [Advanced features](#advanced-features)
  - [Variable shadowing](#variable-shadowing)
  - [Destructuring](#destructuring)

## Installing

The easiest way to install it currently it to download the [latest release](https://github.com/ClementNerma/ReShell/releases/latest). ReShell fits in a single binary, and it's available on Linux and Windows (macOS support is planned).

## Differences with Unix-like shells (Bash, ...)

Unlike traditional Unix-like shells like Bash or ZSH, ReShell is heavily geared towards scripting. What this means is, while basic commands will look more or less the same than in Bash, you will get a lot more features out of ReShell.

There are too many differences to count, but will you discover them along this tutorial.

## The basics

### Writing commands

A command is made of two things: the command's _path_, and its _arguments_.

```reshell
echo Hello!
```

The command's path (or name) is `echo`. This is a command that displays the argument you throw at it, which here is `Hello!`.

If you run it, it will display `Hello!` in your terminal.

Now let's add a little bit of color:

```reshell
echo Hello! -c yellow
```

This will print the same message, but in yellow this time. We have three arguments: `Hello!`, `-c` and `yellow`. Arguments starting with a dash are called _flags_, and the argument that follows them is usually their _value_. Here, it's the color's name.

There are lots of available commands, to see them all, start from an empty prompt and using the `<TAB>` key to list all of them (warning: there may be thousands of them).

If you try to write a command that doesn't exist, like `echoo Hello!`, the command's name will be highlighted in red instead of blue. This is to show you're trying to use an unknown command.

### Chaining commands

Commands can be chained using a `;` separator:

```reshell
# Will print 'Hello' then 'World!' on separate lines
echo Hello ; echo World!
```

Note that if any command in the chain fails, the remaining ones won't be run:

```reshell
# Create a file named 'test'
# Then remove it
# Then try to remove it again: this will file as it was already deleted
# So the 'echo' command afterwards won't be run
touch test ; rm test ; rm test ; echo salut
```

This prevents executing an instruction that depends on the previous one, which would be in an inconsistent state.

### Variables

Sometimes we need to store informations, and we do that by using _variables_:

```reshell
# Declare a variable called 'name'
let message = 'Hello!';

# Display its content
echo $name
```

What we write after the `=` assignment operator is called the variable's _value_. Normally, strings are wrapped into quotes. For simplicity reasons, when you write commands, you don't need to put these quotes if they don't contain special characters like spaces. But when using the values anywhere else, you need to use quotes.

This means that `echo 'Hello!'` is functionally equivalent to `echo Hello!`.

### User input

We can ask for input from the user using the builtin `ask` _command_:

```reshell
ask 'Please enter your name: '
```

Enter your name, and it will print your answer again. This is because `ask` is actually a _function_, and functions can return values. We'll learn more about them later. For now, we just need a way to get the result and put it in a variable:

```reshell
let name = ask('Please enter your name:')

echo $name
```

Why did we use parenthesis here? Because traditionally, functions are called that way. For simplicity reasons, when we call them as commands, we can omit them. But we _could_ technically always use parenthesis if we wanted to.

### String interpolation

Sometimes we want to use a variable inside a string. This is called _interpolation_, and uses double quotes:

```reshell
let name = ask('Please enter your name:')

echo "Your name is: $name"
```

We can also directly call functions inside strings:

```reshell
echo "Your name is: `ask('Please enter your name: ')`"
```

### External commands

Sometimes we may want to run a command that exists both as an internal and as an external command. For instance, the builtin `echo` command doesn't work the same as Linux' `echo` one.

To run an external command, we simply prefix the command's name with a caret `^`:

```shell
# Print a message without a newline using Linux' native 'echo' command
^echo -n "Hello!"
```

This is especially useful when defining aliases or using external paths that start with a variable:

```shell
let a = 2
$a / 2 # Evaluates to 1
$a/1   # Same here

let dir = '/some/directory'

# This will fail because the shell cannot know if we're trying
# to call the "$dir/cmd" command ('/some/directory/cmd')
# or if we're trying to accidentally divide a string
$dir/cmd

# So we need to write this:
^$dir/cmd
```

### Aliases

Aliases are a way to re-use easily a command that we run often:

```shell
# Let's say we always do commits with Git using the following syntax:
git commit -m ...message...

# We can create an alias to make it simpler:
alias gm = git commit -m

# Now we can use it like this:
gm ...message...
```

## Scripting

In this part we'll going to learn how to write scripts that do more than just run commands!

### Conditionals

A _conditional_ allows to execute a set of instructions only if a certain condition is met:

```reshell
let name = ask('Please enter your name:')

if $name == 'John F. Kennedy' {
  echo "You're probably lying"
}
```

We can then chain additional conditions that run entirely if the first one wasn't met:

```reshell
if $name == 'John' {
  echo 'Hello John!'
} else if $name == 'Jack' {
  echo 'Hi there Jack!'
}
```

And we can add a final block for when none of the conditions were met:

```reshell
if $name == 'John' {
  echo 'Hello John!'
} else if $name == 'Jack' {
  echo 'Hi there Jack!'
} else if $name == 'Jerry' {
  echo 'Ho hi there, Jerry!'
} else {
  echo "Hello, person going by the name of $name!"
}
```

### Loops

A _loop_ allows to execute a set of instructions multiple times:

```reshell
for i in 0..10 {
  echo $i
}
```

This program will print numbers from `0` to `9`. The latest bound is not included, until you specifically request it:

```reshell
for i in 1..=10 {
  echo $i
}
```

This program will print numbers from `1` to `10`.

There is also the possibility of running instructions repeatedly _while_ a certain condition is met:

```reshell
let mut i = 0

while $i <= 10 {
  $i = $i + 1
  echo $i
}
```

This is equivalent to the previous loop.

Loops can also use two special instructions:

```reshell
# 'continue' will just skip all instructions below it
# and jump to the beginning of the loop
continue

# 'break' exits the loop immediatly
break
```

### Matching

Sometimes we can get pretty ugly conditions like one of the previous examples:

```reshell
if $name == 'John' {
  echo 'Hello John!'
} else if $name == 'Jack' {
  echo 'Hi there Jack!'
} else if $name == 'Jerry' {
  echo 'Ho hi there, Jerry!'
} else {
  echo "Hello, person going by the name of $name!"
}
```

We can simplify this by using a `match` statement:

```reshell
match $name {
  case 'John'  { echo 'Hello John!' }
  case 'Jack'  { echo 'Hi there Jack!' }
  case 'Jerry' { echo 'Ho hi there, Jerry!' }

  else {
    echo "Hello, person going by the name of $name!"
  }
}
```

### Splitting into multiple files

When your script gets too big, you can split it into multiple files, and _include_:

```reshell
# file: ask.rsh
let name = ask('What is your name? ')

# file: main.rsh
include 'ask.rsh'
echo $name
```

## Functions

Now let's go into the heavy part of scripting: functions!

### Fundamentals

A function is a specific type of command which can _return_ a value. It can be user-defined, but there are also builtin functions (like `echo` or `ask`).

Here is how to declare a custom function:

```reshell
fn sayHello(name) {
  echo "Hello $name"
}

let name = ask('Please enter your name: ')
sayHello $name
```

Let's look at the function more closely: we declare a function called `sayHello`. It takes a single argument called `name`, which is then used as a variable inside the function's _body_.

### Returning values

Functions can return values:

```reshell
fn add(a, b) {
  return $a + $b
}

let total = add(2, 3)
echo $total # Prints: 5
```

The value returned (following the `return` keyword) can be used in various ways, like storing it into a variable.

### Optional arguments

Arguments can be set as _optional_. If they are not provided, their value is set to `null`:

```reshell
# The '?' symbol indicates the argument is optional
fn test(value?) {
  if $value == null {
    echo "You either didn't provide an argument or provided the 'null' value"
  } else {
    echo "Thanks for providing a non-null value!"
  }
}

test Hello! # 'value' argument will be set to `'Hello!'`
test        # 'value' will be set to `null`
```

### Flags

Functions can also use flag arguments:

```reshell
# The '?' marker indicates the argument is optional
# The ': int' part is the argument's type, which we'll learn about later
fn sayHello(name, --repeat?: int) {
  # The '??' operator allows to fall back to another value
  # if the left operand is `null`
  let repeat = $repeat ?? 1

  for i in 0..$repeat {
    echo "Hello, $name!"
  }
}

let name = ask('Tell me your name: ')

sayHello $name
sayHello $name --repeat 10
# can also be written:
sayHello $name --repeat=10
```

When calling a function inside an expression, flags use a special syntax:

```reshell
sayHello($name, repeat: 10)
```

### Presence flags

_Presence flags_ are flags that don't take a value, they are either present or they're not:

```reshell
# Here, the value of 'twice' inside the function's body
# will either be `true` or `false`
fn sayHello(name: string, --twice?) {
  echo "Hello, $name!"

  if $twice {
    echo "Hello, $name! (bis)"
  }
}

sayHello Jack
sayHello Jack --twice

sayHello('Jack', twice: true)

# Incorrect, will fail
sayHello Jack --twice=10
```

### Methods

Methods are special functions that can only be used on specific types (which we'll see in a moment). For instance, you can get the length of a string using its `.len` method:

```reshell
let msg = 'Hello!'
$msg.len() # 5

let num = 2
$num.len() # this won't work because '.len' doesn't exist on numbers
```

You can define your own methods by simply declaring a function that takes a `self` argument:

```reshell
fn twice(self: string) -> string {
  return "$self$self"
}

("Hello").twice() # "HelloHello"
```

### Lambdas

There is a specific type of functions called _lambdas_. These are functions that are used as values instead of being declared:

```reshell
let lambda = {|msg| echo $msg }

$lambda('Hello world!')
```

They can use types for arguments, and take flags. They just can't specify an explicit return type.

```reshell
let lambda = {|msg: string, --repeat?: int|
  for _ in 0..($repeat ?? 1) {
    echo $msg
  }
}

$lambda('Hello world!', repeat: 10)
```

Lambdas are where the trailing statement rule are very useful:

```reshell
let add = {|a, b| $a + $b }

add 2 3 # 5
```

### Error handling

Error handling can be used through values _throwing_ and _catching_:

```reshell
fn divide(a: int, b: int) -> int {
  if $b == 0 {
    throw 'Division by zero is forbidden!'
  }

  return $a / $b
}

divide 2 0 # Will fail because the function 'throw'
```

This is a good way to return errors to the caller. Errors can be handled through a `try` / `catch` block:

```reshell
try {
  divide(2, 0)
} catch e {
  echo "Error: $e"
}
```

## Types

Each value has an associated _type_ which indicates what "category" it belongs to.

### Common types

Common types include the following:

| Type name | Description            | Example            |
| --------- | ---------------------- | ------------------ |
| `string`  | Strings                | `'Hello world!'`   |
| `int`     | Integer numbers        | `2`                |
| `float`   | Floating-point numbers | `2.5`              |
| `bool`    | Booleans               | `true` and `false` |
| `null`    | The null value         | `null`             |

There are other types, but these are the most fundamental ones. There is also a special type called `any` which accepts every single value.

### Debugging a value's type

To show a value's type, we can use the `dbgType` function:

```reshell
dbgType 2
dbgType 'Hello!'
dbgType ([1, 2, 3])
```

### Typing variables

We can use them explicitly for various purposes, like enforcing a variable's type:

```reshell
let mut name = 'Clément'
$name = 2 # this works because variables accept any type by default

let mut name: string = 'Clément'
$name = 2 # this will actually fail because we're not assigning a string
```

### Typing functions

Functions can type their arguments (including flags) as well as their return type. This is not required but helps to ensure the function is not called incorrectly and also doesn't return a value of the incorrect type:

```reshell
fn add(a, b) { return $a + $b }

# This will fail inside the function as we can't add strings together
add 'Hello!' 'World!'

# With explicit typing:
fn add(a: int, b: int) -> int { return $a + $b }

# This will fail as soon as we *call* the function as the arguments are incorrect
add 'Hello!' 'World!'
```

Basically, explicit typing ensures that a variable always hold a value of the correct type.

Note that the last statement of a value (called the _trailing_ one) is returned as a value:

```reshell
fn add(a, b) { $a + $b }
```

Most of the time, you should prefer explicitly using the `return` keyword as it makes your intent clearer and avoids mistakes.

### Structures

Structures have a rigid type that associated _fields_ and values:

```reshell
let person = {
  name: 'John',
  age: 20
}

echo ($person.name) # Prints: John
echo ($person.age)  # Prints: 20

$person.name = 'Jack'
echo ($person.name) # Prints: Jack
```

Structures are typed like this:

```reshell
fn sayHello(person: { name: string, age: int }) {
  echo "Hello, `$person.name`!"
}

sayHello({
  name: 'John',
  age: 20
})
```

To debug a value of any type, we can use the `dbg` function:

```shell
dbg ({
  name: 'John',
  age: 20
})
```

### Lists

Lists are a special type that can hold multiple values at once:

```reshell
let names = ['John', 'Jack', 'Jerry']

# The 'dbg' function allows to display a value's content, no matter its type
# Whereas 'echo' only accepts strings, numbers and booleans
dbg $names
```

We can access lists using indexes:

```reshell
let names = ['John', 'Jack', 'Jerry']

# parenthesis allows to expressions in command arguments
echo ($names[0]) # Prints: John
```

Note that indexes always start at `0`.

You can add new values to a list:

```reshell
# These two are functionally equivalent:
$names[] = 'John 2'
$names.push('John 2')
```

To remove values:

```reshell
# Remove the latest value from a list
# Will return 'null' if the list is empty
$names.pop() # John 2

# Remove a value at a specific index
$names.removeAt(2)
```

Lists can be iterated through in a loop:

```reshell
for name in $names {
  echo $name
}
```

Note that lists are what we call a _container type_, which means we can change its content even when the variable is immutable (as long as we use the `.push` method):

```reshell
let list = [1, 2, 3]

# Won't work because we're using the special syntax
$list[] = 4

# Works fine
$list.push(4)
```

Lists can be typed using `list[<inner type>]`, e.g.:

```reshell
let value: list[int]    = [1, 2, 3]
let value: list[string] = ['a', 'b', 'c']

# we can also use the 'list' type alone, which is functionally equivalent
# to list[any]:
let value: list = [1, 2, 3]
```

### Maps

Maps associate a set of key-values, but unlike with structures they can be added or removed:

```reshell
map {
  name: 'John',
  age: 20
}
```

We can use them like this:

```reshell
echo ($map['name']) # Prints: John
echo ($map['age'])  # Prints: 20

$map['location'] = 'Paris' # We can add new keys!

$map.remove('location') # We can also remove them!
```

It is possible to use _computed keys_ for maps as well:

```reshell
map {
  # Identifier
  key1: 1,

  # Literal string
  'key2': 2,

  # Computed string
  "key`3`": 3,

  # Expression
  ["key`4`"]: 4
}
```

We can also iterate over maps:

```reshell
for key, value in $map {
  echo "$key => $value"
}
```

Maps can be typed using `map[<inner type>]`, e.g.:

```reshell
let value: map[int]    = map { a: 1, b: 2 }
let value: map[string] = map { a: 'a', b: 'b' }

# we can also use the 'map' type alone, which is functionally equivalent
# to map[any]:
let value: map = map { a: 1, b: 'c' }
```

### Type aliases

Sometimes we may need to re-use a type, we can define an _alias_:

```reshell
type Person = {
  name: string,
  age: int
}

fn sayHello(person: Person) {
  echo "Hello, `$person.name`!"
}

sayHello({
  name: 'Jack',
  age: 20
})
```

### Function signatures

Functions have type, which is called their _signature_:

```reshell
type FnExample = fn(name: string)

let lambda: FnExample = |name: string| { echo "Hello!" }
```

### Sub-typing

All types accept anything that's _compatible_ with them, such as:

```reshell
type FnExample = fn(name: string)

# This works fine
let lambda: FnExample = {|name: any| echo $name}
```

### Union types

Sometimes we need to specify that a variable may have different types, we then use _unions_:

```reshell
fn displayValue(value: (string | int)) {
  echo $value
}

# Both of these work fine
displayValue('Hello!')
displayValue(2)
```

Note that union types must always be wrapped inside parenthesis.

### Matching on types

When we use an union type - or `any`, we may need to find out what's the type behind it.

We have two ways of doing that: either use the `typename` method with a list of conditions:

```reshell
let typ = $value.typename()

if $typ == 'string' {
  # ...
} else if $typ == 'int' {
  # ...
} else {
  # ...
}
```

Or we can use a specific type-matching:

```reshell
typematch $value {
  case string {
    # Here we know the value is a string
  }

  case int {
    # Here we know the value is an integer
  }

  else {
    # here we know it's neither a string nor an integer
  }
}
```

## Advanced features

### Variable shadowing

We can re-declare a variable with the same name to _shadow_ the previous one:

```reshell
let var = '{"a":1}'
let var = parseJson($var)
```

### Destructuring

To simplify some assignments, we can use _destructuring_:

```reshell
let [a, b, c] = [1, 2, 3]
let { a, b, c } = { a: 1, b: 2, c: 3 }
let { a, b, c } = map { a: 1, b: 2, c: 3 }

echo $a # Prints: 1
echo $b # Prints: 2
echo $c # Prints: 3
```

Variables can also be renamed:

```reshell
let { a: b } = { a: 1 }

echo $b # Prints: 1
```

We can also do some nesting:

```reshell
let { a: [b, c, { d }] } = { a: [1, 2, { d: 3 }] }

dbg $b # Prints: 1
dbg $c # Prints: 2
dbg #d # Prints: 3
```

