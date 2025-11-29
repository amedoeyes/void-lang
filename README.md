# Void Lang

This project implements a simple lazy evaluated expression based functional programming language with Hindley Milner type system.

```fsharp
let fib = n ->
	if n == 0 || n == 1 then n
	else fib (n - 1) + fib (n - 2);
```

## Features

Types:

- Unit: `()`
- Int: `1 2 3 4 5 6 8 9 ..`
- Char: `'a' 'b' '\n' ..`
- Bool: `true` | `false`
- Lists: `[1, 2, 3, 4]` | `"Hello"`
  - Lists can only contains elements of the same type
- Lambda: `x -> x`

### Bind Declarations:

```fsharp
let add = a -> b -> a + b;
```

### Application

```fsharp
add 1 2 // 3
```

### Currying

```fsharp
let add_10 = add 10;
add_10 1 // 11
```

### Control Flow

```fsharp
let answer = x ->
	if x == 11 then x + 31
	else if x == 31 then x + 11
	else x;
```

### Recursion

```fsharp
let sum = n ->
	if n == 0 then 0
	else n + sum (n - 1);
```

### Operator Declarations

```fsharp
let (^) = x -> n -> if n == 0 then 1 else x * (x ^ (n - 1));
```

### Operator Fixity Declarations

```fsharp
op ^ right 8;
```

> [!NOTE]
> If not declared the default will applied which is left associative with precedence of 9

### Operator Sectioning

```fsharp
let add = (+) 1 1;
let addl = (+ 1) 1;
let addr = (1 +) 1;
```

### Lists:

```fsharp
let numbers = [1, 2, 3, 4];
```

```fsharp
head numbers // 1
```

```fsharp
tail numbers // [2, 3, 4]
```

> [!NOTE]
> `head` and `tail` are builtin functions.

### List Construct Operator

```fsharp
1 : 2 : 3 : [] // [1, 2, 3]
```

```fsharp
0 : [1, 2, 3] // [0, 1, 2, 3]
```

### List Concat Operator

```fsharp
[1, 2, 3] ++ [4, 5, 6] // [1, 2, 3, 4, 5, 6]
```

### Strings

Strings are just `[Char]` but can be represented as characters in quotations:

```fsharp
"Hello World"
```

And because it's essentially a list, all list operations apply.

### List Examples

You can find all of these list functions in `list.void` in the `std` directory.

```fsharp
range 1 10 // [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```

```fsharp
foldl (acc -> x -> acc + x) 0 (range 1 100) // 5050
```

```fsharp
let product = foldl (acc -> x -> acc * x) 1;
let factorial = n -> product (range 1 n);

factorial 10 // 3628800
```

```fsharp
map (x -> x * 2) (range 1 5) // [2, 4, 6, 8, 10]
```

```fsharp
filter (x -> x % 2 == 0) (range 1 10) // [2, 4, 6, 8, 10]
```

```fsharp
any (x -> x % 2 == 0) [1, 3, 5, 6, 7] // true
```

```fsharp
all (x -> x % 2 == 0) [1, 3, 5, 6, 7] // false
```

### Merge Sort Implementation

```fsharp
let merge = l1 -> l2 ->
	if l1 == [] then l2
	else if l2 == [] then l1
	else if head l1 < head l2 then
		head l1 : merge (tail l1) l2
	else
		head l2 : merge l1 (tail l2);

let sort = l ->
	if l == [] then l
	else if tail l == [] then l
	else
		merge
			(sort (take (length l / 2) l))
			(sort (drop (length l / 2) l));
```

```fsharp
sort [6, 3, 1, 5, 1, 4, 2, 0] // [0, 1, 1, 2, 3, 4, 5, 6]
```

### Input

Input can be accessed by either command line arguments using the `args` builtin bind, or by using the `read` builtin function supplied with a path. `read` can read from stdin when given `"-"` as an argument.

```fsharp
let filename = if tail args == [] then "examples/hello.void" else head (tail args);
"File: " ++ filename ++ " | " ++ "Contents: " ++ read filename
```

```fsharp
"Hello, " ++ read "-"
```

### Modules

Modules are filesystem-based where modules map directly to files and directories. For example:

**Project Structure**:

```
.
├── lib/
│   ├── module.void   # Directory module entry point
│   └── div.void      # Sub-module
├── add.void          # Single file module
└── main.void         # Entry point
```

**Files:**

`add.void`:

```fsharp
let add = a -> b -> a + b;
```

`lib/modules.void`:

```fsharp
let mul = a -> b -> a + b;
```

`lib/div.void`:

```fsharp
let div = a -> b -> a / b;
```

`main.void`:

```fsharp
import add;
import lib;
import lib.div;

mul (add 1 2) (div 10 2)
```

**Resolution Rules:**

- Single files: `import add` -> `./add.void`
- Directory modules: `import lib` -> `./lib/module.void`
- Sub-modules: `import lib.div` -> `./lib/div.void`

### Standard Library

```fsharp
import std.list;

sort [6,1,6,3,7,3,4,8,1,0,2]
```

## Type System

The type system uses the Hindley Milner algorithm to infer and check types. For example from the examples above `fib` will be inferred as `fib : Int -> Int` because `n` is being compared with `0`. While for example something like `let id = x -> x;` will be inferred as `id : forall t1 . t1 -> t1` because it's a polymorphic type.

It also implements constraints so for example `let add = a -> b -> a + b;` will be inferred as `add : forall t1 . (Num t1) => t1 -> t1 -> t1` where `t1` must have an instance of `Num`.
