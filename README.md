# Void Lang

This project implements a simple expression based functional programming language with Hindley Milner type system.

```haskell
let fib = n ->
	if n == 0 then 0
	else if n == 1 then 1
	else fib (n - 1) + fib (n - 2);
```

## Features

Types:

- Unit: `()`
- Int: `1 2 3 4 5 6 8 9`
- Bool: `true` | `false`
- Lists: `[1, 2, 3, 4]`
  - Lists can only contains elements of the same type
- Function: `x -> x`

Functions:

```haskell
let add = a -> b -> a + b;
```

Application:

```haskell
add 1 2 // 3
```

Currying:

```haskell
let add_10 = add 10;
add_10 1 // 11
```

Control flow:

```haskell
let answer = x ->
	if x == 11 then x + 31
	else if x == 31 then x + 11
	else x;
```

Recursion:

```haskell
let sum = n ->
	if n == 0 then 0
	else n + sum (n - 1);
```

Lists:

```haskell
let numbers = [1, 2, 3, 4];
```

```haskell
head numbers // 1
```

```haskell
tail numbers // [2, 3, 4]
```

> [!NOTE] `head` and `tail` are builtin functions.

List construct operator:

```haskell
1 : 2 : 3 : [1, 2, 3] // [1, 2]
```

```haskell
0 : [1, 2, 3] // [0, 1, 2, 3]
```

List concat operator:

```haskell
[1, 2, 3] ++ [4, 5, 6] // [1, 2, 3, 4, 5, 6]
```

List examples:

```haskell
range 1 10 // [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```

```haskell
foldl (acc -> x -> acc + x) 0 (range 1 100) // 5050
```

```haskell
let product = foldl (acc -> x -> acc * x) 1;
let factorial = n -> product (range 1 n);

factorial 10 // 3628800
```

```haskell
map (x -> x * 2) (range 1 5) // [2, 4, 6, 8, 10]
```

```haskell
filter (x -> x % 2 == 0) (range 1 10) // [2, 4, 6, 8, 10]
```

```haskell
any (x -> x % 2 == 0) [1, 3, 5, 6, 7] // true
```

```haskell
all (x -> x % 2 == 0) [1, 3, 5, 6, 7] // false
```

> [!NOTE] You can find all of these list functions in `list.void` in the `examples` directory.

## Type System

The language utilizes Hindley Milner algorithm to infer and check types. For example from the examples above `fib` will be inferred as `fib : Int -> Int` because `n` is being compared with `0`. While for example something like `let id = x -> x;` will be inferred as `id : forall t0 . t0 -> t0` because it's a polymorphic type.
