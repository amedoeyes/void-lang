# Void Lang

This project implements a simple functional programming language with Hindley Milner type system.

## Examples

Fibonacci:

```rust
let fib = n ->
	if n == 0 then 0
	else if n == 1 then 1
	else fib (n - 1) + fib (n - 2);

fib 10
```

Power:

```rust
let power = x -> n ->
	if n == 0 then 1
	else x * power x (n - 1);

power 2 10
```

Compose:

```rust
let compose = f -> g -> x -> f (g x);
let inc = x -> x + 1;
let is_even = x -> x % 2 == 0;

compose is_even inc 5
```

## Type System

The language utilizes Hindley Milner algorithm to infer and check types. For example from the examples above `fib` will be inferred as `fib : Int -> Int` because `n` is being compared with `0`. While for example something like `let id = x -> x;` will be inferred as `id : forall t0 . t0 -> t0` because it's polymorphic type.
