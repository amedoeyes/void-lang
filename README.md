# Void Lang

This project implements a simple functional programming language with Hindley Milner type system.

## Examples

Fibonacci:

```haskell
let fib = n ->
	if n == 0 then 0
	else if n == 1 then 1
	else fib (n - 1) + fib (n - 2);

fib 10
```

Fibonacci tail recursive:

```haskell
let fib_helper = a -> b -> n ->
	if n == 0 then a
	else fib_helper b (a + b) (n - 1);

let fib = n -> fib_helper 0 1 n;

fib 91
```

Lists (Church-encoded):

```haskell
let nil = f -> z -> z;
let cons = h -> t -> f -> z -> f h (t f z);
let fold = f -> z -> list -> list f z;

let length = list ->
	fold (x -> acc -> acc + 1) 0 list;

let sum = list ->
	fold add 0 list;

let map = f -> list ->
	fold (h -> t -> cons (f h) t) nil list;

let filter = pred -> list ->
	fold (h -> t -> if pred h then cons h t else t) nil list;
```

```haskell
let nums = cons 1 (cons 2 (cons 3 nil));
sum (map (x -> x * 2) nums) // 12
```

```haskell
let nums = cons 1 (cons 2 (cons 3 nil));
length (filter (x -> x % 2 == 0) nums) // 1
```

## Type System

The language utilizes Hindley Milner algorithm to infer and check types. For example from the examples above `fib` will be inferred as `fib : Int -> Int` because `n` is being compared with `0`. While for example something like `let id = x -> x;` will be inferred as `id : forall t0 . t0 -> t0` because it's a polymorphic type.
