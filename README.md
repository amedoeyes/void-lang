# Void Lang

Statically typed, lazily-evaluated functional programming language with Hindley-Milner type inference. Inspired by Haskell and Rust.

```fsharp
let fib = n ->
	if n == 0 || n == 1 then n
	else fib (n - 1) + fib (n - 2);

let main = println (fib 42);
```

> [!WARNING]
> Still in active development. Expect bugs and breaking changes.

## Features

### Bind Declarations:

Immutably bind an expression to a symbol

```fsharp
let x = 31;
let add = a -> b -> a + b;
```
We can add type annotation on binds

```fsharp
let bool_to_int : Bool -> Int = b -> if b then 1 else 0;
```
It's redundant here, because the type of `bool_to_int` is already infered as `Bool -> Int`. It can however be useful if we want to restrict a polymorphic function

```fsharp
let id_int : Int -> Int = x -> x;
```
Without the annotation, the type of `id_int` would be infered as `<a> a -> a` a function that takes something and returns it. But because we annotated it with `Int -> Int` we restricted it to only accepting `Int`.

### Local Binds

We can have local binds using block expressions

```fsharp
let main = {
	let x = 31;
	let y = 11;
	println (x + y)
};
```

Local binds can also be recursive

```fsharp
let fib = n -> {
	let aux = a -> b -> n ->
		if n == 0 then a
		else aux b (a + b) (n - 1);

	aux 0 1 n
};

let main = println (fib 42);
```

### Operator Declarations

Operators are just fancy binds that can be used in infix notation

```fsharp
op ^ right 8; // if not declared the default will applied which is left associative with precedence of 9
let (^) = x -> n -> if n == 0 then 1 else x * (x ^ (n - 1));

let main = println (2^10);
```

We can create all sorts of fun operators like pipes

```fsharp
op |> left 9;
let (|>) = a -> b -> b a;

let main = factorial 10 |> println;
```

### Operator Sectioning

```fsharp
let sub = (-); // turns the operater into a -> b -> a - b
let sub_one_l = (- 1); // turns the operater into a -> a - 1
let sub_one_r = (1 -); // turns the operater into b -> 1 - b
```

### ADTs

Algebraic data types lets us define custom types

```fsharp
type Bool = enum { False, True };

type List = <a> enum {
	Nil,
	Cons a (List a),
};

type Option = <a> enum {
	None,
	Some a,
};
```

And to distinguish between variants or access data stored in a variant we use match expression

```fsharp
let bool_to_int = b -> match b with {
	False => 0,
	True => 1,
};

let sum = l -> match l with {
	Nil => 0,
	Cons x xs => x + (sum xs)
};
```

> [!NOTE]
> Yes, `Bool` is an ADT and not a primitive type, and if expressions are just syntactic sugar that will build a match expression instead.
