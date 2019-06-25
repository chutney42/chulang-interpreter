# ChuLang Interpreter

Interpreter of lazy, strongly and statically typed, functional language based mainly on Ocaml and Haskell. Created during JPP course.

## How to run
It is stack project so compile it using
```stack build```

There are two possible modes:

1. Interpreter can read from standard input. Every line is interpreted separately but the state is kept between lines executions.
``` stack exec interpreter ```

2. Interpreter can execute one or more files. Every file is interpreted separately but the state is kept between files executions.
``` stack exec interpreter good/stdlib.cl good/lists.cl ```

Every program is made of three instruction types:
1. expression
``` (\x y. x + y) 1 2 ; ```

2. variable declaration  
``` let f x y = x + y ; ```

3. type declaration
``` type Maybe a = Just a | Nothing ; ```

## What is done
* Built-in types: int, bool
* Arithmetics, comparisons
* Conditional expressions
* Functions with many arguments, recursion
* Lambda expressions, partial application, higher-order functions, closures
* Handle of evaluation errors such as zero devision
* Static name binding
* Static typing
* Polymorphic and recursive algebraic types
* Multi-level pattern matching
* Type reconstruction (Damas–Hindley–Milner type system based on http://dev.stephendiehl.com/fun/006_hindley_milner.html)

## What could be done
1. Negative numbers xd
2. Syntactic sugar for lists
3. Tuples

## Further information
Parsing done using BNF Converter.

There are some examples in `good` and `bad` directory.