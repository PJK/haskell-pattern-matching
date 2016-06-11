# Pattern Matching in Haskell

[Pavel Kalvoda](https://github.com/PJK)
[Tom Sydney Kerckhove](https://github.com/NorfairKing)

Proof-of-concept implementation of the algorithm proposed by [Schrijvers, et al.](http://dl.acm.org/citation.cfm?id=2784748&CFID=628992486&CFTOKEN=93477105).

## Build/Install/Run

To build/install, you will need a copy of the repository:

```
git clone https://github.com/PJK/paps-pattern-matching
```

### Assignment and Report

The assignment and report for the project are written in standard LaTeX.
To build them, you will need `latexmk`.

```
make -C assignment
make -C report
```

### Code

The code for the tool is written in Haskell and built with `stack`/`cabal`.
A `stack.yaml` is provided.

Run `stack build` to build, `stack install` to install and `stack test` to run the tests.

To run the code as intended, you will need the `z3` SMT solver.
The code will work without problems if you don't have `z3`, but you may not get the desired result.

## Usage

As `patterns --help` will tell you:

```
Usage: patterns COMMAND [-d|--debug]
  Analyse pattern matching by Pavel Kalvoda and Tom Sydney Kerckhove

Available options:
  -h,--help                Show this help text
  -d,--debug               turn on debug information

Available commands:
  analyze                  Analyze and present recommendations
  evaluatedness            Present evaluatedness
  dump-results             Dump analysis results for testing
```

## How to read results

### Recommendations

There are three kinds of recommendations that you may get.


#### Non-exhaustive pattern

##### Simple patterns

Example code: `data/exact/Bools.hs`

``` Haskell
and :: Bool -> Bool -> Bool
and True True = True
and False _ = False
```

As you can see, there is a clause missing.

```
$ patterns analyze data/exact/Bools.hs
In function and:
The patterns may not be exhaustive, the following clauses are missing:
and True False
```

This means that, to complete the function's definition, you may have to add another clause, namely `and True False`.
Because the tool only approximates, it is possible to be recommended to complete a function that already has exhaustive patterns in practice.

##### Patterns with guards

The tool also works for programs with guards.

Example code: `data/exact/Abs.hs`

``` Haskell
abs :: Int -> Int
abs x
    | x < 0 = - x
    | x > 0 = x
```

This function is undefined when `x` equals `0`, and the tool figures this out:

```
$ patterns analyze data/exact/Abs.hs

In function abs:
The patterns may not be exhaustive, the following clauses are missing:
abs ~a
Constraints:
  ~f == False
  ~f == ~a > 0
  ~c == False
  ~c == ~a < 0

Satisfiable. Model:
  ~f = False :: Bool
  ~a =     0 :: Integer
  ~c = False :: Bool
```

Note: the tilde's in front of the variables `~a`, etc just indicate that this is a fresh variable introduced in the analysis.
They are simply variables like any other, but they make sure we don't accidentally use the same variable twice in the analysis.

This should be read as follows:
To complete the function, you may need to add a clause of the form `abs ~a`.
Here are the constraints under which this clause would be selected and here is a model that shows one specific situation in which that would in fact happen.

#### Redundant clause

Example code: `data/exact/SimpleRedundant.hs`

``` Haskell
g :: Bool -> Int
g True = 1
g True = 2
g False = 3
```

As you can see, the second clause is redundant.

```
$ patterns analyze data/exact/SimpleRedundant.hs
In function g:
The following clause is redundant:
g True
```

This means that this clause can never be selected for evaluation, and its removal will not alter the semantics of the function.
Even though the tool approximates, it overapproximates, so when it tells you that a clause is redundant, that is guaranteed to be true.
It is then safe to remove this clause.

#### Inaccessible Right-hand side

Example code: `data/exact/ReportExample.hs`

``` Haskell
f :: Bool -> Bool -> Int
f _    True  = 1
f True True  = 2
f _    False = 3
```

The second clause has an inaccessible right-hand side.

```
$ patterns analyze data/exact/ReportExample.hs
In function f:
The following clause has an inaccesible right hand side:
f True True
```

This means that this clause will never be selected for evaluation.
Even though the tool approximates, it overapproximates, so when it tells you that a clause has an inaccessible right-hand side, that is guaranteed to be true.
It is then safe to remove this clause.

### Evaluatedness

Example code: `data/exact/Tree.hs`

``` Haskell
data Tree a = Fork a (Tree a) (Tree a) | Nil

func :: Tree a -> Int
func Nil              = 1
func (Fork _ Nil Nil) = 2
func (Fork _ Nil _)   = 3
func (Fork _ _   Nil) = 4
```

When we ask the tool to compute the evaluatedness of this function, we get the following output:

```
Evaluatedness of function "func"

func ~a
~a: ~a

func (Fork ~e ~f ~g)
(Fork ~e ~f ~g): (Fork _ ~f _)

func (Fork ~e Nil ~g)
(Fork ~e Nil ~g): (Fork _ Nil ~g)

func (Fork ~e (Fork ~k ~l ~m) ~g)
(Fork ~e (Fork ~k ~l ~m) ~g): (Fork _ (Fork _ _ _) ~g)
```

While this looks very complicated at first, it is actually not so bad to read once you understand the format.

The first line of every 'paragraph' is something of the form `<function> <arg1 arg2 ... argk>`.
This line indicates the form of the input and should be read as "When the function `function` is evaluated with the arguments `<arg1 arg2 ... argk>`,".
The next lines then complete this sentence: `<arg1>: <evaluatedness pattern of arg1>` should be read as `<arg1> will be evaluated such that `<evaluatedness pattern of arg1>`.

Concretely, for the example above this becomes:

```
func ~a
~a: ~a
```

"When the input to `func` is an arbitrary argument, which we will call `~a`,
`~a`'s first constructor is evaluated."

```
func (Fork ~e ~f ~g)
(Fork ~e ~f ~g): (Fork _ ~f _)
```

"When the input to `func` is of the form `(Fork ~e ~f ~g)` where `~e`, `~f` and `~g` are arbitrary expressions,
`~e` and `~g` remain untouched but `~f`'s first constructor will be evaluated."

etc...

## Development

Should you want to contribute, please install the git hooks as provided to ensure (some standard of) code quality.

```
spark deploy hooks.sus
```
