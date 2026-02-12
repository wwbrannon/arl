
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Arl

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/wwbrannon/arl/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/wwbrannon/arl/actions/workflows/R-CMD-check.yaml)
[![Codecov](https://codecov.io/gh/wwbrannon/arl/branch/main/graph/badge.svg)](https://codecov.io/gh/wwbrannon/arl)
[![R
Coverage](https://codecov.io/gh/wwbrannon/arl/branch/main/graph/badge.svg?flag=r-code)](https://codecov.io/gh/wwbrannon/arl?flags%5B0%5D=r-code)
[![Arl
Coverage](https://codecov.io/gh/wwbrannon/arl/branch/main/graph/badge.svg?flag=arl-code)](https://codecov.io/gh/wwbrannon/arl?flags%5B0%5D=arl-code)
[![CRAN
status](https://www.r-pkg.org/badges/version/arl)](https://CRAN.R-project.org/package=arl)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!--
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/arl)](https://CRAN.R-project.org/package=arl)
[![R-universe](https://wwbrannon.r-universe.dev/badges/arl)](https://wwbrannon.r-universe.dev/arl)
[![pkgdown](https://img.shields.io/badge/pkgdown-site-blue.svg)][pkgdown-site]
--> <!-- badges: end -->

A Lisp dialect implemented in and with access to R, leveraging R’s
Scheme heritage.

## Overview

Arl is a Lisp dialect that seamlessly integrates with R. It provides:

- **Homoiconic syntax**: Code as data, data as code
- **Powerful macros**: Transform code at compile-time with `defmacro`
  and quasiquote
- **R interoperability**: Call any R function, use R data structures
- **Lexical scoping**: Clean, predictable variable binding
- **First-class functions**: Functions are values, closures work
  naturally

## Installation

``` r
# Install from source
devtools::install()

# Or install dependencies and load
devtools::install_deps()
devtools::load_all()
```

## Quick Start

``` r
library(arl)

# Start the REPL
engine <- Engine$new()
engine$repl()
```

The engine loads the base stdlib automatically.

## Command Line

Install the CLI wrapper once after installing the package:

``` r
arl::install_cli()
```

``` bash
# Start REPL
arl

# Run a file
arl --file script.arl

# Eval a single expression
arl --eval "(+ 1 2)"

# Positional files are executed in order
arl script.arl other.arl
```

If the install path is not on your `PATH`, add it:

``` r
dirname(system.file("exec", "arl", package = "arl"))
```

``` bash
export PATH="$(Rscript -e 'cat(dirname(system.file("exec", "arl", package = "arl")))'):$PATH"
```

### Examples

``` lisp
; Arithmetic
(+ 1 (* 2 3))  ; => 7

; Variables
(define x 10)
(+ x 5)  ; => 15

; Destructuring
(define (a b . rest) (list 1 2 3 4))
(list a b rest)  ; => (1 2 (3 4))

; Functions
(define factorial
  (lambda (n)
    (if (< n 2)
      1
      (* n (factorial (- n 1))))))

(factorial 5)  ; => 120

; R interop
(mean (c 1 2 3 4 5))  ; => 3

; Named arguments
(seq :from 1 :to 10 :by 2)  ; => 1 3 5 7 9

; Macros
;;' @description Evaluate body when test is truthy.
(defmacro when (test . body)
  `(if ,test (begin ,@body) #nil))

(when (> 5 3) (print "yes"))  ; => "yes"

; Documentation via ;;' annotation comments
(help "when")

; Higher-order functions
(define double (lambda (x) (* x 2)))
(map double (list 1 2 3))  ; => (2 4 6)
```

## Features

### Special Forms

- `quote` / `'` - Return unevaluated expression
- `if` - Conditional evaluation
- `define` - Variable/function definition
- `set!` - Mutation of existing bindings
- `lambda` - Anonymous functions
- `begin` - Sequence of expressions
- `defmacro` - Define macros
- `quasiquote` / `` ` `` - Template with selective evaluation
- `and`, `or` - Short-circuit boolean operators
- `while` - Loop with condition
- `delay` - Lazy promise creation
- `load` - Load and evaluate a file
- `run` - Run a file in an isolated environment
- `import` - Import a module’s exports
- `module` - Define a module with exports
- `~` - Formula (for R modeling)
- `::`, `:::` - R package namespace access
- `help` - Look up documentation

### Continuations

Arl provides downward-only continuations via R’s native `callCC`
function, exposed as `call/cc` and `call-with-current-continuation`.
Unlike full Scheme continuations, R’s `callCC` supports one-shot,
downward-only escapes (early returns). Continuations capture Arl-level
control flow; side effects are not rewound.

### Tail Call Optimization

Arl’s compiler implements **self-tail-call optimization** (self-TCO).
When you define a function with `(define name (lambda ...))` and the
body contains self-calls in tail position, the compiler automatically
rewrites them as `while` loops – avoiding stack growth. This works
through `if`, `begin`, `cond`, `let`, `let*`, and `letrec` in tail
position.

``` lisp
;; The compiler optimizes this -- no stack overflow even for large n
(define factorial
  (lambda (n acc)
    (if (< n 2)
      acc
      (factorial (- n 1) (* acc n)))))

(factorial 100000 1)
```

Since self-calls become loop iterations, recursive call frames won’t
appear in stack traces on error – only the outermost call is visible.

For mutual recursion or explicit looping patterns, `loop`/`recur` from
the `looping` module is still available:

``` lisp
(import looping)

(define factorial
  (lambda (n)
    (loop ((i n) (acc 1))
      (if (< i 2)
        acc
        (recur (- i 1) (* acc i))))))
```

### Standard Library

The REPL loads a base standard library (implemented in R) with core
helpers and access to all base R functions.

**Core List Operations:** - `car`, `cdr`, `cons`, `call`, `list*`,
`append`, `reverse`, `apply` - `first`, `rest`, `last`, `nth`
(convenience aliases)

**Higher-Order Functions:** - `map`, `mapcat`, `filter`, `remove`,
`reduce`, `foldl`, `foldr` - `every?`, `any?`, `complement`, `compose`,
`partial`

**Sequence Helpers:** - `take`, `drop`, `take-while`, `drop-while`,
`partition`, `flatten`, `zip` - `repeatedly`, `repeat`

**Predicates:** - `null?`, `nil?`, `list?`, `pair?`, `list-or-pair?`,
`symbol?`, `keyword?` - `number?`, `string?`, `vector?`, `true?`,
`false?`, `fn?`, `callable?`

**Control Flow Macros:** - `when`, `unless`, `cond`, `case`

**Binding & Looping:** - `let`, `let*`, `letrec`, `destructuring-bind` -
`for`

**Threading Macros:** - `->`, `->>` (thread-first, thread-last)

**Error Handling:** - `try`, `catch`, `finally`, `error`, `warn`,
`assert`, `trace`, `try*`

**Continuations & Promises:** - `call/cc`,
`call-with-current-continuation`, `promise?`, `force`

**String & I/O:** - `str`, `string-join`, `string-split`, `trim`,
`format` - `read-line`, `display`, `println`

**Macro System:** - `gensym`, `macro?`, `eval`, `macroexpand`,
`macroexpand-1`

**Interop:** - `dict`, `hash`, `r/call`, `identity`

**Arithmetic & Comparison:** - `+`, `-`, `*`, `/`, `%`, `=`, `<`, `>`,
`<=`, `>=` - `not`

For detailed documentation of all functions, see the [**Standard Library
Reference**](docs/stdlib-reference.md).

The full standard library is loaded automatically for convenience. It is
organized into modules, each in `inst/arl/`, which you can also import
individually in your own modules or when working with a bare engine
(`Engine$new(load_stdlib = FALSE)`):

``` lisp
(import control)   ; when/unless/cond/case/try*
(import binding)   ; let/let*/letrec
(import looping)   ; until/for/loop/recur
(import threading) ; -> and ->>
(import error)     ; try/catch/finally
```

**Modules and loading:** `load` runs a file in the current environment
(definitions visible). `run` runs a file in an isolated child
(definitions not visible). `import` loads a module and attaches its
exports into the current scope only; each module is loaded once per
engine. From R, `engine$load_file(path)` runs a file in an isolated
scope; to have definitions visible in the engine, use
`engine$load_file_in_env(path, engine$env$env)` or evaluate
`(load "script.arl")` from the REPL. You can define your own modules
with `module` and `import`—see the [Modules and
Imports](articles/modules.html) guide.

### Examples

Check out the [examples](articles/examples.html) page for complete
working programs with syntax highlighting. They range from small
algorithms to macro techniques, data pipelines, and report outputs:

- **[fibonacci.arl](articles/examples.html#fibonacci)** - Multiple
  Fibonacci implementations (recursive, iterative, sequence generation)
- **[quicksort.arl](articles/examples.html#quicksort)** - Quicksort and
  mergesort demonstrating list operations
- **[fizzbuzz.arl](articles/examples.html#fizzbuzz)** - Various FizzBuzz
  implementations showcasing control flow
- **[macro-examples.arl](articles/examples.html#macro-examples)** -
  Comprehensive macro system demonstrations
- **[pipeline-macros.arl](articles/examples.html#pipeline-macros)** -
  Macro-driven pipeline expansion and data flow
- **[data-analysis.arl](articles/examples.html#data-analysis)** - R
  interop for data processing and statistics
- **[graph-paths.arl](articles/examples.html#graph-paths)** - BFS
  traversal and Dijkstra shortest paths with report output
- **[log-parser.arl](articles/examples.html#log-parser)** - Text parsing
  and summary stats from log lines
- **[sales-report.arl](articles/examples.html#sales-report)** - R
  interop for data wrangling and CSV report generation
- **[task-runner.arl](articles/examples.html#task-runner)** - Dependency
  resolution and execution ordering

### Semantics

- **Truthiness**: `#f`/`FALSE`, `#nil`/`NULL`, and `0` are falsey;
  everything else is truthy.
- **Lists**: Arl lists are backed by R lists or calls; `car` returns the
  head and `cdr` returns the tail as a list.
- **Keywords**: `:kw` tokens are self-evaluating and become named
  arguments in function calls.

### R Integration

All R functions are accessible directly:

``` lisp
; Call R functions
(lm (~ y x) :data df)

; Use R operators
($ mylist field)
([ vector 1)

; Access R data structures
(define df (data.frame :x (c 1 2 3) :y (c 4 5 6)))
```

## Development

See the [Makefile](Makefile) for common development commands:

``` bash
# Run tests
make test

# Check package
make build
make check

# Generate documentation
make document
```

## Citing Arl

If you use Arl in academic work, please cite it. The package includes a
`CITATION` file, so you can use:

``` r
citation("arl")
```

## Architecture

Arl leverages R’s existing eval/quote/environment system:

1.  **Tokenizer**: Lexical analysis of Arl source
2.  **Parser**: Converts S-expressions to R calls
3.  **Macro expander**: Processes `defmacro` definitions
4.  **Compiler**: Compiles Arl AST to R code, handling all special forms
    and self-TCO
5.  **R bridge**: Compiled code is evaluated via R’s `eval()`, with
    seamless access to all R functions

## License

MIT
