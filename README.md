
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rye

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/wwbrannon/rye/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/wwbrannon/rye/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/rye)](https://CRAN.R-project.org/package=rye)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!--
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/rye)](https://CRAN.R-project.org/package=rye)
[![Codecov](https://codecov.io/gh/wwbrannon/rye/branch/main/graph/badge.svg)](https://codecov.io/gh/wwbrannon/rye)
[![R-universe](https://wwbrannon.r-universe.dev/badges/rye)](https://wwbrannon.r-universe.dev/rye)
[![pkgdown](https://img.shields.io/badge/pkgdown-site-blue.svg)][pkgdown-site]
--> <!-- badges: end -->

A Lisp dialect implemented in and with access to R, leveraging R’s
Scheme heritage.

## Overview

Rye is a Lisp dialect that seamlessly integrates with R. It provides:

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
library(rye)

# Start the REPL
engine <- RyeEngine$new()
engine$repl()
```

The engine loads the base stdlib automatically.

## Command Line

Install the CLI wrapper once after installing the package:

``` r
rye::rye_install_cli()
```

``` bash
# Start REPL
rye

# Run a file
rye --file script.rye

# Eval a single expression
rye --eval "(+ 1 2)"

# Positional files are executed in order
rye script.rye other.rye
```

If the install path is not on your `PATH`, add it:

``` r
dirname(system.file("exec", "rye", package = "rye"))
```

``` bash
export PATH="$(Rscript -e 'cat(dirname(system.file("exec", "rye", package = "rye")))'):$PATH"
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
(defmacro when (test body)
  "Evaluate body when test is truthy."
  `(if ,test ,body #nil))

(when (> 5 3) (print "yes"))  ; => "yes"

; Macro docstrings are a leading string literal
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
- `lambda` - Anonymous functions
- `begin` - Sequence of expressions
- `defmacro` - Define macros
- `quasiquote` / `` ` `` - Template with selective evaluation
- `load` - Load and evaluate a file
- `import` - Import a module’s exports
- `module` - Define a module with exports
- `~` - Formula (for R modeling)

### Continuations

Rye provides downward-only continuations via R’s native `callCC`
function, exposed as `call/cc` and `call-with-current-continuation`.
Unlike full Scheme continuations, R’s `callCC` supports one-shot,
downward-only escapes (early returns). Continuations capture Rye-level
control flow; side effects are not rewound.

### Tail Call Optimization

Rye does not provide automatic tail-call optimization. For efficient
tail-recursive patterns, use `loop`/`recur` from the `looping` module,
which expands to an R while loop:

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

**Control Flow Macros:** - `when`, `unless`, `and`, `or`, `cond`, `case`

**Binding & Looping:** - `let`, `let*`, `letrec`, `destructuring-bind` -
`while`, `for`

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

Additional Rye stdlib modules live in `inst/rye` so you can load just
what you need:

``` lisp
(load "control")   ; when/unless/and/or/cond/case
(load "binding")   ; let/let*/letrec
(load "looping")   ; while/for
(load "threading") ; -> and ->>
(load "error")     ; try/catch/finally
```

**Modules and loading:** `load` runs a file in the current environment
(definitions visible). `run` runs a file in an isolated child
(definitions not visible). `import` loads a module and attaches its
exports into the current scope only; each module is loaded once per
engine. From R, `engine$load_file(path)` runs a file in an isolated
scope; to have definitions visible in the engine, use
`engine$load_file_in_env(path, engine$env$env)` or evaluate
`(load "script.rye")` from the REPL. You can define your own modules
with `module` and `import`—see the [Modules and
Imports](articles/modules.html) guide.

### Examples

Check out the [examples](articles/examples.html) page for complete
working programs with syntax highlighting. They range from small
algorithms to macro techniques, data pipelines, and report outputs:

- **[fibonacci.rye](articles/examples.html#fibonacci)** - Multiple
  Fibonacci implementations (recursive, iterative, sequence generation)
- **[quicksort.rye](articles/examples.html#quicksort)** - Quicksort and
  mergesort demonstrating list operations
- **[fizzbuzz.rye](articles/examples.html#fizzbuzz)** - Various FizzBuzz
  implementations showcasing control flow
- **[macro-examples.rye](articles/examples.html#macro-examples)** -
  Comprehensive macro system demonstrations
- **[pipeline-macros.rye](articles/examples.html#pipeline-macros)** -
  Macro-driven pipeline expansion and data flow
- **[data-analysis.rye](articles/examples.html#data-analysis)** - R
  interop for data processing and statistics
- **[graph-paths.rye](articles/examples.html#graph-paths)** - BFS
  traversal and Dijkstra shortest paths with report output
- **[log-parser.rye](articles/examples.html#log-parser)** - Text parsing
  and summary stats from log lines
- **[sales-report.rye](articles/examples.html#sales-report)** - R
  interop for data wrangling and CSV report generation
- **[task-runner.rye](articles/examples.html#task-runner)** - Dependency
  resolution and execution ordering

### Semantics

- **Truthiness**: only `#f`/`FALSE` and `#nil`/`NULL` are falsey;
  everything else is truthy.
- **Lists**: Rye lists are backed by R lists or calls; `car` returns the
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

## Citing Rye

If you use Rye in academic work, please cite it. The package includes a
`CITATION` file, so you can use:

``` r
citation("rye")
```

## Architecture

Rye leverages R’s existing eval/quote/environment system:

1.  **Parser**: Converts S-expressions to R calls
2.  **Macro expander**: Processes `defmacro` definitions
3.  **Evaluator**: Handles special forms, delegates to R’s `eval()`
4.  **R bridge**: Seamless access to all R functions

## License

MIT
