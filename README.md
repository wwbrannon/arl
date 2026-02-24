
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Arl

<!-- badges: start -->

[![R CMD
check](https://github.com/wwbrannon/arl/actions/workflows/check.yaml/badge.svg?branch=main)](https://github.com/wwbrannon/arl/actions/workflows/check.yaml)
[![Codecov](https://app.codecov.io/gh/wwbrannon/arl/graph/badge.svg?token=7pxGM6lI73)](https://app.codecov.io/gh/wwbrannon/arl)
[![CRAN
status](https://www.r-pkg.org/badges/version/arl)](https://CRAN.R-project.org/package=arl)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.18740488.svg)](https://doi.org/10.5281/zenodo.18740488)
<!--
[![R Coverage](https://app.codecov.io/gh/wwbrannon/arl/branch/main/graph/badge.svg?token=7pxGM6lI73&flag=r-code)](https://app.codecov.io/gh/wwbrannon/arl?flags%5B0%5D=r-code)
[![Arl Coverage](https://app.codecov.io/gh/wwbrannon/arl/branch/main/graph/badge.svg?token=7pxGM6lI73&flag=arl-code)](https://app.codecov.io/gh/wwbrannon/arl?flags%5B0%5D=arl-code)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-red.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/arl)](https://CRAN.R-project.org/package=arl)
[![R-universe](https://wwbrannon.r-universe.dev/badges/arl)](https://wwbrannon.r-universe.dev/arl)
[![pkgdown](https://img.shields.io/badge/pkgdown-site-blue.svg)][pkgdown-site]
--> <!-- badges: end -->

A Lisp dialect that compiles to R: write macros, use every R function
directly, and get tail-call optimization out of the box.

## Overview

Arl compiles to R and evaluates with R’s own `eval()`, so every R
function and data structure is available with no need for a
compatibility layer or
[FFI](https://en.wikipedia.org/wiki/Foreign_function_interface). On top
of that, you get a Lisp macro system for compile-time code
transformation and self-tail-call optimization for stack-safe recursion.

## Installation

``` r
# install.packages("arl")
devtools::install_github("wwbrannon/arl")
```

Arl is not yet on CRAN; install from GitHub with `devtools` as shown
above. Once a CRAN release is available, `install.packages("arl")` will
work.

## Quick Start

``` r
library(arl)
engine <- Engine$new()
engine$repl()
```

The engine loads prelude modules (core, list, functional, control, etc.)
automatically. Non-prelude modules like `sort`, `dict`, and `strings`
require explicit `(import ...)`. (You can also disable prelude loading
with `load_prelude=FALSE` for a bare engine.) Type `(quit)` or press
Ctrl+C to exit the REPL.

Arithmetic and variables work as you’d expect:

``` lisp
(+ 1 (* 2 3))  ; => 7

(define x 10)
(+ x 5)  ; => 15
```

Functions are defined with `lambda`, and `let` provides local bindings:

``` lisp
(define factorial
  (lambda (n)
    (if (< n 2)
      1
      (* n (factorial (- n 1))))))

(factorial 5)  ; => 120

(let ((a 1) (b 2))
  (+ a b))  ; => 3
```

All R functions are callable directly, with keywords for named
arguments:

``` lisp
(mean (c 1 2 3 4 5))  ; => 3
(seq :from 1 :to 10 :by 2)  ; => 1 3 5 7 9
```

Macros transform code at compile time. `;;'` comments attach
documentation:

``` lisp
;;' @description Evaluate body when test is truthy.
(defmacro when (test . body)
  `(if ,test (begin ,@body) #nil))

(when (> 5 3) (print "yes"))  ; => "yes"
```

Higher-order functions work on lists:

``` lisp
(map (lambda (x) (* x 2)) (list 1 2 3))  ; => (2 4 6)
```

## Command Line

Run `arl::install_cli()` to see how to put the CLI wrapper on your PATH:

``` r
arl::install_cli()
#> Arl CLI wrapper script: /path/to/arl/bin/posix/arl
#>
#> To make it available on your PATH, create a symlink:
#>
#>   mkdir -p ~/.local/bin
#>   ln -s "/path/to/arl/bin/posix/arl" ~/.local/bin/arl
#>
#> Then ensure ~/.local/bin is on your PATH.
```

``` bash
arl                       # start REPL
arl --file script.arl     # run a file
arl --eval "(+ 1 2)"     # evaluate an expression
arl script.arl other.arl  # run multiple files in order
arl --help                # see all options
```

## Features

### Special Forms and Built-in Functions

Special forms are expressions with evaluation rules that differ from
normal function calls – for example, `if` does not evaluate all its
arguments, and `define` binds a name rather than passing it as a value.
They are handled directly by the compiler and cannot be redefined or
passed as values.

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
- `import` - Import a module’s exports
- `module` - Define a module with exports

R formula syntax (`~`) and package namespace access (`::`, `:::`) are
available via R interop, but are not Arl special forms.

`help` (along with `doc`/`doc!`) is a built-in function, not a special
form.

In addition to special forms, Arl provides a small set of **built-in
functions** implemented in R and available before any stdlib modules
load (and if stdlib loading is disabled), including: `eval`, `read`,
`write`, `load`, `gensym`, `capture`, `macro?`, `macroexpand`, `pair?`,
`car`, `cdr`, `cons`, `promise?`, `force`, `promise-expr`,
`toplevel-env`, `current-env`, `r-eval`, `help`, `doc`, `doc!`,
`module-ref`, `module?`, `module-exports`, and `module-name`. Unlike
special forms, these are ordinary functions and can be passed as values.
See the [Language
Reference](https://github.com/wwbrannon/arl/blob/main/vignettes/lang-reference.Rmd)
vignette for the full list.

### Continuations

Arl provides downward-only continuations via R’s native `callCC`
function, exposed as `call-cc` and `call-with-current-continuation`.
Unlike full Scheme continuations, R’s `callCC` supports one-shot,
downward-only escapes (early returns). Continuations capture Arl-level
control flow; side effects are not rewound.

### Tail Call Optimization

Arl’s compiler implements **self-tail-call optimization** (self-TCO).
When you bind a function with `(define name (lambda ...))` or
`(set! name (lambda ...))` and the body contains self-calls in tail
position, the compiler automatically rewrites them as `while` loops –
avoiding stack growth. This works through `if`, `begin`, `cond`, `let`,
`let*`, and `letrec` in tail position.

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

Prelude modules are loaded automatically; non-prelude modules require
explicit `(import ...)`. Key areas include:

- **Lists** (prelude): `car`, `cdr`, `cons`, `append`, `reverse`, `nth`,
  `list*`
- **Higher-order** (prelude): `map`, `filter`, `reduce`, `compose`,
  `partial`, `every?`, `any?`
- **Sequences** (prelude): `take`, `drop`, `take-while`, `drop-while`,
  `partition`, `flatten`, `zip`
- **Control flow** (prelude): `when`, `unless`, `cond`, `case`
- **Bindings** (prelude): `let`, `let*`, `letrec`, `destructuring-bind`
- **Threading** (prelude): `->`, `->>`
- **Looping** (import looping): `do-list`, `loop`/`recur`, `until`
- **Error handling** (prelude/import assert): `try`, `try-catch`
  (prelude); `assert`, `assert-equal` (import assert)
- **Strings & display** (import strings/display): `string-join`,
  `string-split`, `string-append` (import strings); `display`,
  `println`, `format-value` (import display)
- **Macros**: `gensym`, `macroexpand`, `eval`
- **Predicates** (prelude): `null?`, `list?`, `number?`, `string?`,
  `fn?`, and more

For the complete function reference, see the [Language
Reference](https://github.com/wwbrannon/arl/blob/main/vignettes/lang-reference.Rmd)
vignette.

The stdlib is organized into modules under `inst/arl/`. Prelude modules
are always available; non-prelude modules require explicit import (or
use `Engine$new(load_prelude = FALSE)` for a completely bare engine):

``` lisp
(import math)      ; inc/dec/abs/min/max/floor/ceiling/round/square/...
(import looping)   ; until/do-list/loop/recur
(import sort)      ; list-sort/sort-by
(import strings)   ; str/string-join/string-split/...
```

### Modules and File Loading

- `(load "file.arl")` – run a file in the current environment
  (definitions visible)
- `(load "file.arl" env)` – run a file in the specified environment
- `(run "file.arl")` – run a file in an isolated child environment
- `(import M)` – load module M and attach its exports to the current
  scope

From R: `engine$load_file_in_env(path)` corresponds to `load`;
`engine$load_file_in_env(path, new.env(parent = engine$get_env()))`
corresponds to `run`. See the [Modules and
Imports](https://github.com/wwbrannon/arl/blob/main/vignettes/modules.Rmd)
vignette for defining your own modules.

### Semantics

- **Truthiness**: `#f`/`FALSE`, `#nil`/`NULL`, and `0` are falsey;
  everything else is truthy (same as R).
- **Lists**: Arl lists are backed by R lists or calls; `car` returns the
  head and `cdr` returns the tail as a list. Dotted pairs (`cons` with
  non-list cdr) are also supported; see the [Arl vs
  Scheme](https://github.com/wwbrannon/arl/blob/main/vignettes/arl-vs-scheme.Rmd)
  vignette.
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

### Examples

Check out the
[examples](https://github.com/wwbrannon/arl/tree/main/inst/examples) for
complete working programs with syntax highlighting. They range from
small algorithms to macro techniques, data pipelines, and report
outputs:

- **[fibonacci.arl](https://github.com/wwbrannon/arl/blob/main/inst/examples/fibonacci.arl)** -
  Multiple Fibonacci implementations (recursive, iterative, sequence
  generation)
- **[quicksort.arl](https://github.com/wwbrannon/arl/blob/main/inst/examples/quicksort.arl)** -
  Quicksort and mergesort demonstrating list operations
- **[fizzbuzz.arl](https://github.com/wwbrannon/arl/blob/main/inst/examples/fizzbuzz.arl)** -
  Various FizzBuzz implementations showcasing control flow
- **[macro-examples.arl](https://github.com/wwbrannon/arl/blob/main/inst/examples/macro-examples.arl)** -
  Comprehensive macro system demonstrations
- **[pipeline-macros.arl](https://github.com/wwbrannon/arl/blob/main/inst/examples/pipeline-macros.arl)** -
  Macro-driven pipeline expansion and data flow
- **[data-analysis.arl](https://github.com/wwbrannon/arl/blob/main/inst/examples/data-analysis.arl)** -
  R interop for data processing and statistics
- **[graph-paths.arl](https://github.com/wwbrannon/arl/blob/main/inst/examples/graph-paths.arl)** -
  BFS traversal and Dijkstra shortest paths with report output
- **[log-parser.arl](https://github.com/wwbrannon/arl/blob/main/inst/examples/log-parser.arl)** -
  Text parsing and summary stats from log lines
- **[sales-report.arl](https://github.com/wwbrannon/arl/blob/main/inst/examples/sales-report.arl)** -
  R interop for data wrangling and CSV report generation
- **[task-runner.arl](https://github.com/wwbrannon/arl/blob/main/inst/examples/task-runner.arl)** -
  Dependency resolution and execution ordering

## Architecture

Arl leverages R’s existing eval/quote/environment system:

1.  **Lexer/Tokenizer**: Lexical analysis of Arl source, producing a
    token stream for the parser to consume.
2.  **Parser**: Consume the tokenizer’s token stream and produce an
    abstract syntax tree (AST), removing syntactic sugar like `'`
    (quote) \`\`\` (quasiquote), etc.
3.  **Macro expander**: Expand macros occurring in the input into code.
    Macro expansion is recursive, with each step generating new code
    that may have further macro calls to expand. Expansion terminates
    when no macros remain to expand. Macros are the signature feature of
    Lisp, and the expansion phase provides the opportunity to do
    arbitrary computation and source-code transformation at compile
    time.
4.  **Compiler**: Compile the Arl AST resulting from macro expansion
    into R code, handling all special forms and tail call optimization
    where possible.
5.  **R Evaluation**: The R code resulting from compilation is passed to
    R’s `base::eval()` for evaluation, taking advantage of the highly
    optimized R evaluator and providing seamless access to all R
    functions.

Every part of this pipeline is implemented in pure R, with no compiled C
code. The simplicity of Lisp syntax means that the parser in particular
can be a simple [recursive
descent](https://en.wikipedia.org/wiki/Recursive_descent_parser) parser,
rather than a more complex implementation with flex and bison.

## Development

See the [Makefile](https://github.com/wwbrannon/arl/blob/main/Makefile)
for common development commands:

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

If you use Arl in academic work, please cite it:

``` r
citation("arl")
```

## License

MIT
