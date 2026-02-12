
<!-- index.md — pkgdown homepage (overrides README.md for site only) -->

# Arl

**A Lisp dialect for R** — homoiconic syntax, powerful macros, and seamless access to every R function.

## Quick Examples

### Basics

```lisp
(+ 1 (* 2 3))        ; => 7
(define x 10)
(define square (lambda (x) (* x x)))
(square x)            ; => 100
```

### R Interop

```lisp
(mean (c 1 2 3 4 5))           ; => 3
(seq :from 1 :to 10 :by 2)     ; => 1 3 5 7 9
(lm (~ y x) :data df)
```

### Macros

```lisp
(defmacro when (test . body)
  `(if ,test (begin ,@body) #nil))

(when (> 5 3) (print "yes"))   ; => "yes"
```

### Data Pipelines

```lisp
(import threading)

(->> (list 1 2 3 4 5)
     (filter (lambda (x) (> x 2)))
     (map (lambda (x) (* x x)))
     (reduce +))               ; => 50
```

## Key Features

- **Seamless R interop** — call any R function, use keywords for named arguments, work with R data structures directly.
- **Powerful macro system** — `defmacro` with quasiquote, unquote, and splicing gives you compile-time code transformation.
- **Self-tail-call optimization** — the compiler rewrites self-recursive tail calls as loops, so deep recursion won't overflow the stack.
- **Modular standard library** — `(import threading)`, `(import control)`, `(import error)` — load only the modules you need.
- **Built-in help** — attach docstrings to functions and macros, then query them with `(help "name")`.

## Install

```r
# Install from source
devtools::install()

library(arl)
engine <- Engine$new()
engine$repl()
```

Or use the CLI:

```bash
arl::install_cli()   # one-time setup from R
arl                  # start REPL
arl --file script.arl
arl --eval "(+ 1 2)"
```

## Learn More

- [**Getting Started**](articles/getting-started.html) — installation, REPL basics, core syntax
- [**Examples**](articles/examples.html) — complete working programs from fibonacci to data analysis
- [**Macros Guide**](articles/macros.html) — defmacro, quasiquote, and hygiene
- [**R Interop**](articles/r-interop.html) — calling R from Arl and vice versa
- [**Standard Library**](articles/stdlib-reference.html) — full function reference
- [**Benchmarks**](articles/benchmarks.html) — performance characteristics and profiling

---

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/wwbrannon/arl/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/wwbrannon/arl/actions/workflows/R-CMD-check.yaml)
[![Codecov](https://codecov.io/gh/wwbrannon/arl/branch/main/graph/badge.svg)](https://codecov.io/gh/wwbrannon/arl)
[![CRAN status](https://www.r-pkg.org/badges/version/arl)](https://CRAN.R-project.org/package=arl)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
