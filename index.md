
<!-- index.md — pkgdown homepage (overrides README.md for site only) -->

# Arl

**An embeeded Lisp dialect for R** — a true Lisp macro system, tail-call
optimization, and other features new to R, but with seamless R interoperability.

Arl compiles to R and evaluates with R's native `eval()`, so every R function
and data structure is available directly — no compatibility layers needed. The
macro system lets you transform code at compile time, and support for self-tail-call
optimization allows deep recursive patterns without stack overflow. Both of
these (and other features!) go beyond what R natively provides.

To get started, see the [installation and core syntax
guide](articles/getting-started.html) or jump straight to the
[examples](articles/examples.html) for complete working programs.

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
(lm (~ mpg wt) :data mtcars)
```

### Macros

```lisp
(defmacro when (test . body)
  `(if ,test (begin ,@body) #nil))

(when (> 5 3) (print "yes"))   ; => "yes"
```

### Data Pipelines

```lisp
(->> (list 1 2 3 4 5)
     (filter (lambda (x) (> x 2)))
     (map (lambda (x) (* x x)))
     (reduce +))               ; => 50
```

## Features & Guides

- **[Seamless R interop](articles/r-interop.html)** — call any R function, use keywords for named arguments, work with R data structures directly.
- **[Powerful macro system](articles/macros.html)** — `defmacro` with quasiquote, unquote, and splicing gives you compile-time code transformation.
- **[Self-tail-call optimization](articles/tail-call-optimization.html)** — the compiler rewrites self-recursive tail calls as loops, so deep recursion won't overflow the stack.
- **[Modular standard library](articles/lang-reference.html)** — `(import threading)`, `(import control)`, `(import sort)` — load only the modules you need.
- **[Benchmarks](articles/benchmarks.html)** — performance characteristics, profiling, and optimization history.

## Install

### From R

```r
> devtools::install_github("wwbrannon/arl")

> library(arl)
> engine <- Engine$new()
> engine$repl()
```

### CLI

```bash
> arl::install_cli()     # one-time setup from R

$ arl                    # start REPL
$ arl --file script.arl  # or "-f"
$ arl --eval "(+ 1 2)"   # or "-e"
```
