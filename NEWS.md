# rye 0.0.1

Initial CRAN release.

## Features

* **Self-tail-call optimization (self-TCO):** The compiler detects
  `(define name (lambda ...))` with self-calls in tail position and rewrites
  them as `while` loops, avoiding stack overflow. Works through `if`, `begin`,
  `cond`, `let`, `let*`, and `letrec`, and supports destructuring params,
  keyword args, and rest params.
* **Rye code coverage:** New `CoverageTracker` class instruments Rye stdlib
  and user code, tracking line-level and branch-level (`if`) coverage. Reports
  in console, HTML, and JSON formats; integrates with Codecov via flags.
* **Sorting module:** `sort`, `sort-by`, `merge`, and `stable-sort` functions.
* **Dict and set modules:** Hash-backed `dict` and `set` data structures with
  full helper suites (`dict-get`, `dict-set`, `dict-merge`, `set-union`, etc.).
* **Struct module:** `defstruct` macro for S3-backed struct constructors,
  predicates, and accessors.
* **Help and docstring system:** `(help "topic")` dispatches to specials,
  macros, function docstrings, or R help. `(doc fn)` / `(doc! fn "text")` for
  programmatic docstring access.
* **Continuations:** `call/cc` and `call-with-current-continuation` via R's
  native `callCC`.

## Documentation

* Getting-started, stdlib reference, compiler/internals, and troubleshooting
  vignettes.
* Per-function documentation for dict, set, sorting, and help/docstring APIs.
