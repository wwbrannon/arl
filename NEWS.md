# rye (development version)

## Features

* **Self-tail-call optimization (self-TCO):** The compiler detects
  `(define name (lambda ...))` with self-calls in tail position and rewrites
  them as `while` loops, avoiding stack overflow. Works through `if`, `begin`,
  `cond`, `let`, `let*`, and `letrec`, and supports destructuring params,
  keyword args, and rest params.
* **Rye code coverage:** New `RyeCoverageTracker` class instruments Rye stdlib
  and user code, tracking line-level and branch-level (`if`) coverage. Reports
  in console, HTML, and JSON formats; integrates with Codecov via flags.
* **Sorting module:** New `sort`, `sort-by`, `merge`, and `stable-sort`
  functions (`inst/rye/sort.rye`).
* **Dict and set modules:** Hash-backed `dict` and `set` data structures with
  full helper suites (`dict-get`, `dict-set`, `dict-merge`, `set-union`, etc.).
* **Struct module:** `defstruct` macro for S3-backed struct constructors,
  predicates, and accessors.
* **Help and docstring system:** `(help "topic")` dispatches to specials,
  macros, function docstrings, or R help. `(doc fn)` / `(doc! fn "text")` for
  programmatic docstring access.
* **Continuations:** `call/cc` and `call-with-current-continuation` via R's
  native `callCC`.

## Bug Fixes

* Fixed `(= NULL NULL)` to return `#t` instead of `logical(0)`.
* Fixed `case` macro to use Scheme semantics (datum lists, not bare values).
* Fixed `math/log` to accept an optional base parameter; removed `logb`.
* Fixed duplicate `repeatedly` definition; fixed `str()` with no arguments.
* Fixed macro debug code gating, empty else blocks, and unsafe `unlockBinding`
  call flagged by R CMD check.

## Documentation

* Fixed `when`/`unless` macro signatures in vignettes to match actual
  rest-parameter form `(test . body)` with `begin`/`unquote-splicing`.
* Added missing vignettes to `_pkgdown.yml` (stdlib-types, stdlib-math,
  stdlib-equality, stdlib-conversions).
* Added `RyeCoverageTracker` to pkgdown reference.
* Expanded dict/set helper documentation with per-function entries.
* Added sorting function documentation.
* Added help/docstring system documentation to getting-started and stdlib
  reference vignettes.
* Fixed module loading examples to use `(import ...)` instead of `(load ...)`.
* Added compiler/internals, troubleshooting vignettes, and expanded
  CONTRIBUTING.md.

## Internal

* Rewrote tokenizer from character-at-a-time loop to regex-based lexing.
* Three tiers of performance optimizations (constant folding, memoization,
  environment caching, compiled runtime fast paths).
* Extracted shared quasiquote walker into `R/quasiquote.R`.
* Refactored freestanding helpers into class private methods.
* Reduced duplication, fixed quadratic growth, cleaned up lint and dead code.
* Comprehensive test audit: fixed weak assertions, Scheme conformance issues,
  and stdlib bugs uncovered by coverage tracking.
* Stdlib coverage improved from 95.2% to 99.1%.

# rye 0.0.1

- Initial experimental release.
- Documentation refresh for README, vignettes, and stdlib reference.
