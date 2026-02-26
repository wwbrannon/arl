# arl 0.1.1

## Bug Fixes

* Fixed stale module cache poisoning when prelude macros differ between
  sessions. The module cache now tracks a prelude fingerprint and
  invalidates entries when macros change.
* Fixed missing dependency declaration in `looping.arl`.
* Fixed Windows backslash path handling in tests.
* Fixed `FileDeps` tests to work during `R CMD check`.
* Fixed bad URLs in README and vignettes.
* Included vignette files in built tarballs (`.Rbuildignore` fix).

## Testing

* Tests now disallow unexpected/unhandled warnings.
* Added `make coverage-test-file` target for single-file coverage testing.

# arl 0.1.0

Initial release of arl, a Scheme-inspired Lisp dialect embedded in R.

## Language

* **Special forms:** `quote`, `if`, `define`, `set!`, `lambda`, `begin`,
  `defmacro`, `quasiquote`/`unquote`/`unquote-splicing`, `and`, `or`,
  `while`, `delay`, `import`, and `module`.
* **Truthiness:** `#f`, `#nil`, and `0` are falsy; everything else is truthy.
* **Cons cells:** First-class dotted pairs, with both cons-cell and R-native
  list representations.
* **Destructuring:** Pattern destructuring in `define`, `set!`, `let`,
  and `lambda`, including nested patterns, rest parameters, and defaults.
* **Multiple values:** `values` and `call-with-values`.
* **Continuations:** `call-cc` via R's native `callCC`.

## Macro System

* Compile-time code transformation with quasiquotation templates.
* Hygiene via `gensym` and `capture`.
* Introspection with `macro?`, `macroexpand`, `macroexpand-1`, and
  `macroexpand-all`.
* Documentation attachment via `;;'` comments.

## Module System

* Named and anonymous modules with explicit or `export-all` exports.
* Qualified access via `/` syntax (e.g. `math/inc`).
* Import modifiers: `:refer`, `:as`, `:rename`, `:reload`.
* Modules are first-class environment objects with locked bindings.
* Re-export support, circular dependency detection, and module caching.
* Introspection: `module?`, `module-name`, `module-exports`, `module-ref`.

## Compiler

* Single-pass compilation from Arl to R expressions.
* Self-tail-call optimization: rewrites self-recursive functions in tail
  position as `while` loops.
* Constant folding, dead code elimination, strength reduction, identity
  lambda elimination, boolean flattening, and arithmetic infix compilation.

## Standard Library

22 modules (~800 definitions) auto-loaded via a prelude:

* **Core and logic:** `when`, `unless`, `cond`, error handling, identity.
* **Types and equality:** Full suite of type predicates; `equal?`, `eqv?`,
  `eq?`.
* **List and sequences:** `append`, `reverse`, `take`, `drop`, `nth`,
  `range`, `repeat`, `cycle`, `zip`, and more.
* **Functional:** `map`, `filter`, `reduce`, `fold`, `compose`, `partial`,
  `juxt`, `complement`, `constantly`.
* **Control flow:** `try-catch`, `finally`, condition signaling and handling.
* **Binding forms:** `let`, `let*`, `letrec` with destructuring.
* **Threading macros:** `->`, `->>`, `as->`, `some->`, `some->>`,
  `cond->`, `cond->>`.
* **Looping:** `until`, `do-list`, Clojure-style `loop`/`recur`.
* **Math:** `inc`, `dec`, `abs`, `sqrt`, trigonometric functions, and more.
* **Strings:** `str`, `string-concat`, `string-split`, and friends.
* **I/O:** File reading/writing, display, and formatting.
* **Data structures:** Hash-backed `dict` and `set` types with full
  operation suites; `defstruct` macro for S3-backed record types.
* **Sorting:** `list-sort`, `sort-by`, `merge-sorted`, `stable-sort`.
* **Assertions:** `assert-equal`, `assert-true`, `assert-false`, and others.

## R Interoperability

* Direct access to all R base and default-package functions.
* Keyword arguments (`:name value`), formulas, `$`, `[`, `[[`, and `@`.
* `r-eval` and `r-call` for dynamic R evaluation.
* Bidirectional data exchange: define R objects into Arl, extract results
  back to R.
* Configurable R package visibility.

## Developer Tools

* **REPL:** Interactive prompt with multi-line input, readline history,
  bracketed paste, and error recovery.
* **CLI:** Script execution, `--eval` expressions, quiet mode, bare engine
  mode, and stdin support.
* **Help system:** `(help topic)` dispatches to special forms, builtins,
  stdlib docstrings, or R help. `doc` and `doc!` for programmatic access.
* **Coverage tracking:** Line-level and branch-level instrumentation with
  console, HTML, and JSON reports.
* **Knitr engine:** `{arl}` code chunks in R Markdown with persistent
  engine state and syntax highlighting.

## Documentation

* Vignettes: getting started, standard library reference, compiler and
  internals, and troubleshooting.
