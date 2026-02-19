# arl 0.1.0

## Module System

* **Qualified access via `/`:** Import a module and access its exports with
  `mod/sym` syntax (e.g. `math/inc`). The `/` is reader sugar for the new
  `module-ref` builtin.
* **Symbol renames:** `r/call` → `r-call`, `r/eval` → `r-eval`,
  `call/cc` → `call-cc` (freeing `/` for namespace use).
* **New import modifiers:** `:refer` and `:as` replace the old `:only`,
  `:except`, and `:prefix` modifiers. Bare `(import X)` now binds the module
  without dumping exports; use `(import X :refer :all)` for unqualified access.
* **First-class modules:** Modules are bound as first-class environment objects.
  Use `:as` to alias, `:refer` to selectively import, and qualified access for
  everything else.
* **Nameless modules:** `(module (export ...) body...)` derives its name from
  the source filename.
* **Binding locking:** Module bindings are locked after load (immutable from
  outside).
* **`export-all :re-export`:** Facade modules can re-export imported symbols.
* **`_` prefix convention:** Names starting with `_` are excluded from
  `export-all` (private by convention).
* **New builtins:** `module-ref`, `module?`, `namespace?`, `module-exports`,
  `module-name` for module introspection.

## Compiler

* **Self-tail-call optimization (self-TCO):** The compiler detects
  `(define name (lambda ...))` with self-calls in tail position and rewrites
  them as `while` loops, avoiding stack overflow. Works through `if`, `begin`,
  `cond`, `let`, `let*`, and `letrec`, and supports destructuring params,
  keyword args, and rest params.

## Standard Library

* **Sorting module:** `sort`, `sort-by`, `merge`, and `stable-sort` functions.
* **Dict and set modules:** Hash-backed `dict` and `set` data structures with
  full helper suites (`dict-get`, `dict-set`, `dict-merge`, `set-union`, etc.).
* **Struct module:** `defstruct` macro for S3-backed struct constructors,
  predicates, and accessors.
* **Help and docstring system:** `(help "topic")` dispatches to specials,
  macros, function docstrings, or R help. `(doc fn)` / `(doc! fn "text")` for
  programmatic docstring access.
* **Continuations:** `call-cc` and `call-with-current-continuation` via R's
  native `callCC`.
* **Arl code coverage:** A coverage-tracking facility instruments Arl stdlib
  and user code, tracking line-level and branch-level (`if`) coverage. Reports
  in console, HTML, and JSON formats; integrates with Codecov via flags.

## Documentation

* Getting-started, stdlib reference, compiler/internals, and troubleshooting
  vignettes.
* Per-function documentation for dict, set, sorting, and help/docstring APIs.
