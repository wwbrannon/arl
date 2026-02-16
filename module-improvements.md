# Module System Improvements

This document catalogs the limitations of Arl's current module system, compares
how Scheme, R, and Python handle the same challenges, and proposes a prioritized
list of improvements.

## Current Limitations

### 1. No Module Reloading / Redefinition

The registry hard-blocks re-registration: if a module name is already registered,
it errors. There is an `unregister()` method, but it isn't wired up to any
user-facing workflow. During interactive development you can't reload a changed
module without creating a fresh engine.

### 2. One Module Per File (Implicit Coupling)

The system assumes each `.arl` file contains exactly one `(module ...)` form.
File-path-based caching, path aliasing, and the static dependency analysis in
`file-deps.R` all encode this assumption. You can't put multiple small utility
modules in one file, and there is no "script mode" file that is also importable
as a module.

### 3. Flat Module Namespace

Module names are flat strings with no hierarchy. There is no
`(import mylib.utils)` or `(import mylib/utils)`. As the ecosystem grows beyond
the stdlib, flat names will cause collisions.

### 4. Circular Dependencies Are Hard Errors

Cycle detection in `import_compiled` throws immediately. The only resolution has
been manual workaround (e.g., `list.arl` using R's `identical` and the builtin
`=` instead of importing from `equality`). There is no mechanism for forward
declarations, late binding, or deferred imports.

### 5. Exports Are Copy-Based, Not Reference-Based

`attach_into` copies values from the module env into the importing env with
`mget` + `list2env`. If a module mutates a top-level binding after it has been
imported, the importer sees the stale value. Each import of the same module into
different consumers creates independent copies of every exported value. For
closures this mostly doesn't matter (they close over the module env), but for
data values or mutable objects the semantics are surprising. Copy semantics also
make reloading and circular dependency resolution fundamentally harder.

### 6. `export-all` Is Fragile

`export-all` excludes `.__*` names and `.__imported_symbols`, but any helper
without a `.__` prefix gets exported. There is no `(private ...)` declaration or
naming convention for non-internal helpers that should remain private.

### 7. Stdlib Loads Eagerly and Entirely

`.load_stdlib_into_env` iterates all modules in load order and attaches every one
into the engine env. Every Arl program pays the cost of loading the entire
stdlib, even if it only uses `car`. There is no lazy loading or autoload
mechanism.

### 8. No Re-Export / Facade Modules

There is no `(re-export ...)` pattern. If you want a convenience module that
bundles several others, you must manually import and re-define each binding.
`export-all` actively filters out imported symbols, preventing even the obvious
workaround.

### 9. Macro Hygiene Across Modules

`defmacro` is Lisp-style unhygienic: expansion is template-based and evaluates
in the caller's environment. Module boundaries make this trickier because the
macro might expand to code referencing symbols that the caller doesn't have
imported. Closures sidestep this (they close over the defining module), but pure
template macros don't.

### 10. Import Side Effects / Ordering

Imports are compile-time special forms with side effects. Conditional imports are
not possible (`import` inside `if` compiles but creates confusing runtime
behavior), and the top-level-only restriction is not enforced.

### 11. No Qualified Access to Module Exports

The only way to import is to dump (possibly filtered) exports into the current
scope. There is no `mod.foo` or `mod/foo` qualified access after import. The
`:prefix` modifier is the closest workaround but is manual and doesn't preserve
the module-as-namespace idea.

---

## Comparison to Other Languages

### Module Reloading

- **Scheme (R7RS):** Libraries are loaded once; no standard reload mechanism.
  Racket has `dynamic-rerequire` as a development tool.
- **R:** Fully supports it. `devtools::load_all()` re-sources everything.
  Namespaces can be unloaded with `unloadNamespace()` and reloaded. `source()`
  just overwrites bindings.
- **Python:** `importlib.reload(mod)` exists but is notoriously tricky — existing
  references to old objects aren't updated. The common workaround is restarting
  the interpreter.

### One Module Per File

- **Scheme (R7RS):** Libraries are logical units; implementations choose how to
  map them to files. Racket uses one-per-file as convention but supports
  submodules with `module` and `module*`.
- **R:** Inverted: many `.R` files concatenated into one namespace per package.
- **Python:** One module per file, plus packages (directories with
  `__init__.py`).

One-module-per-file is a reasonable convention shared by Python and Racket. The
real gap is the lack of a script mode.

### Flat Module Namespace

- **Scheme (R7RS):** Hierarchical: `(import (scheme base))`, `(import (srfi 1))`.
- **R:** Flat (single-string package names). CRAN enforces uniqueness. `::` gives
  qualified access.
- **Python:** Hierarchical with dots: `import os.path`,
  `from collections.abc import Mapping`.

### Circular Dependencies

- **Scheme:** Generally forbidden or undefined. Racket detects and errors.
- **R:** Allowed in a limited sense — packages can have circular `Imports` if
  they only use each other at function-call time (not load time). `importFrom`
  creates bindings that resolve lazily because R functions are looked up at call
  time.
- **Python:** Handles them surprisingly well. `import foo` creates a
  partially-initialized module object; circular imports get the partial object.
  As long as you don't access circular bindings at import time (only inside
  function bodies), it works. This is because Python modules are mutable
  namespace objects with identity — imports are references, not copies.

### Copy vs. Reference Semantics

- **Scheme:** Imported bindings are immutable, so copy vs. reference doesn't
  matter.
- **R:** Reference-based. `importFrom(dplyr, filter)` puts a binding that points
  to the same object in dplyr's namespace. Lazy evaluation means function calls
  resolve through environment lookup.
- **Python:** Reference-based. `import foo` gives a reference to the module
  object; `foo.bar` does a live attribute lookup. `from foo import bar` captures
  the current value (won't track later rebinding of `foo.bar`).

This is the most architecturally significant difference. Copy semantics make
circular deps and module reloading fundamentally harder.

### Export-All / Private Bindings

- **Scheme (R7RS):** No export-all. You must list every export. Private is the
  default. Racket has `(provide (all-defined-out))` but discourages it.
- **R:** Explicit via `NAMESPACE`. `exportPattern("^[^.]")` is common (similar to
  Arl's `export-all`).
- **Python:** Convention-based. `_foo` is private by convention. `__all__` limits
  `from mod import *`.

Consensus: explicit exports, private by default.

### Eager Stdlib Loading

- **Scheme:** `(import (scheme base))` loads only that library.
- **R:** Packages load lazily. `lazyLoad` defers deserialization until access.
- **Python:** `import os` loads only `os`. The stdlib isn't pre-loaded.

Every mature language does some form of lazy/on-demand loading.

### Re-Export

- **Scheme:** `(export (rename orig new))` and Racket's
  `(provide (all-from-out submod))`.
- **R:** Built in. `roxygen2` has `@importFrom pkg fn` + `@export fn`.
- **Python:** `from .submod import *` in `__init__.py`.

All three support it.

### Macro Hygiene

- **Scheme:** Hygienic macros (`syntax-rules`, `syntax-case`) are the crown
  jewel. Identifiers in expansions refer to bindings from where the macro was
  defined, not where it's used. The module system and macro system are deeply
  integrated.
- **R:** No macro system. Tidy evaluation / quosures are the closest analog.
- **Python:** No macro system.

### Conditional / Non-Top-Level Imports

- **Scheme:** Imports must be top-level in the library definition.
- **R:** `library()` and `require()` can appear anywhere, including inside
  function bodies. `requireNamespace()` returns TRUE/FALSE for conditional loading.
- **Python:** `import` can appear anywhere — inside functions, inside `if`
  blocks. Commonly used for optional dependencies and avoiding circular imports.

### Qualified Access

- **Scheme (R7RS):** No qualified access. `(import (prefix (srfi 1) srfi/))` is
  the workaround.
- **R:** `dplyr::filter` — `::` is universally used and idiomatic.
- **Python:** `import numpy as np; np.array(...)` — qualified access is the
  preferred style.

### Summary Table

| Issue                  | Scheme   | R          | Python       | Arl          |
|------------------------|----------|------------|--------------|--------------|
| Reload                 | No       | Yes        | Fragile      | No           |
| File mapping           | Flexible | Many→one   | One→one      | One→one      |
| Namespace hierarchy    | Yes      | No         | Yes          | No           |
| Circular deps          | Error    | Lazy works | Partial works| Error        |
| Import semantics       | Immutable| Reference  | Reference    | Mutable copy |
| Private default        | Yes      | Yes        | Convention   | No           |
| Lazy stdlib            | Per-lib  | lazyLoad   | Per-import   | Eager all    |
| Re-export              | Yes      | Yes        | Yes          | No           |
| Macro hygiene          | Built-in | N/A        | N/A          | None         |
| Conditional import     | No       | Yes        | Yes          | Not enforced |
| Qualified access       | Via pfx  | `::`       | Default      | Via `:prefix`|

---

## Prioritized Improvements

### Tier 1: High Value, Moderate Effort

**1. Lazy stdlib loading.** Use `delayedAssign` in `engine_env` so modules are
only loaded on first access. Nearly free given R's infrastructure, reduces
startup cost, and is a pure improvement with no semantic changes. The main work
is changing `.load_stdlib_into_env` to register delayed bindings instead of
eagerly loading and attaching everything.

**2. Module reloading.** Unlock the registry entry, re-evaluate the file,
re-attach. Essential for interactive development. The tricky part is what happens
to consumers that already imported old bindings (copy semantics means they're
stale). A simple version that just works for "reload and re-import at the REPL"
is fine as a starting point.

**3. Qualified access via `:as`.** `(import mod :as m)` that gives you `m/foo`.
This is syntactic sugar: compile `m/foo` as a lookup into the module's env (or
just desugar to `:prefix`). More ergonomic than `:prefix` because `:as` is a
familiar idiom. Needs a reader-level decision about whether `m/foo` is a single
symbol or new syntax.

**4. Enforce top-level-only imports.** Compile error if `import` appears inside
`lambda`, `let`, `if`, etc. Cheap to implement (check nesting depth during
compilation), prevents a class of confusing bugs, and matches Scheme's
strictness.

### Tier 2: Medium Value, Moderate Effort

**5. Re-export.** Let `(export ...)` include imported symbols (the explicit-export
path probably already works since it checks `exists(name, envir = module_env)`).
The fix for `export-all` is adding a `:re-export` modifier or a separate
`(re-export-from mod sym ...)` form. Enables facade/convenience modules.

**6. Hierarchical module names.** `(module (mylib utils) ...)` with names as
lists, mapped to directory paths. Registry keys become structured (e.g.,
`"mylib/utils"`). The main design question is path resolution conventions. Not
urgent while the only modules are the stdlib, but important as soon as users
write libraries.

**7. Private-by-default for `export-all`.** Add a naming convention (e.g., `_`
prefix) or a `(private ...)` declaration that `export-all` respects. Low effort,
reduces accidental exports.

### Tier 3: High Value, High Effort (Architectural)

**8. Reference-based imports.** Instead of copying values with `mget`/`list2env`,
make the module env accessible and resolve bindings through it. Options range
from lightweight (`makeActiveBinding` to create forwarding bindings) to
heavyweight (make module environments part of the parent chain). This is the
single change that would most improve the system — it makes reloading actually
work for consumers, softens circular dependency issues, and enables true
qualified access. But it touches the core binding model and has performance
implications.

**9. Circular dependency support.** Given reference-based imports, you could allow
cycles by registering a partially-initialized module env early (Python's
approach). Without reference-based imports, forward declarations or a two-phase
loading scheme would be needed. Not worth tackling before #8.

**10. Macro hygiene across modules.** The real fix is a hygienic expander
(`syntax-rules` or `syntax-case`). That's a major undertaking that would change
the macro system's fundamental character. The pragmatic alternative is
documenting the Common Lisp-style approach: macros should expand to calls to
exported functions, keeping the expanded code simple and the real logic in
closures (which close over the defining module's env). This sidesteps the
problem without solving it.

### Tier 4: Low Priority / Wait-and-See

**11. Conditional imports.** Useful but niche. Can be handled with a `(require
mod)` form that returns a boolean and does runtime loading, separate from the
compile-time `import`. Wait until there's a concrete use case.

**12. Multiple modules per file.** Nice to have, but one-per-file is a fine
convention shared by Python and Racket. The script-mode gap (top-level code
without `module` wrapper) is more pressing than multi-module files, and that
already works via `eval_text`.

### Architectural Note

Items 1, 2, 3, and 8 are connected: lazy loading, reloading, qualified access,
and reference-based imports all push toward **modules as first-class environment
objects that persist and are accessed directly**, rather than the current model of
**modules as bags of values that get copied into consumers**. If #8 is on the
horizon, it makes sense to implement 1–3 in a way that is compatible with that
future, even before tackling #8 itself.
