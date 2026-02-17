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

`.load_modules` iterates all modules in load order and attaches every one
into the engine env. Every Arl program pays the cost of loading the entire
stdlib, even if it only uses `car`. There is no lazy loading or autoload
mechanism.

### 8. No Re-Export / Facade Modules

There is no `(re-export ...)` pattern. If you want a convenience module that
bundles several others, you must manually import and re-define each binding.
`export-all` actively filters out imported symbols, preventing even the obvious
workaround.

### 9. Macro Scoping Across Modules

Arl's `defmacro` is Julia-style with a post-expansion hygienization pass that
renames macro-introduced local bindings with gensyms to avoid accidental capture.
This handles the classic hygiene problem (introduced locals shadowing user
bindings), but does *not* handle the cross-module scoping problem: when a macro's
expansion references free variables (helper functions, constants) from the
macro's defining module, those symbols don't exist in the caller's environment.

Closures sidestep this (they close over the defining module), but the expanded
*syntax* is just bare symbols that will be compiled and evaluated in the caller's
scope.

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

### Macro Scoping Across Modules

- **Scheme:** Hygienic macros (`syntax-rules`, `syntax-case`) are the crown
  jewel. Identifiers in expansions refer to bindings from where the macro was
  defined, not where it's used. The module system and macro system are deeply
  integrated.
- **R:** No macro system. Tidy evaluation / quosures are the closest analog.
- **Python:** No macro system.
- **Clojure:** Syntax-quote (backtick) resolves symbols to their
  namespace-qualified values at read time, so macro expansions automatically
  reference the defining namespace's bindings regardless of use site.

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
| Macro cross-mod scope  | Built-in | N/A        | N/A          | Partial      |
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

**5. Macro cross-module scoping via value splicing.** When a macro's expansion
references free variables from the defining module (unexported helpers, constants),
those symbols don't exist at the use site. Rather than implementing a full
hygienic expander (`syntax-rules` / `syntax-case`), this can be solved by
extending the existing hygienization pass to resolve introduced free-variable
references at expansion time.

The approach is a form of Bawden & Rees's "syntactic closures" (1988), similar to
what Clojure's syntax-quote does: for each symbol in the expansion that is
(a) macro-introduced (not from user arguments), (b) free (not locally bound
within the expansion), and (c) exists in the defining env — replace the bare
symbol with the actual R value (closure or constant) from the defining env.

R supports embedding actual function objects in call trees:
`eval(as.call(list(fn, 42)))` works when `fn` is a closure. The closure carries
its defining environment, so all its own free variables resolve correctly. No
wrapper lambda is needed, so evaluation order, control flow (`return`, `break`),
and conditional evaluation are all preserved — unlike the alternative of wrapping
the entire expansion in a closure, which forces eager evaluation of all arguments
and breaks control flow across the boundary.

Three practical concerns and their solutions:

1. **Debugging readability.** Spliced values make `macroexpand` output opaque.
   Fix: wrap spliced values in a thin S3 class (`arl_resolved_ref`) with an
   `arl_source_symbol` attribute recording the original name. A `print` method
   on this class restores readable output.

2. **Serialization/caching.** R's `saveRDS` has no customization hooks; it will
   try to serialize the entire environment chain of any embedded closure. Fix:
   the caching layer (which already does custom pre/post processing) deflates
   `arl_resolved_ref` objects to symbolic placeholders `(module_name,
   symbol_name)` before writing, and reinflates them against the module registry
   on cache load.

3. **Constants.** Non-function values (constants, data) can simply be spliced
   directly as literal values in the expansion.

This fits naturally into the existing architecture: the hygienization pass already
distinguishes "introduced" vs "call_site" symbols via `arl_syntax` wrappers and
already walks the expansion to rename introduced locals. Resolving introduced
free variables is a small extension to the same walk, not a new mechanism.

Note that eager value splicing and deferred env lookup are points on a continuum,
not fundamentally different approaches. Both replace a bare symbol with
"something that knows where to find the value" — they differ only in when the
lookup happens:

- **Eager (expansion-time):** resolve the value at expansion time, carry it in
  `arl_resolved_ref`. Faster at runtime (no lookup), but needs deflate/inflate
  for serialization.
- **Deferred (runtime):** carry `(module_name, symbol_name)` in the syntax,
  resolve at runtime via the module registry. Slower at runtime (one `get()` per
  reference per call), but caching is trivial since you're just storing strings.
- **Hybrid:** resolve eagerly for the in-memory representation (fast runtime),
  but have `arl_resolved_ref` also carry the symbolic name so it can deflate to
  the deferred representation on demand for caching. This is the recommended
  approach.

**6. Re-export.** Let `(export ...)` include imported symbols (the explicit-export
path probably already works since it checks `exists(name, envir = module_env)`).
The fix for `export-all` is adding a `:re-export` modifier or a separate
`(re-export-from mod sym ...)` form. Enables facade/convenience modules.

**7. Hierarchical module names.** `(module (mylib utils) ...)` with names as
lists, mapped to directory paths. Registry keys become structured (e.g.,
`"mylib/utils"`). The main design question is path resolution conventions. Not
urgent while the only modules are the stdlib, but important as soon as users
write libraries.

**8. Private-by-default for `export-all`.** Add a naming convention (e.g., `_`
prefix) or a `(private ...)` declaration that `export-all` respects. Low effort,
reduces accidental exports.

### Tier 3: High Value, High Effort (Architectural)

**9. Reference-based imports.** Instead of copying values with `mget`/`list2env`,
make the module env accessible and resolve bindings through it. Options range
from lightweight (`makeActiveBinding` to create forwarding bindings) to
heavyweight (make module environments part of the parent chain). This is the
single change that would most improve the system — it makes reloading actually
work for consumers, softens circular dependency issues, and enables true
qualified access. But it touches the core binding model and has performance
implications.

**10. Circular dependency support.** Given reference-based imports, you could
allow cycles by registering a partially-initialized module env early (Python's
approach). Without reference-based imports, forward declarations or a two-phase
loading scheme would be needed. Not worth tackling before #9.

### Tier 4: Low Priority / Wait-and-See

**11. Conditional imports.** Useful but niche. Can be handled with a `(require
mod)` form that returns a boolean and does runtime loading, separate from the
compile-time `import`. Wait until there's a concrete use case.

**12. Multiple modules per file.** Nice to have, but one-per-file is a fine
convention shared by Python and Racket. The script-mode gap (top-level code
without `module` wrapper) is more pressing than multi-module files, and that
already works via `eval_text`.

### Architectural Note

Items 1, 2, 3, and 9 are connected: lazy loading, reloading, qualified access,
and reference-based imports all push toward **modules as first-class environment
objects that persist and are accessed directly**, rather than the current model of
**modules as bags of values that get copied into consumers**. If #9 is on the
horizon, it makes sense to implement 1–3 in a way that is compatible with that
future, even before tackling #9 itself.

# Already addressed

o) prelude loading: refactor stdlib into prelude loaded by default always and a
broader set loaded only explicitly by import.

o) #1: prelude loading obviates the need for lazy stdlib loading because we're
not loading all of it by default anymore. startup cost is small enough now

#2: module reloading: this is implemented via the "(import <modname> :reload)"
form

o) #4: import position restrictions

o) #5: completed. cross-module macro scoping fixed so that macro expansions can
refer at execution time to objects contained in the lexical scope (i.e., the
defining module) of the macro

#6: reexport: completed. modules can re-export imported symbols and fusion
modules work

o) #9: reference-based imports via proxy environments with active bindings.

o) #10: circular dependency support — decided against. Arl imports have
compile-time effects (macro availability), so Python-style partial-module
imports don't generalize: macros from a partially-loaded module aren't available
during compilation of the importing module. Error messages would also degrade
(mysterious "object not found" instead of clear cycle detection errors), and the
only real cycle (list ↔ equality) is already cleanly resolved by having list use
raw R operators. If future cycles arise, a targeted mechanism (forward
declarations or extracting shared definitions into a third module) is preferred.

o) #8: private-by-default for export-all. Names starting with _ (single
underscore) are excluded from export-all. This is a convention (like Python's
_foo), separate from the .__ prefix which is reserved for Arl internals.
Explicit (export ...) can still export _-prefixed names if needed.

o) not in list: dynamic R package search path by default, configurable to a
fixed one. NOTE docs should mention getOption("defaultPackages") as a way to get
a nice minimal isolated reproducible configuration

o) #11: reserved special form for import-runtime (not implemented yet)

o) module naming / relation to files: addressed. Nameless modules derive name
from filename. Named modules register explicitly. One-module-per-file convention.

o) first-class module objects: addressed. `(import X)` binds the module env as
a first-class value. `:refer`/`:as` control how exports are accessed.

o) #3: qualified access: addressed. `/` syntax (`mod/sym`) desugars to
`module-ref`. Works with `:as` aliases.

o) #7: hierarchical module names: addressed. Namespace nodes support `a/b/c`
style hierarchical names.

## Remaining

    o) #12: multiple modules per file

Note: The comparison table above reflects the pre-overhaul state of Arl. Many
cells in the "Arl" column have improved (qualified access, re-export,
namespace hierarchy, private-by-default for export-all, etc.).
