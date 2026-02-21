# Module System Design

## Overview

Arl's module system provides encapsulated, reusable code units with explicit
exports, qualified access, and a shared registry. Modules are loaded once per
engine, cached as compiled expressions, and attached into scopes via proxy
environments or direct ("squash") bindings.

## Environment Chain

The engine constructs a layered environment chain:

```
engine_env          ← user definitions, REPL bindings
  ↓ parent
prelude_env         ← exports from 10 prelude modules (squashed)
  ↓ parent
builtins_env        ← =, ==, !=, not, +, -, *, /, .__import, .__module, etc.
  ↓ parent
[R package chain]   ← stats, utils, base, etc. (from search() or pinned set)
  ↓ parent
baseenv()
```

### Design Rationale

- **Prelude separation.** `prelude_env` sits between `engine_env` and
  `builtins_env`, isolating prelude bindings from user code while making them
  available via inheritance.
- **Builtins isolation.** `builtins_env` contains Arl operators that differ
  from R semantics (R's `=` is assignment; Arl's `=` is comparison). Placing
  these below the prelude keeps them accessible everywhere.
- **Module parents.** Module environments parent to `prelude_env` (or
  `builtins_env` if prelude is not loaded), **not** to `engine_env`. This
  enforces proper modularity: modules get core stdlib but not user definitions.
  Dependencies must be explicitly imported.
- **Registry placement.** `.__macros` and `.__module_registry` live in
  `builtins_env` so all inheriting environments find them via `inherits=TRUE`.

## Prelude Loading

The 10 prelude modules (listed in `inst/arl/prelude-modules.txt`) are loaded
during `Engine$new()` when `load_prelude = TRUE` (the default):

```
logic, core, types, list, equality, functional,
control, sequences, binding, threading
```

Prelude loading uses **squash mode**: exports are dumped directly as active
bindings into `prelude_env`, with no proxy environments in the parent chain.
This is simpler and avoids extra environment objects for bindings that are
always present.

Non-prelude modules (`math`, `looping`, `sort`, `strings`, `dict`, `set`,
`io`, `display`, `assert`, `struct`, `conversions`, `r-interop`, `_utils`)
require explicit `(import ...)`.

## Import Modes: Squash vs Proxy

### Squash Mode (Prelude Only)

Creates active bindings **directly in the target environment**. Each binding
forwards reads to the module's export. Macros are stored in the target env's
own `.__macros` registry.

**Advantages:** Simple, no extra environment objects.
**Disadvantages:** Cannot reload without clearing and recreating all bindings.

### Proxy Mode (Default for Non-Prelude)

Creates a new **proxy environment** spliced into the target env's parent
chain. The proxy contains active bindings that forward reads to the module
env, plus a `.__macros` registry with active bindings for exported macros.
The proxy is marked with `.__import_proxy=TRUE` and
`.__import_module_name`.

**Advantages:** Supports dynamic reload — proxy bindings automatically pick up
new values. Clean separation from the target env's own bindings.
**Idempotency:** Before creating a proxy, the system walks the parent chain
to check for an existing proxy for the same module. Duplicate proxies are not
created.

## Module Registry

The `ModuleRegistry` (`module-registry.R`) is an R6 class wrapping a locked
environment. Each module entry is a **locked environment** (not a plain list)
containing:

| Field | Type | Description |
|---|---|---|
| `env` | environment | The module's execution environment |
| `exports` | character | Vector of exported symbol names |
| `path` | character | Absolute filesystem path to source |

### Keying

A single module can be registered under multiple keys:
- **Module name** (e.g., `"math"`)
- **Absolute path alias** (e.g., `"/path/to/inst/arl/math.arl"`)

`register(name, env, exports)` creates the primary entry. `alias(path, name)`
adds a path key pointing to the same entry. This ensures that importing the
same module by name or by path reuses the same instance.

### Key Operations

- `exists(name)` / `get(name)` — lookup
- `update_exports(name, exports)` — update after reload (recreates locked env,
  updates all keys)
- `rebuild_proxies(name, engine_env)` — after reload, walk all known
  environments and update their proxy bindings
- `attach(name, squash)` / `attach_into(name, target_env, ...)` — bind
  module exports into a target environment

## The `import` Special Form

### Compilation

The compiler (`compile_import`) enforces that `import` only appears at the
top level (`nesting_depth == 0`). It parses keyword modifiers and compiles
to a call to the `.__import` runtime function:

```r
.__import(quote(module_name), .__env,
          rename = ..., refer = ..., as_alias = ..., reload = ...)
```

The module name is **quoted** (not evaluated) to prevent variable lookup.

### Supported Forms

```lisp
(import X)                          ; bind module env as X; qualified access only
(import X :refer :all)              ; dump all exports + bind module env
(import X :refer (sym1 sym2))       ; import specific exports only
(import X :as alias)                ; bind module as alias
(import X :as alias :refer :all)    ; both
(import X :rename ((old new) ...))  ; rename on import
(import X :reload)                  ; force reload from source
(import "path/to/file.arl")        ; import by file path
```

### Runtime Behavior

`import_compiled()` in `runtime.R` executes the import:

1. **Resolve.** Determine registry key: module name → stdlib directory then
   CWD; string path → normalize to absolute.
2. **Cycle detection.** Check `context$loading_modules` stack. If the module
   is already being loaded, error with the dependency chain.
3. **Load if needed.** If not in registry, load the source file via
   `load_file_in_env()`. The file must contain a `(module ...)` form that
   calls `.__module()` to register itself.
4. **Bind module.** Unless squashing, assign the module environment to a local
   name (e.g., `math`) for qualified access.
5. **Create namespace.** For hierarchical names containing `/`, create
   namespace nodes (locked environments with class `arl_namespace`).
6. **Attach exports.** Call `attach_into()` to create proxy or squash
   bindings based on `:refer` and mode.

## Qualified Access

After `(import math)`, exports are accessed via `math/sin`. The parser
desugars `/` into `(module-ref math sin)`.

The `module-ref` builtin handles two cases:
- **Module environment:** Direct lookup in the module's immediate bindings
  (not inherited). Errors if the symbol is not exported.
- **Namespace node:** Extends the prefix and looks up in the registry.
  `(module-ref collections sorted-set)` looks up `"collections/sorted-set"`.

## Binding Collision Handling

When `(import dict :refer :all)` would create a module binding named `dict`
that shadows the exported function `dict`, the module binding is skipped.
The check occurs after the export list is processed to avoid cascading
problems. This means the function `dict` remains accessible unqualified,
while `dict/keys`, `dict/values`, etc. still work for qualified access.

## Re-Exports

Modules can re-export imported symbols:

```lisp
(module collections
  (export-all :re-export)
  (import list :refer :all)
  (import dict :refer :all))
```

`finalize_module_env` walks the parent chain looking for `.__import_proxy`
markers to collect imported names, then creates forwarding bindings in the
module's export set.

## Circular Dependency Detection

### Early Registration

Modules register in the registry **before** body evaluation completes. This
allows subsequent imports within the same compilation unit to find the module.

### Cycle Detection

The `loading_modules` stack tracks modules currently being loaded. Before
loading a new module, the runtime checks if it is already in the stack:

```
(import a) → push "a"
  a's body: (import b) → push "b"
    b's body: (import a) → "a" already in stack → error
```

The error message includes the full dependency chain:
`"Circular dependency: a -> b -> a"`.

### Known Cycle: list ↔ equality

The `list` and `equality` modules have a natural circular dependency (equality
needs list predicates, list comparison needs equality). This is resolved by
having `list.arl` use R's `identical()` and the builtin `=` instead of
importing `equal?` from `equality`.

## Module Body Evaluation

Module bodies use **interleaved compile/eval**: each expression is
macro-expanded, compiled, and evaluated before the next expression is
processed. This is critical for macro-based modules where an early `(import X)`
defines macros used by subsequent code in the same module.

### Module Environment Setup

1. New environment created with `prelude_env` as parent.
2. `.__module=TRUE` marker assigned and locked.
3. `install_helpers()` adds `.__import`, `.__module`, `.__assign_pattern`,
   and other internal functions.
4. Macro registry (`.__macros`) created in the module env.

### Finalization

After body evaluation:
1. Non-internal bindings are locked (prevent accidental mutation from outside).
2. Re-export forwardings created if `:re-export` was specified.
3. `export-all` symbol set collected (excluding `_`-prefixed private names
   and imported symbols).
4. Registry updated with final exports list.

## Module Caching

Compiled module expressions are cached under `tools::R_user_dir("arl", "cache")/modules/` (see `cache-invalidation.md` for details). Each source directory gets a unique cache subdirectory keyed by its path hash. Key points for the module system:

- **Deflation:** Before serialization, tagged function closures (marked
  `arl_resolved_from`) are replaced with symbolic placeholders
  `.__resolve_ref(module, symbol)`.
- **Inflation:** After loading, placeholders are resolved back to actual
  values from the module registry.
- **Coverage interaction:** Cache is bypassed when coverage is active, since
  instrumented code differs from non-instrumented code.

## File Dependency Resolution

`FileDeps` (`file-deps.R`) performs static analysis of a directory:

1. Parse each `.arl` file for `(module ...)`, `(export ...)`, and
   `(import ...)` forms.
2. Build a dependency graph: vertices = module names, edges from imports.
3. Apply topological sort (Kahn's algorithm via `topological-sort.R`).
4. Output: `inst/arl/load-order.txt`, the order in which stdlib modules
   should be loaded.

Regenerate with `make stdlib-order` after changing module imports.
