# AGENTS.md

## Project Overview

Arl is a fully-functional Lisp dialect implemented in R with seamless R interoperability. The project leverages R's Scheme heritage to provide homoiconic syntax, powerful macros, and first-class functions while maintaining direct access to all R capabilities.

**Current status**: Experimental but feature-complete with comprehensive tests, documentation, and examples.

## Project Structure

Standard R package layout:
- `R/` - Core implementation (~8800 lines across 20 files)
  - `tokenizer.R` - Lexical analysis
  - `parser.R` - S-expression parsing to R calls
  - `compiler.R` - Compiles Arl AST to R code; handles all special forms
  - `runtime.R` - Runtime helpers (truthiness, assignment, stdlib functions)
  - `macro.R` - Macro expansion system
  - `quasiquote.R` - Shared quasiquote walker
  - `engine.R` - Engine R6 class (main entry point)
  - `env.R` - Arl environment management
  - `repl.R` - Interactive REPL
  - `cli.R` - Command-line interface
  - `help-system.R` - Help system
  - `doc-parser.R` - Documentation string parser
  - `module-cache.R` - Module caching
  - `module-registry.R` - Module registry and dependency tracking
  - `file-deps.R` - File dependency resolution
  - `topological-sort.R` - Topological sort for module loading order
  - `cells.R` - Mutable cell implementation
  - `coverage.R` - Code coverage support
  - `source-tracker.R` - Source location tracking
  - `utils.R` - Utilities
- `inst/` - Installed package files
  - `arl/` - Modular stdlib (~25 modules: core, control, functional, binding, looping, threading, error, struct, assert, r-interop, list, sequences, strings, math, logic, io, conversions, types, dict, display, equality, set, sort, _r); 11 "prelude" modules are loaded at startup (see `prelude-modules.txt`), others require explicit `(import ...)`
  - `examples/` - Complete working programs (fibonacci, quicksort, data-analysis, macros, fizzbuzz, graph-paths, log-parser, pipeline-macros, sales-report, task-runner)
  - `design-docs/` - Design documentation for internal subsystems
- `exec/arl` - Command-line executable (shell script wrapper)
- `tests/`
  - `testthat/` - Comprehensive test suite (60+ test files plus helpers)
  - `native/` - Native Arl test files (17 `.arl` tests run via `make test-native`)
- `benchmarks/` - Performance benchmarking suite (component benchmarks, profiling scripts)
- `tools/` - Build helpers (stdlib load-order generation, dependency analysis, CRAN submission tools, doc generators)
- `vignettes/` - User documentation (18 vignettes: getting-started, macros, modules, R interop, stdlib reference, internals, troubleshooting, etc.)
- `man/` - Roxygen2 documentation
- `DESCRIPTION` - Package metadata
- `NAMESPACE` - Package exports
- `Makefile` - Development commands

## Development Commands

**Always use `make` targets** rather than running `Rscript`, `R -q -e`, or `devtools::*` commands directly. The Makefile targets handle prerequisite steps (clearing `.arl_cache`, rebuilding `load-order.rds`, calling `devtools::load_all()`) that are easy to forget when running commands by hand. Skipping these steps can lead to testing against stale code.

Use `make help` to see all available targets. Key targets include:

```bash
make help              # Show all available targets
make install           # Install the package
make test              # Run testthat test suite
make test-file FILE=test-parser  # Run a single test file
make test-native       # Run native .arl tests
make lint              # Run lintr checks
make document          # Generate all documentation
make build             # Build package tarball
make check             # R CMD check
make bench             # Run benchmarks
make coverage          # Generate coverage reports
make cran-prep         # Prepare for CRAN submission
make clean             # Cleanup build artifacts
```

## Architecture

Arl leverages R's existing eval/quote/environment system rather than reimplementing everything:

1. **Tokenizer** (`tokenizer.R`) - Lexical analysis producing tokens (LPAREN, SYMBOL, NUMBER, STRING, etc.)

2. **Parser** (`parser.R`) - Converts Arl S-expressions to R calls; expands quote/quasiquote sugar (`'`, `` ` ``, `,`, `,@`) into explicit forms during parsing

3. **Macro Expander** (`macro.R`) - Processes `defmacro` definitions; supports quasiquote with unquote/unquote-splicing; provides `macroexpand` and `macroexpand-1`

4. **Compiler** (`compiler.R`) - Compiles Arl AST to R code; handles all special forms (`quote`, `if`, `define`, `lambda`, `begin`, `defmacro`, `quasiquote`, `while`, `set!`, etc.); implements self-tail-call optimization. The compiled R code is then evaluated via R's native `eval()`

5. **Runtime** (`runtime.R`) - Runtime helpers used by compiled code: truthiness predicate (`.__true_p`), pattern assignment, stdlib functions (list ops, higher-order functions, predicates, etc.)

6. **Standard Library** (`runtime.R`, `inst/arl/*.arl`) - Core library in R (runtime helpers); modular extensions in Arl. **Module system**: `(import M)` attaches M's exports only in the scope where import was evaluated; each module is loaded once per engine (shared cache). `(load path)` / `(load path env)` is a built-in function that evaluates a file in the target env. `(run path [parent])` is a stdlib convenience that evaluates in `new.env(parent = parent)`. From R, use `load_file_in_env(path)` for source-like visibility, or pass an explicit child env for isolation.

7. **R Bridge** - Direct access to all R functions; keywords (`:name`) become named arguments; R operators and data structures work naturally

8. **REPL** (`repl.R`) - Interactive shell with error handling and history

9. **CLI** (`cli.R`, `exec/arl`) - Command-line tool supporting `--file`, `--eval`, positional script execution, and interactive mode

## Language Design

**Semantics**: Scheme-like with R integration
- **Truthiness**: `#f`/`FALSE`, `#nil`/`NULL`, and `0` are falsey
- **Lists**: Backed by R calls/lists; `car` returns head, `cdr` returns tail
- **Keywords**: `:kw` tokens self-evaluate and become named arguments
- **Special forms**: `quote`, `quasiquote`, `if`, `begin`, `define`, `set!`, `lambda`, `defmacro`, `and`, `or`, `while`, `delay`, `import`, `module`
- **Scoping**: Lexical scoping via R environments
- **Macros**: Compile-time code transformation with quasiquote/unquote
- **Tail call optimization**: Self-TCO is implemented by the compiler for `(define name (lambda ...))` patterns -- self-tail-calls through `if`/`begin`/`cond`/`let`/`let*`/`letrec` are rewritten as loops. `loop`/`recur` is still useful for mutual recursion.

**Reserved names**: Names starting with `.__` (dot-underscore-underscore) are reserved for internal use. The leading `.` hides them from `ls()` and the `__` signals internal machinery. User code cannot `define` or `set!` these names -- the compiler rejects them at compile time and the runtime rejects them as a fallback. All runtime helpers (`.__env`, `.__true_p`, `.__assign_pattern`, ...), compiler gensyms (`.__tmp__N`, `.__tco_<param>`, ...), sentinels (`.__helpers_installed`, `.__module`, ...), and registry bindings (`.__macros`, `.__module_registry`) use this prefix. The `arl_` prefix is reserved separately for S3 attributes/classes (`arl_src`, `arl_closure`, etc.) and filesystem paths (`.arl_cache`, `.arl_history`).

**R Interoperability**:
- All R functions callable directly: `(mean (c 1 2 3))` → `3`
- Named arguments via keywords: `(seq :from 1 :to 10 :by 2)`
- R operators accessible: `($)`, `([)`, etc.
- R data structures native: `(data.frame :x (c 1 2 3))`
- Formulas: `(~ y x)` for modeling
