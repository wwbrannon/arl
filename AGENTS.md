# AGENTS.md

## Project Overview

Rye is a fully-functional Lisp dialect implemented in R with seamless R interoperability. The project leverages R's Scheme heritage to provide homoiconic syntax, powerful macros, and first-class functions while maintaining direct access to all R capabilities.

**Current status**: Experimental but feature-complete with comprehensive tests, documentation, and examples.

## Project Structure

Standard R package layout:
- `R/` - Core implementation (~3000 lines)
  - `tokenizer.R` - Lexical analysis
  - `parser.R` - S-expression parsing to R calls
  - `eval.R` - Evaluator with special forms
  - `macro.R` - Macro expansion system
  - `stdlib.R` - Standard library (list ops, higher-order functions, predicates, etc.)
  - `translator.R` - R-to-Rye syntax translator
  - `repl.R` - Interactive REPL
  - `cli.R` - Command-line interface
  - `help.R` - Help system
  - `utils.R` - Utilities
- `inst/` - Installed package files
  - `rye/` - Modular stdlib extensions (control.rye, binding.rye, looping.rye, threading.rye, error.rye)
  - `examples/` - Complete working programs (fibonacci, quicksort, data-analysis, macros, etc.)
  - `cli/rye` - Command-line executable
- `tests/testthat/` - Comprehensive test suite (16 test files)
- `vignettes/` - User documentation (getting-started, macros, R interop, stdlib reference, etc.)
- `man/` - Roxygen2 documentation
- `DESCRIPTION` - Package metadata
- `NAMESPACE` - Package exports
- `Makefile` - Development commands

## Development Commands

Common development tasks via Makefile:

```bash
# Show all available commands
make help

# Install package
make install

# Run tests
make test

# Run a single test file
make test-file FILE=test-parser

# Generate documentation (roxygen2, README, vignettes, pkgdown)
make document

# Build and check package
make build
make check

# CRAN preparation
make cran-prep
make cran-check
make cran-comments

# Cleanup
make clean
make cran-clean
```

## Architecture

Rye leverages R's existing eval/quote/environment system rather than reimplementing everything:

1. **Tokenizer** (`tokenizer.R`) - Lexical analysis producing tokens (LPAREN, SYMBOL, NUMBER, STRING, etc.)

2. **Parser** (`parser.R`) - Converts Rye S-expressions to R calls; expands quote/quasiquote sugar (`'`, `` ` ``, `,`, `,@`) into explicit forms during parsing

3. **Macro Expander** (`macro.R`) - Processes `defmacro` definitions; supports quasiquote with unquote/unquote-splicing; provides `macroexpand` and `macroexpand-1`

4. **Evaluator** (`eval.R`) - Handles special forms (`quote`, `if`, `define`, `lambda`, `begin`, `defmacro`, `quasiquote`, `~`); delegates non-special-form evaluation to R's native `eval()`

5. **Standard Library** (`stdlib.R`, `inst/rye/*.rye`) - Core library (list operations, higher-order functions, predicates, string/I/O, error handling) implemented in R; modular extensions (control flow, binding, looping, threading, error handling) implemented in Rye

6. **R Bridge** - Direct access to all R functions; keywords (`:name`) become named arguments; R operators and data structures work naturally

7. **REPL** (`repl.R`) - Interactive shell with error handling and history

8. **CLI** (`cli.R`, `inst/cli/rye`) - Command-line tool supporting `--file`, `--eval`, positional script execution, and interactive mode

## Language Design

**Semantics**: Scheme-like with R integration
- **Truthiness**: Only `#f`/`FALSE` and `#nil`/`NULL` are falsey
- **Lists**: Backed by R calls/lists; `car` returns head, `cdr` returns tail
- **Keywords**: `:kw` tokens self-evaluate and become named arguments
- **Special forms**: `quote`, `if`, `define`, `lambda`, `begin`, `defmacro`, `quasiquote`, `~`
- **Scoping**: Lexical scoping via R environments
- **Macros**: Compile-time code transformation with quasiquote/unquote

**R Interoperability**:
- All R functions callable directly: `(mean (c 1 2 3))` → `3`
- Named arguments via keywords: `(seq :from 1 :to 10 :by 2)`
- R operators accessible: `($)`, `([)`, etc.
- R data structures native: `(data.frame :x (c 1 2 3))`
- Formulas: `(~ y x)` for modeling
