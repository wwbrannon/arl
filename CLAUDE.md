# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Rye is a Lisp dialect implemented in and with access to R. The project is in early development.

## Project Structure

This is an R package project. When implemented, expect:
- `R/` - R source files
- `src/` - C/C++ source if needed for performance
- `DESCRIPTION` - R package metadata
- `NAMESPACE` - R package exports
- `tests/` - Test suite (likely using testthat)

## Development Commands

Common development tasks are available via the Makefile. Run `make help` to see all available commands:

```bash
# Show all available commands
make help

# Install dependencies
make install

# Run tests
make test

# Run a single test file
make test-file FILE=test-parser

# Generate roxygen2 documentation
make document

# Build and check package
make build
make check
```

## Architecture

The implementation will require these core components:

1. **Parser** - Tokenize and parse Lisp S-expressions into R data structures
2. **Evaluator** - Execute Lisp expressions, handling special forms and function application
3. **Environment** - Manage lexical scoping and variable bindings
4. **R Bridge** - Seamless interop between Lisp and R (calling R functions, accessing R objects)

Key design considerations:
- Lisp lists can map to R's pairlist or list structures
- R's first-class functions enable implementing Lisp lambdas naturally
- Must decide between Common Lisp vs Scheme semantics for core features
