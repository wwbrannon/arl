# Contributing to Arl

Thanks for your interest in Arl. This project is experimental and welcomes
issues, examples, and documentation improvements.

## Getting started

1. Fork and clone the repository.
2. Install dependencies:

```bash
make install
```

3. Run the test suite:

```bash
make test
```

## Architecture overview

Arl is a Lisp dialect that compiles to R code and evaluates it with R's
native `eval()`. The pipeline is:

```
Source -> Tokenizer -> Parser -> Macro Expander -> Compiler -> R eval()
```

For details, see the [Compiler and Internals](articles/internals.html)
vignette and [AGENTS.md](AGENTS.md) for the full project structure.

Key source files:

| File | Purpose |
|------|---------|
| `R/tokenizer.R` | Regex-based lexer |
| `R/parser.R` | S-expression parser |
| `R/macro.R` | Macro expansion |
| `R/compiler.R` | AST-to-R compilation, TCO, optimizations |
| `R/runtime.R` | Runtime helpers and base stdlib |
| `R/engine.R` | `Engine` R6 class (main entry point) |
| `R/env.R` | Environment management |
| `inst/arl/*.arl` | Modular stdlib (Arl source) |

## Code style

- R6 classes use `public`/`private` with `private$` naming.
- Internal classes use comment-doc format (not roxygen) for private methods.
- Prefer clear, descriptive names; keep functions small and testable.
- Run `make lint` to check for lint issues.

## Tests

Arl has two test suites:

- **R tests** (`tests/testthat/test-*.R`) -- test engine internals, R interop,
  and comprehensive edge cases using testthat.
- **Native tests** (`tests/native/test-*.arl`) -- test Arl language semantics
  and stdlib from the user perspective, using Arl's own `assert` module.

See [tests/README.md](tests/README.md) for the full test guide, including
when to write R tests vs native tests and how to avoid duplication.

Run tests with:

```bash
make test                        # all tests
make test-file FILE=test-parser  # single R test file
make test-native FILE=test-sort  # single native test file
```

## Documentation workflow

```bash
make devdoc    # regenerate roxygen man pages
make readme    # render README.Rmd -> README.md
make site      # build pkgdown site
make document  # all of the above plus vignettes
```

- Edit `README.Rmd` (not `README.md` directly), then run `make readme`.
- Vignettes live in `vignettes/`.
- Man pages are generated from roxygen comments in `R/`.
- The pkgdown config is `_pkgdown.yml`.

## Adding a stdlib function

1. **Define** the function in the appropriate `inst/arl/*.arl` module
   (or create a new module if it's a new topic).
2. **Export** it from the module's `(export ...)` list.
3. **Document** it with `;;'` annotation comments immediately before the
   `define`. Include `@description`, `@examples`, and `@seealso` at
   minimum. See [Documenting functions](getting-started.html#documenting-functions)
   for the full tag reference.
4. **Regenerate vignettes**: run `make lang-docs` to update the
   auto-generated `vignettes/lang-*.Rmd` files (do not edit them by hand).
5. **Write tests**: native test for idiomatic usage, R test for edge cases.
6. **Rebuild stdlib cache**: `make stdlib-order` (run before `make install`).

## Adding examples

Examples live in `inst/examples/`. Keep them small and focused, and include
brief comments explaining what they demonstrate.

## Development commands

Run `make help` to see all available commands. Key ones:

```bash
make install       # install the package
make test          # run all tests
make check         # R CMD check --as-cran
make coverage      # run R + Arl coverage analysis
make bench         # run benchmarks
make document      # generate all documentation
make stdlib-order  # print stdlib load order
make clean         # remove build artifacts
```

## Issues and pull requests

Please include a clear description of the change and a short test plan.
