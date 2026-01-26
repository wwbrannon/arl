# Contributing to Rye

Thanks for your interest in Rye. This project is experimental and welcomes
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

## Documentation workflow

- Edit `README.Rmd`, then regenerate `README.md`.
- Vignettes live in `vignettes/`.
- Man pages are generated from roxygen comments in `R/`:

```bash
make document
```

## Adding examples

Examples live in `inst/examples/`. Keep them small and focused, and include
brief comments explaining what they demonstrate.

## Style

- Prefer clear, descriptive names.
- Keep functions small and testable.
- Add tests for new behavior when possible.

## Issues and pull requests

Please include a clear description of the change and a short test plan.
