# Run the Arl CLI

Entry point for the Arl command-line interface. Parses arguments and
runs the requested action (REPL, file evaluation, or expression
evaluation).

## Usage

``` r
cli(args = commandArgs(trailingOnly = TRUE))
```

## Arguments

- args:

  Command-line arguments to parse (defaults to
  `commandArgs(trailingOnly = TRUE)`).

## Value

Invisibly returns `NULL`.
