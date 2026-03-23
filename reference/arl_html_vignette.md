# HTML vignette format with Arl syntax highlighting

A wrapper around
[`html_vignette`](https://pkgs.rstudio.com/rmarkdown/reference/html_vignette.html)
that passes `--syntax-definition` to Pandoc so that `{arl}` code blocks
receive proper syntax highlighting.

## Usage

``` r
arl_html_vignette(pandoc_args = NULL, check_title = TRUE, ...)
```

## Arguments

- pandoc_args:

  Additional Pandoc arguments (merged with the syntax-definition
  argument added automatically).

- check_title:

  Whether to check that the vignette title matches the
  `VignetteIndexEntry`. Set to `FALSE` for pkgdown-only articles that
  intentionally omit a `VignetteIndexEntry`.

- ...:

  Arguments passed to
  [`html_vignette`](https://pkgs.rstudio.com/rmarkdown/reference/html_vignette.html).

## Value

An R Markdown output format object.
