# Register the Arl knitr engine

Call this function in a vignette's setup chunk to enable `{arl}` code
chunks. The engine evaluates Arl code using a shared `Engine` instance
that persists across chunks within a single document.

## Usage

``` r
register_knitr_engine()
```

## Value

NULL (invisibly); called for its side effect of registering the engine.

## Examples

``` r
# \donttest{
# In a vignette setup chunk:
arl::register_knitr_engine()
# }
```
