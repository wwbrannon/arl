# Documenting Functions and Macros

Arl has two ways to attach documentation to functions and macros: `;;'`
annotation comments (used at definition time) and the `doc!` builtin
(used at runtime). Both produce the same `arl_doc` attribute, which
`help` and `doc` read.

## Annotation comments (`;;'`)

Annotation comments use the `;;'` prefix and support roxygen-like tags.
Place them immediately before a `define` or `defmacro`:

    arl> ;;' @description Compute the square of x.
    arl> ;;' @examples
    arl> ;;' (square 3)   ; => 9
    arl> ;;' (square -2)  ; => 4
    arl> ;;' @seealso cube, expt
    arl> ;;' @note Pure function; no side effects.
    arl> (define square
    arl>   (lambda (x) (* x x)))
    #> <function>

The same syntax works for macros:

    arl> ;;' @description Execute body only when condition is false.
    arl> ;;' @examples
    arl> ;;' (unless #f (display "ran\n"))
    arl> (defmacro unless (condition . body)
    arl>   `(if (not ,condition) (begin ,@body)))

### Supported tags

| Tag            | Purpose                                                      |
|----------------|--------------------------------------------------------------|
| `@description` | One-line or multi-line description.                          |
| `@examples`    | One or more example expressions (one per continuation line). |
| `@seealso`     | Comma-separated list of related function names.              |
| `@note`        | Additional notes (caveats, performance, history).            |
| `@signature`   | Override the auto-detected call signature.                   |
| `@section`     | Section heading for grouping functions in generated docs.    |

Multi-line content is written with continuation `;;'` lines:

``` arl
;;' @description Concatenate strings with a separator.
;;'   Accepts any number of string arguments.
;;' @examples
;;' (string-join ", " '("a" "b" "c"))  ; => "a, b, c"
;;' (string-join "" '("x" "y"))        ; => "xy"
(define string-join ...)
```

The compiler reads `;;'` blocks at compile time and bakes the resulting
documentation into the definition, so no runtime overhead is incurred.
This is the recommended way to document functions, macros, and module
exports.

## Runtime documentation with `doc!`

`doc!` attaches documentation fields to an already-defined function or
macro. It is useful for interactive work, dynamically generated
definitions, or adding fields incrementally:

    arl> (define double (lambda (x) (* x 2)))
    #> <function>

    arl> ; Set just the description
    arl> (doc! double "Double the input value.")
    #> <function>

    arl> ; Set specific fields with keyword arguments
    arl> (doc! double :examples "(double 3) ; => 6")
    #> <function>
    arl> (doc! double :note "Pure function.")
    #> <function>

    arl> ; Set multiple fields at once
    arl> (doc! double :description "Double x." :examples "(double 5) ; => 10")
    #> <function>

`doc!` **merges** with existing documentation — setting `:examples` does
not erase a previously set `:description`.

## Retrieving documentation with `doc`

`doc` reads fields from a function’s documentation:

    arl> (doc double)              ; => "Double x." (description, the default)
    #> "Double x."
    arl> (doc double "examples")   ; => "(double 5) ; => 10"
    #> "(double 5) ; => 10"
    arl> (doc double "note")       ; => "Pure function."
    #> "Pure function."
    arl> (doc double "all")        ; => named list of all fields
    #> (:description "Double x." :examples "(double 5) ; => 10" :note "Pure function.")

## Which mechanism to use

| Situation                                            | Use                      |
|------------------------------------------------------|--------------------------|
| Defining a function or macro in a `.arl` source file | `;;'` annotations        |
| Attaching docs interactively in the REPL             | `doc!` or `;;'`          |
| Building docs dynamically from data                  | `doc!` with keyword args |
| Reading docs programmatically                        | `doc`                    |
| Looking up docs as a user                            | `help`                   |

Both `;;'` and `doc!` produce the same `arl_doc` attribute, so `help`
and `doc` work identically regardless of how the documentation was
attached.
