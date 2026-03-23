# R6 class for tracking and reporting Arl code execution coverage

Tracks which lines of .arl source files actually execute during runtime.
Maintains execution counts per file/line and generates reports. Supports
flexible configuration for tracking custom directories, test files, and
custom comment syntax.

## Note

This class is exported for use by advanced tooling (CI scripts, IDE
plugins, etc.) and for testing purposes. Its API should be considered
**internal** and subject to change without notice. Most users should
interact with coverage through the
[Engine](https://willbrannon.com/arl/reference/Engine.md) methods
`enable_coverage()`, `disable_coverage()`, `get_coverage()`, and
`reset_coverage()` instead.

## Public fields

- `coverage`:

  Environment mapping "file:line" keys to execution counts

- `enabled`:

  Logical flag to enable/disable tracking

- `all_files`:

  Character vector of all .arl files being tracked

- `code_lines`:

  Environment mapping file paths to integer vectors of code line numbers

- `coverable_lines`:

  Environment mapping file paths to integer vectors of AST-derived
  coverable line numbers

## Methods

### Public methods

- [`CoverageTracker$new()`](#method-ArlCoverageTracker-new)

- [`CoverageTracker$track()`](#method-ArlCoverageTracker-track)

- [`CoverageTracker$register_coverable()`](#method-ArlCoverageTracker-register_coverable)

- [`CoverageTracker$get_summary()`](#method-ArlCoverageTracker-get_summary)

- [`CoverageTracker$discover_files()`](#method-ArlCoverageTracker-discover_files)

- [`CoverageTracker$reset()`](#method-ArlCoverageTracker-reset)

- [`CoverageTracker$set_enabled()`](#method-ArlCoverageTracker-set_enabled)

- [`CoverageTracker$report_console()`](#method-ArlCoverageTracker-report_console)

- [`CoverageTracker$report_html()`](#method-ArlCoverageTracker-report_html)

- [`CoverageTracker$report_json()`](#method-ArlCoverageTracker-report_json)

- [`CoverageTracker$clone()`](#method-ArlCoverageTracker-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize the coverage tracker

#### Usage

    CoverageTracker$new(
      search_paths = NULL,
      include_tests = FALSE,
      path_strip_patterns = NULL,
      output_prefix = "arl",
      report_title = "Arl Code Coverage",
      code_line_pattern = "^\\s*[^[:space:];]"
    )

#### Arguments

- `search_paths`:

  Character vector of directories to search for .arl files (NULL = use
  stdlib)

- `include_tests`:

  Whether to include test files in coverage tracking (default: FALSE)

- `path_strip_patterns`:

  Custom regex patterns for stripping paths in reports (NULL = use
  defaults)

- `output_prefix`:

  Subdirectory name for report outputs (default: "arl")

- `report_title`:

  Title to use in coverage reports (default: "Arl Code Coverage")

- `code_line_pattern`:

  Regex pattern to identify code lines vs comments/blanks

------------------------------------------------------------------------

### Method `track()`

Track execution of an expression with source info

#### Usage

    CoverageTracker$track(arl_src)

#### Arguments

- `arl_src`:

  Source information object with file, start_line, end_line

------------------------------------------------------------------------

### Method `register_coverable()`

Register coverable lines from an instrumented source range

#### Usage

    CoverageTracker$register_coverable(file, start_line, end_line)

#### Arguments

- `file`:

  Source file path

- `start_line`:

  Start line of the instrumented form

- `end_line`:

  End line of the instrumented form

------------------------------------------------------------------------

### Method `get_summary()`

Get coverage summary as list: file -\> line -\> count

#### Usage

    CoverageTracker$get_summary()

------------------------------------------------------------------------

### Method `discover_files()`

Discover all .arl files to track

Searches for .arl files in configured search paths or stdlib by default.
By default excludes test files unless include_tests = TRUE.

#### Usage

    CoverageTracker$discover_files()

------------------------------------------------------------------------

### Method `reset()`

Reset coverage data

#### Usage

    CoverageTracker$reset()

------------------------------------------------------------------------

### Method `set_enabled()`

Enable/disable tracking

#### Usage

    CoverageTracker$set_enabled(enabled)

#### Arguments

- `enabled`:

  Logical value to enable (TRUE) or disable (FALSE) coverage tracking

------------------------------------------------------------------------

### Method `report_console()`

Generate console coverage report

#### Usage

    CoverageTracker$report_console(output_file = NULL)

#### Arguments

- `output_file`:

  Optional file to write report to (default: console only)

------------------------------------------------------------------------

### Method `report_html()`

Generate HTML coverage report

#### Usage

    CoverageTracker$report_html(output_file)

#### Arguments

- `output_file`:

  Path to output HTML file (required)

------------------------------------------------------------------------

### Method `report_json()`

Generate codecov-compatible JSON format

#### Usage

    CoverageTracker$report_json(output_file)

#### Arguments

- `output_file`:

  Path to output JSON file (required)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CoverageTracker$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# \donttest{
# Track coverage of a single stdlib file (logic.arl)
tracker <- CoverageTracker$new()
engine <- Engine$new(coverage_tracker = tracker, load_prelude = FALSE)
logic_file <- system.file("arl", "logic.arl", package = "arl")
engine$load_file_in_env(logic_file, engine$get_env())
#> function (a, b) 
#> {
#>     .__coverage_track("/tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/logic.arl", 
#>         75L, 75L)
#>     if (.__true_p(a)) {
#>         .__coverage_track("/tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/logic.arl", 
#>             76L, 76L)
#>         if (.__true_p(b)) {
#>             .__coverage_track("/tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/logic.arl", 
#>                 76L, 76L)
#>             FALSE
#>         }
#>         else {
#>             .__coverage_track("/tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/logic.arl", 
#>                 76L, 76L)
#>             TRUE
#>         }
#>     }
#>     else {
#>         .__coverage_track("/tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/logic.arl", 
#>             77L, 77L)
#>         if (.__true_p(b)) {
#>             .__coverage_track("/tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/logic.arl", 
#>                 77L, 77L)
#>             TRUE
#>         }
#>         else {
#>             .__coverage_track("/tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/logic.arl", 
#>                 77L, 77L)
#>             FALSE
#>         }
#>     }
#> }
#> <environment: 0x55d8a25d94b8>
#> attr(,"arl_doc")
#> attr(,"arl_doc")$description
#> [1] "Logical exclusive OR with Arl truthiness."
#> 
#> attr(,"arl_doc")$signature
#> [1] "(xor a b)"
#> 
#> attr(,"arl_doc")$examples
#> [1] "(xor #t #f)       ; => #t\n(xor #f #t)       ; => #t\n(xor #t #t)       ; => #f\n(xor #f #f)       ; => #f\n(xor 1 0)         ; => #t (1 is truthy, 0 is falsy)\n(xor 1 2)         ; => #f (both truthy)"
#> 
#> attr(,"arl_doc")$assert
#> [1] "(assert-true (xor #t #f))\n(assert-true (xor #f #t))\n(assert-false (xor #t #t))\n(assert-false (xor #f #f))\n(assert-true (xor 1 0))\n(assert-false (xor 1 2))"
#> 
#> attr(,"arl_doc")$seealso
#> [1] "not, and, or (special forms)"
#> 
#> attr(,"arl_doc")$note
#> [1] "Exclusive OR: returns #t when exactly one argument is truthy, #f when both are truthy or both are falsy."
#> 
#> attr(,"arl_doc")$arguments
#> [1] "a — First value\nb — Second value"
#> 
engine$eval(engine$read("(not #t)"))
#> [[1]]
#> not(TRUE)
#> 
tracker$report_console()
#> 
#> Arl Code Coverage (Execution Coverage)
#> ======================================
#> 
#> /tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/assert.arl    0/  67 lines (  0.0%)
#> /tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/binding.arl    0/  76 lines (  0.0%)
#> /tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/control.arl    0/  91 lines (  0.0%)
#> /tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/conversions.arl    0/  90 lines (  0.0%)
#> /tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/core.arl    0/  79 lines (  0.0%)
#> /tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/dict.arl    0/ 220 lines (  0.0%)
#> /tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/display.arl    0/ 175 lines (  0.0%)
#> /tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/equality.arl    0/ 113 lines (  0.0%)
#> /tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/functional.arl    0/ 167 lines (  0.0%)
#> /tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/io.arl    0/ 129 lines (  0.0%)
#> /tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/list.arl    0/ 183 lines (  0.0%)
#> /tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/logic.arl    3/   7 lines ( 42.9%)
#> /tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/looping.arl    0/ 108 lines (  0.0%)
#> /tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/math.arl    0/  83 lines (  0.0%)
#> /tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/r-interop.arl    0/  54 lines (  0.0%)
#> /tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/sequences.arl    0/ 197 lines (  0.0%)
#> /tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/set.arl    0/ 162 lines (  0.0%)
#> /tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/sort.arl    0/  71 lines (  0.0%)
#> /tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/strings.arl    0/ 194 lines (  0.0%)
#> /tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/struct.arl    0/  33 lines (  0.0%)
#> /tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/threading.arl    0/  75 lines (  0.0%)
#> /tmp/Rtmpw2pmb5/temp_libpath1f7450a68ed0/arl/arl/types.arl    0/ 142 lines (  0.0%)
#> 
#> Total: 3/2516 lines (0.1%)
#> 
# }
```
