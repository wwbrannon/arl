# knitr engine for Arl code blocks in vignettes.
#
# Provides:
#   - eng_arl()              — knitr language engine for {arl} chunks
#   - register_knitr_engine() — register the engine (called from vignette setup chunks)
#   - arl_html_vignette()    — rmarkdown output format with Arl syntax highlighting

# ---------------------------------------------------------------------------
# Shared engine lifecycle
# ---------------------------------------------------------------------------

.knitr_state <- new.env(parent = emptyenv())

#' Get or create the shared Arl engine for knitr evaluation.
#'
#' The engine is lazily initialised and reset when knitr moves to a new
#' input document (detected via \code{knitr::current_input()}).
#' @return An \code{Engine} instance.
#' @keywords internal
#' @noRd
get_arl_engine <- function() {
  current_doc <- knitr::current_input()
  if (is.null(current_doc)) current_doc <- ""

  if (!identical(.knitr_state$doc, current_doc)) {
    .knitr_state$engine <- NULL
    .knitr_state$doc <- current_doc
  }

  if (is.null(.knitr_state$engine)) {
    .knitr_state$engine <- Engine$new()
  }

  .knitr_state$engine
}

#' Reset the shared knitr Arl engine.
#' @keywords internal
#' @noRd
reset_arl_engine <- function() {
  .knitr_state$engine <- NULL
  .knitr_state$doc <- NULL
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Engine function
# ---------------------------------------------------------------------------

#' Evaluate Arl code in a knitr chunk.
#'
#' This is the knitr language engine function registered for \code{{arl}}
#' chunks.  It parses and evaluates Arl code using a shared \code{Engine}
#' instance (persistent across chunks within a single vignette) and formats
#' output in REPL style.
#'
#' @param options Chunk options list passed by knitr.
#' @return A character string produced by \code{knitr::engine_output()}.
#' @keywords internal
#' @noRd
eng_arl <- function(options) {
  # Tell knitr / Pandoc this is "arl" so the syntax definition applies
  options$engine <- "arl"

  if (!isTRUE(options$eval)) {
    return(knitr::engine_output(options, options$code, ""))
  }

  engine <- get_arl_engine()
  code <- paste(options$code, collapse = "\n")

  out <- tryCatch(
    evaluate_arl_code(engine, code),
    error = function(e) {
      if (isTRUE(options$error)) {
        paste0("Error: ", conditionMessage(e))
      } else {
        stop(e)
      }
    }
  )

  knitr::engine_output(options, options$code, out)
}

#' Evaluate Arl code REPL-style and collect output.
#'
#' Parses \code{code} into expressions, evaluates each one, and collects
#' both side-effect output (via \code{capture.output}) and formatted return
#' values.  NULL results are suppressed (matching REPL behaviour).
#'
#' @param engine An \code{Engine} instance.
#' @param code Character string of Arl source code.
#' @return Character string of collected output lines.
#' @keywords internal
#' @noRd
evaluate_arl_code <- function(engine, code) {
  exprs <- engine$read(code, source_name = "<knitr>")
  if (length(exprs) == 0L) return("")

  output_lines <- character(0L)

  engine$source_tracker$with_error_context(function() {
    for (expr in exprs) {
      # Capture any side-effect output (display, print, etc.)
      side_output <- utils::capture.output({
        result <- engine$eval(expr)
      })

      if (length(side_output) > 0L) {
        output_lines <<- c(output_lines, side_output)
      }

      # Format and append the return value (skip NULL, matching REPL)
      if (!is.null(result)) {
        formatted <- engine$env$format_value(result)
        if (nzchar(formatted)) {
          output_lines <<- c(output_lines, formatted)
        }
      }
    }
  })

  paste(output_lines, collapse = "\n")
}

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#' Register the Arl knitr engine
#'
#' Call this function in a vignette's setup chunk to enable \code{{arl}} code
#' chunks.  The engine evaluates Arl code using a shared \code{Engine}
#' instance that persists across chunks within a single document.
#'
#' @export
#' @examples
#' \dontrun{
#' # In a vignette setup chunk:
#' arl::register_knitr_engine()
#' }
register_knitr_engine <- function() {
  knitr::knit_engines$set(arl = eng_arl)
}

#' HTML vignette format with Arl syntax highlighting
#'
#' A wrapper around \code{\link[rmarkdown]{html_vignette}} that passes
#' \code{--syntax-definition} to Pandoc so that \code{{arl}} code blocks
#' receive proper syntax highlighting.
#'
#' @param pandoc_args Additional Pandoc arguments (merged with the
#'   syntax-definition argument added automatically).
#' @param ... Arguments passed to \code{\link[rmarkdown]{html_vignette}}.
#' @return An R Markdown output format object.
#' @export
arl_html_vignette <- function(pandoc_args = NULL, ...) {
  # During R CMD check the file is in the installed package; during
  # source-level builds (devtools::build_vignettes) it is under ../inst/
  syntax_file <- system.file("pandoc", "arl.xml", package = "arl")
  if (!nzchar(syntax_file)) {
    syntax_file <- file.path("..", "inst", "pandoc", "arl.xml")
  }

  if (file.exists(syntax_file)) {
    pandoc_args <- c(pandoc_args, "--syntax-definition", syntax_file)
  }

  rmarkdown::html_vignette(pandoc_args = pandoc_args, ...)
}
