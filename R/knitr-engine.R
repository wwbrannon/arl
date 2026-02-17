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
    eng <- Engine$new()

    # Load all stdlib modules so vignette code can use the full language
    # without explicit (import ...) for every module.
    load_order_path <- system.file("arl", "load-order.txt", package = "arl")
    if (nzchar(load_order_path) && file.exists(load_order_path)) {
      all_modules <- readLines(load_order_path, warn = FALSE)
      all_modules <- all_modules[nzchar(all_modules)]
      arl_env <- Env$new(eng$get_env())
      registry <- arl_env$module_registry
      for (mod in all_modules) {
        if (!registry$exists(mod)) {
          path <- resolve_stdlib_path(mod)
          if (!is.null(path)) {
            eng$load_file_in_env(path, eng$get_env())
          }
        }
      }
      for (mod in all_modules) {
        if (registry$exists(mod)) {
          registry$attach_into(mod, eng$get_env())
        }
      }
    }

    .knitr_state$engine <- eng
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
#' output as an interleaved REPL transcript: each expression's source lines
#' are prefixed with \code{arl> } and its output with \code{#> }.
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

  transcript <- tryCatch(
    evaluate_arl_code(engine, code),
    error = function(e) {
      if (isTRUE(options$error)) {
        paste0("Error: ", conditionMessage(e))
      } else {
        stop(e)
      }
    }
  )

  # The transcript already contains arl> prompts and #> output prefixes,
  # so suppress knitr's default code echo and comment prefix.
  repl_options <- options
  repl_options$echo <- FALSE
  repl_options$comment <- ""
  knitr::engine_output(repl_options, code = "", out = transcript)
}

#' Build an interleaved REPL transcript from Arl code.
#'
#' Parses \code{code} into expressions, maps each back to its source lines
#' using \code{arl_src} attributes, and builds a transcript where source
#' lines are prefixed with \code{arl> } and output lines with \code{#> }.
#' Comments and blank lines between expressions are attributed to the
#' following expression.  NULL results are suppressed (matching REPL
#' behaviour).
#'
#' @param engine An \code{Engine} instance.
#' @param code Character string of Arl source code.
#' @return Character string of the REPL transcript.
#' @keywords internal
#' @noRd
evaluate_arl_code <- function(engine, code) {
  exprs <- engine$read(code, source_name = "<knitr>")
  code_lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  n_lines <- length(code_lines)

  prompt <- "arl> "
  output_prefix <- "#> "

  # Helper: append prompted source lines to the transcript.
  # Blank lines are kept blank (no prompt) for readability.
  add_source <- function(transcript, from, to) {
    if (from > to || from < 1L || to > n_lines) return(transcript)
    for (i in from:to) {
      ln <- code_lines[i]
      if (grepl("^\\s*$", ln)) {
        transcript <- c(transcript, "")
      } else {
        transcript <- c(transcript, paste0(prompt, ln))
      }
    }
    transcript
  }

  # Helper: append output lines (side effects or formatted values).
  add_output <- function(transcript, text) {
    if (length(text) == 0L) return(transcript)
    combined <- paste(text, collapse = "\n")
    if (!nzchar(combined)) return(transcript)
    lines <- strsplit(combined, "\n", fixed = TRUE)[[1]]
    c(transcript, paste0(output_prefix, lines))
  }

  # No expressions (only comments / blank lines): show prompted source
  if (length(exprs) == 0L) {
    if (n_lines == 0L) return("")
    return(paste(add_source(character(0L), 1L, n_lines), collapse = "\n"))
  }

  transcript <- character(0L)
  prev_end <- 0L

  engine$with_error_context(function() {
    for (expr in exprs) {
      src <- attr(expr, "arl_src", exact = TRUE)

      if (!is.null(src)) {
        expr_end <- src$end_line
      } else {
        # Fallback for bare symbols (which can't carry arl_src):
        # scan forward past blanks/comments to the next content line.
        scan <- prev_end + 1L
        while (scan <= n_lines && grepl("^\\s*(;|$)", code_lines[scan])) {
          scan <- scan + 1L
        }
        expr_end <- scan
      }

      # Include all lines from after the previous expression through this
      # expression's end.  This captures gap lines (comments, blank lines)
      # before the expression as well as the expression itself.
      range_end <- min(expr_end, n_lines)
      transcript <<- add_source(transcript, prev_end + 1L, range_end)
      prev_end <<- range_end

      # Capture any side-effect output (display, print, etc.)
      side_output <- utils::capture.output({
        result <- engine$eval(expr)
      })

      if (length(side_output) > 0L) {
        transcript <<- add_output(transcript, side_output)
      }

      # Format and append the return value (skip NULL, matching REPL)
      if (!is.null(result)) {
        formatted <- engine$format_value(result)
        if (nzchar(formatted)) {
          transcript <<- add_output(transcript, formatted)
        }
      }
    }
  })

  # Trailing lines after the last expression (comments, blank lines)
  if (prev_end < n_lines) {
    transcript <- add_source(transcript, prev_end + 1L, n_lines)
  }

  paste(transcript, collapse = "\n")
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
#' @param check_title Whether to check that the vignette title matches the
#'   \code{VignetteIndexEntry}.  Set to \code{FALSE} for pkgdown-only articles
#'   that intentionally omit a \code{VignetteIndexEntry}.
#' @param ... Arguments passed to \code{\link[rmarkdown]{html_vignette}}.
#' @return An R Markdown output format object.
#' @export
arl_html_vignette <- function(pandoc_args = NULL, check_title = TRUE, ...) {
  # During R CMD check the file is in the installed package; during
  # source-level builds (devtools::build_vignettes) it is under ../inst/
  syntax_file <- system.file("pandoc", "arl.xml", package = "arl")
  if (!nzchar(syntax_file)) {
    syntax_file <- file.path("..", "inst", "pandoc", "arl.xml")
  }

  if (file.exists(syntax_file)) {
    pandoc_args <- c(pandoc_args, "--syntax-definition", syntax_file)
  }

  if (!check_title) {
    options(rmarkdown.html_vignette.check_title = FALSE)
  }

  rmarkdown::html_vignette(pandoc_args = pandoc_args, ...)
}
