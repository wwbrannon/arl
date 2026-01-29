#' Source tracking for errors
#'
#' @keywords internal
#' @noRd
SourceTracker <- R6::R6Class(
  "SourceTracker",
  private = list(
    stack = list()
  ),
  public = list(
    src_new = function(file, start_line, start_col, end_line = start_line, end_col = start_col) {
      structure(
        list(
          file = file,
          start_line = start_line,
          start_col = start_col,
          end_line = end_line,
          end_col = end_col
        ),
        class = "rye_src"
      )
    },
    src_get = function(expr) {
      if (is.null(expr)) {
        return(NULL)
      }
      attr(expr, "rye_src", exact = TRUE)
    },
    src_set = function(expr, src) {
      if (is.null(expr) || is.null(src)) {
        return(expr)
      }
      if (is.symbol(expr)) {
        return(expr)
      }
      attr(expr, "rye_src") <- src
      expr
    },
    src_inherit = function(expr, from) {
      src <- self$src_get(from)
      if (is.null(src)) {
        return(expr)
      }
      if (!is.null(self$src_get(expr))) {
        return(expr)
      }
      self$src_set(expr, src)
    },
    strip_src = function(value) {
      if (is.null(value) || is.symbol(value)) {
        return(value)
      }

      has_src_attr <- !is.null(attr(value, "rye_src", exact = TRUE))

      if (!is.call(value) && (!is.list(value) || !is.null(attr(value, "class", exact = TRUE)))) {
        if (has_src_attr) {
          attr(value, "rye_src") <- NULL
        }
        return(value)
      }

      if (!has_src_attr && length(value) == 0) {
        return(value)
      }

      if (has_src_attr) {
        attr(value, "rye_src") <- NULL
      }

      if (is.call(value)) {
        stripped <- lapply(as.list(value), self$strip_src)
        return(as.call(stripped))
      }

      if (is.list(value) && is.null(attr(value, "class", exact = TRUE))) {
        stripped <- lapply(value, self$strip_src)
        if (!is.null(names(value))) {
          names(stripped) <- names(value)
        }
        return(stripped)
      }

      value
    },
    get = function() {
      private$stack
    },
    reset = function() {
      private$stack <- list()
      invisible(NULL)
    },
    push = function(src) {
      private$stack <- c(private$stack, list(src))
      invisible(NULL)
    },
    pop = function() {
      if (length(private$stack) == 0) {
        return(NULL)
      }
      last <- private$stack[[length(private$stack)]]
      private$stack <- private$stack[-length(private$stack)]
      last
    },
    format_src = function(src) {
      if (is.null(src)) {
        return(NULL)
      }
      file <- src$file
      if (is.null(file) || !is.character(file) || length(file) != 1 || !nzchar(file)) {
        file <- "<input>"
      }
      start <- paste0(src$start_line, ":", src$start_col)
      end <- paste0(src$end_line, ":", src$end_col)
      if (identical(start, end)) {
        return(paste0(file, ":", start))
      }
      paste0(file, ":", start, "-", end)
    },
    format_error = function(e, include_r_stack = TRUE) {
      lines <- c(paste0("Error: ", conditionMessage(e)))
      if (inherits(e, "rye_error")) {
        src_stack <- e$src_stack
        if (!is.null(src_stack) && length(src_stack) > 0) {
          loc <- self$format_src(src_stack[[length(src_stack)]])
          if (!is.null(loc)) {
            lines <- c(lines, paste0("Location: ", loc))
          }
          if (length(src_stack) > 1) {
            lines <- c(lines, "Rye stack:")
            for (src in rev(src_stack)) {
              loc <- self$format_src(src)
              if (!is.null(loc)) {
                lines <- c(lines, paste0("  at ", loc))
              }
            }
          }
        }
        if (isTRUE(include_r_stack)) {
          r_stack <- e$r_stack
          if (!is.null(r_stack) && length(r_stack) > 0) {
            lines <- c(lines, "R stack:")
            max_frames <- 20
            calls <- r_stack
            if (length(calls) > max_frames) {
              calls <- utils::tail(calls, max_frames)
            }
            for (call in rev(calls)) {
              lines <- c(lines, paste0("  ", paste(deparse(call), collapse = "")))
            }
            if (length(r_stack) > max_frames) {
              lines <- c(lines, "  ...")
            }
          }
        }
      }
      paste(lines, collapse = "\n")
    },
    print_error = function(e, file = stderr()) {
      cat(self$format_error(e), "\n", sep = "", file = file)
    },
    with_error_context = function(fn) {
      prev_stack <- self$get()
      on.exit({
        self$reset()
        if (!is.null(prev_stack) && length(prev_stack) > 0) {
          for (src in prev_stack) {
            self$push(src)
          }
        }
      }, add = TRUE)
      self$reset()
      tryCatch(
        fn(),
        error = function(e) {
          if (inherits(e, "rye_error")) {
            stop(e)
          }
          cond <- rye_error(conditionMessage(e), self$get(), sys.calls())
          stop(cond)
        }
      )
    }
  )
)
