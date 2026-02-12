# SourceTracker: Attaches source locations (file, line, col) to expressions for error
# reporting. Used by parser and compiled runtime.
#
# @field (private) stack List of arl_src for current evaluation context.
#
#' @keywords internal
#' @noRd
SourceTracker <- R6::R6Class(
  "SourceTracker",
  private = list(
    stack_env = NULL,
    stack_n = 0L
  ),
  public = list(
    initialize = function() {
      private$stack_env <- new.env(hash = FALSE, parent = emptyenv())
      private$stack_n <- 0L
    },
    # @description Create a arl_src structure for a source location.
    # @param file File name or NULL.
    # @param start_line Start line number.
    # @param start_col Start column number.
    # @param end_line End line (default start_line).
    # @param end_col End column (default start_col).
    # @return Object of class "arl_src".
    src_new = function(file, start_line, start_col, end_line = start_line, end_col = start_col) {
      structure(
        list(
          file = file,
          start_line = start_line,
          start_col = start_col,
          end_line = end_line,
          end_col = end_col
        ),
        class = "arl_src"
      )
    },
    # @description Get arl_src attribute from an expression.
    # @param expr Expression (call, list, etc.).
    # @return arl_src or NULL.
    src_get = function(expr) {
      if (is.null(expr)) {
        return(NULL)
      }
      attr(expr, "arl_src", exact = TRUE)
    },
    # @description Attach arl_src to an expression.
    # @param expr Expression to attach to.
    # @param src arl_src from src_new.
    # @return expr (invisibly) or expr unchanged if symbol.
    src_set = function(expr, src) {
      if (is.null(expr) || is.null(src)) {
        return(expr)
      }
      if (is.symbol(expr)) {
        return(expr)
      }
      attr(expr, "arl_src") <- src
      expr
    },
    # @description Copy source from one expression to another if expr has none.
    # @param expr Expression to possibly attach to.
    # @param from Expression to take arl_src from.
    # @return expr.
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
    # @description Recursively remove arl_src attributes from a value (for returning from eval).
    # @param value Any value (call, list, etc.).
    # @return Value with arl_src stripped.
    strip_src = function(value) {
      if (is.null(value) || is.symbol(value)) {
        return(value)
      }

      if (r6_isinstance(value, "Cons")) {
        value$car <- self$strip_src(value$car)
        value$cdr <- self$strip_src(value$cdr)
        return(value)
      }

      has_src_attr <- !is.null(attr(value, "arl_src", exact = TRUE))

      if (!is.call(value) && (!is.list(value) || !is.null(attr(value, "class", exact = TRUE)))) {
        if (has_src_attr) {
          attr(value, "arl_src") <- NULL
        }
        return(value)
      }

      if (is.call(value) && !is.null(attr(value, "class", exact = TRUE))) {
        if (has_src_attr) {
          attr(value, "arl_src") <- NULL
        }
        return(value)
      }

      if (!has_src_attr && length(value) == 0) {
        return(value)
      }

      if (has_src_attr) {
        attr(value, "arl_src") <- NULL
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
    # @description Return the current source stack (list of arl_src).
    # @return List.
    get = function() {
      n <- private$stack_n
      if (n == 0L) return(list())
      env <- private$stack_env
      out <- vector("list", n)
      for (i in seq_len(n)) out[[i]] <- env[[as.character(i)]]
      out
    },
    # @description Clear the source stack.
    reset = function() {
      private$stack_env <- new.env(hash = FALSE, parent = emptyenv())
      private$stack_n <- 0L
      invisible(NULL)
    },
    # @description Push a arl_src onto the stack (used during eval for error context).
    # @param src arl_src from src_new.
    push = function(src) {
      n <- private$stack_n + 1L
      private$stack_n <- n
      private$stack_env[[as.character(n)]] <- src
      invisible(NULL)
    },
    # @description Pop the top arl_src from the stack.
    # @return The popped arl_src or NULL if empty.
    pop = function() {
      n <- private$stack_n
      if (n == 0L) return(NULL)
      key <- as.character(n)
      last <- private$stack_env[[key]]
      rm(list = key, envir = private$stack_env)
      private$stack_n <- n - 1L
      last
    },
    # @description Format a arl_src as "file:line:col" or "file:line:col-line:col".
    # @param src arl_src or NULL.
    # @return Character string or NULL.
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
      if (inherits(e, "arl_error")) {
        src_stack <- e$src_stack
        if (!is.null(src_stack) && length(src_stack) > 0) {
          loc <- self$format_src(src_stack[[length(src_stack)]])
          if (!is.null(loc)) {
            lines <- c(lines, paste0("Location: ", loc))
          }
          if (length(src_stack) > 1) {
            lines <- c(lines, "Arl stack:")
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
    # @description Build a arl_error condition with message and optional Arl/R stack traces.
    # @param message Error message string.
    # @param src_stack List of arl_src for Arl stack (default empty).
    # @param r_stack List of call objects for R stack (default empty).
    # @return Condition of class arl_error.
    create_error = function(message, src_stack = list(), r_stack = list()) {
      structure(
        list(message = message, src_stack = src_stack, r_stack = r_stack),
        class = c("arl_error", "error", "condition")
      )
    },
    # @description Run fn(); on error, augment condition with current stack for location in message.
    # @param fn Function of no arguments to run.
    # @return Result of fn().
    with_error_context = function(fn) {
      prev_env <- private$stack_env
      prev_n <- private$stack_n
      on.exit({
        private$stack_env <- prev_env
        private$stack_n <- prev_n
      }, add = TRUE)
      self$reset()
      result_with_vis <- tryCatch(
        withVisible(fn()),
        error = function(e) {
          if (inherits(e, "arl_error")) {
            stop(e)
          }
          cond <- self$create_error(conditionMessage(e), self$get(), sys.calls())
          stop(cond)
        }
      )
      # Preserve invisibility
      if (result_with_vis$visible) {
        result_with_vis$value
      } else {
        invisible(result_with_vis$value)
      }
    }
  )
)
