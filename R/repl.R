# REPL: Stateful read-eval-print loop. read_form() gets input; main loop parses, evaluates, prints.
#
# @section REPL options:
# \describe{
#   \item{\code{arl.repl_quiet}}{If TRUE, show minimal startup banner (e.g. set by CLI \code{-q}/\code{--quiet}).}
#   \item{\code{arl.repl_use_history}}{If FALSE, do not load, save, or add to readline history.}
#   \item{\code{arl.repl_bracketed_paste}}{If TRUE (default), enable bracketed paste mode.}
#   \item{\code{arl.repl_read_form_override}}{Override \code{read_form()}:
#     function(input_fn, prompt, cont_prompt) -> list(text, exprs, error?) or NULL.}
#   \item{\code{arl.repl_can_use_history_override}}{Override for \code{can_use_history()}: function or logical.}
# }
#
# @field engine Engine instance used for parsing/evaluation.
# @field prompt Primary prompt string.
# @field cont_prompt Continuation prompt for multi-line input.
# @field input_fn Function used to read a single line of input.
# @field output_fn Function used to emit output messages.
# @field history_state Environment tracking history state.
# @field history_path Path to history file used for readline integration.
# @param engine Engine instance; required for evaluation.
# @param prompt Prompt string shown for new forms.
# @param cont_prompt Prompt string for continuation lines.
# @param input_fn Optional function to read a line (defaults to input_line).
# @param output_fn Output function for REPL banners/messages.
# @param history_state Environment storing history state.
# @param history_path Optional history file path; default ~/.arl_history.
# @param e Condition object to inspect for incomplete input.
# @param exprs List of Arl expressions to evaluate.
# @param value Value to print using the engine formatter.
# @param path History file path to load.
# @param text Line of input to add to readline history.
#
#' @keywords internal
#' @noRd
REPL <- R6::R6Class(
  "ArlREPL",
  public = list(
    engine = NULL,
    prompt = "arl> ",
    cont_prompt = "...> ",
    input_fn = NULL,
    output_fn = NULL,
    history_state = NULL,
    history_path = NULL,
    # @description Initialize the REPL state.
    # @param engine Engine instance (required for run()).
    # @param prompt, cont_prompt Prompt strings.
    # @param input_fn, output_fn Input/output functions.
    # @param history_state, history_path History state env and file path.
    initialize = function(
      engine = NULL,
      prompt = "arl> ",
      cont_prompt = "...> ",
      input_fn = NULL,
      output_fn = cat,
      history_state = new.env(parent = emptyenv()),
      history_path = NULL
    ) {
      self$engine <- engine
      self$prompt <- prompt
      self$cont_prompt <- cont_prompt
      self$input_fn <- if (is.null(input_fn)) self$input_line else input_fn
      self$output_fn <- output_fn
      self$history_state <- history_state
      self$history_path <- if (is.null(history_path)) self$history_path_default() else history_path
      private$ensure_history_state()
    },
    # @description Read a single input line, using readline when available.
    # When bracketed paste mode is enabled and the line starts with the BPM start sequence
    # (ESC [200 ~), reads until the BPM end sequence (ESC [201 ~) and returns the whole pasted block.
    input_line = function(prompt) {
      bpm_start <- "\033[200~"
      bpm_end <- "\033[201~"
      read_one = function(p) {
        if (isTRUE(interactive()) && isTRUE(capabilities("cledit"))) {
          return(readline(p))
        }
        cat(p)
        utils::flush.console()
        line <- readLines("stdin", n = 1, warn = FALSE)
        if (length(line) == 0) {
          return(NULL)
        }
        line
      }
      line <- read_one(prompt)
      if (is.null(line)) {
        return(NULL)
      }
      use_bpm <- isTRUE(.pkg_option("repl_bracketed_paste", TRUE))
      if (use_bpm && startsWith(line, bpm_start)) {
        chunk <- substring(line, nchar(bpm_start) + 1L)
        if (endsWith(chunk, bpm_end)) {
          return(substring(chunk, 1L, nchar(chunk) - nchar(bpm_end)))
        }
        lines <- chunk
        repeat {
          next_line <- read_one("")
          if (is.null(next_line)) {
            return(paste(lines, collapse = "\n"))
          }
          lines <- c(lines, next_line)
          if (endsWith(next_line, bpm_end)) {
            n <- length(lines)
            lines[[n]] <- substring(lines[[n]], 1L, nchar(lines[[n]]) - nchar(bpm_end))
            return(paste(lines, collapse = "\n"))
          }
        }
      }
      line
    },
    # @description Return the default history file path.
    history_path_default = function() {
      file.path(tools::R_user_dir(.pkg_name, "data"), paste0(.pkg_name, "_history"))
    },
    # @description Determine whether readline history can be used.
    can_use_history = function() {
      if (!isTRUE(.pkg_option("repl_use_history", TRUE))) {
        return(FALSE)
      }
      override <- .pkg_option("repl_can_use_history_override")
      if (!is.null(override)) {
        if (is.function(override)) {
          return(isTRUE(override()))
        }
        return(isTRUE(override))
      }
      isTRUE(interactive()) && isTRUE(capabilities("cledit"))
    },
    # @description Detect parse errors that indicate incomplete input.
    is_incomplete_error = function(e) {
      msg <- conditionMessage(e)
      grepl("Unexpected end of input|Unclosed parenthesis|Unterminated string", msg)
    },
    # @description Read a complete Arl form from the input stream.
    read_form = function() {
      private$require_engine()
      override <- .pkg_option("repl_read_form_override")
      if (!is.null(override)) {
        if (is.function(override)) {
          return(override(
            input_fn = self$input_fn,
            prompt = self$prompt,
            cont_prompt = self$cont_prompt
          ))
        }
        return(override)
      }
      # NOTE: readline provides line editing/history, but EOF (Ctrl-D) is not
      # distinguishable from an empty line, so we cannot reliably exit on Ctrl-D.
      buffer <- character(0)

      repeat {
        line <- self$input_fn(if (length(buffer) == 0) self$prompt else self$cont_prompt)
        if (is.null(line)) {
          return(NULL)
        }
        if (length(buffer) == 0 && trimws(line) == "") { # nolint: object_usage_linter
          next
        }

        buffer <- c(buffer, line)
        text <- paste(buffer, collapse = "\n")

        parsed <- tryCatch(
          self$engine$read(text, source_name = "<repl>"),
          error = function(e) e
        )

        if (inherits(parsed, "error")) {
          if (self$is_incomplete_error(parsed)) {
            next
          }
          stop(parsed)
        }

        return(list(text = text, exprs = parsed))
      }
    },
    # @description Evaluate expressions and print each resulting value.
    eval_and_print_exprs = function(exprs) {
      private$require_engine()
      self$engine$with_error_context(function() {
        result <- NULL
        for (expr in exprs) {
          result <- self$engine$eval(expr)
          self$print_value(result)
        }
        invisible(result)
      })
    },
    # @description Print a formatted value using the engine formatter.
    print_value = function(value) {
      private$require_engine()
      if (is.null(value)) {
        return(invisible(NULL))
      }
      formatted <- self$engine$format_value(value)
      if (nzchar(formatted)) {
        cat(formatted, "\n", sep = "")
      }
      utils::flush.console()
      invisible(value)
    },
    # @description Load readline history into the current session.
    load_history = function(path = self$history_path) {
      if (!self$can_use_history()) {
        return(invisible(FALSE))
      }
      snapshot <- tempfile("arl_rhistory_")
      saved <- tryCatch({
        utils::savehistory(snapshot)
        TRUE
      }, error = function(...) FALSE)
      if (!saved) {
        return(invisible(FALSE))
      }
      if (file.exists(path)) {
        lines <- readLines(path, warn = FALSE)
        if (length(lines) > private$history_max_entries) {
          lines <- lines[seq.int(length(lines) - private$history_max_entries + 1L, length(lines))]
          writeLines(lines, path)
        }
        if (length(lines) > 0L) {
          self$history_state$last_entry <- lines[length(lines)]
        }
        try(utils::loadhistory(path), silent = TRUE)
      }
      self$history_state$enabled <- TRUE
      self$history_state$path <- path
      self$history_state$snapshot <- snapshot
      invisible(TRUE)
    },
    # @description Save readline history for the current session.
    save_history = function() {
      if (!self$can_use_history()) {
        return(invisible(NULL))
      }
      if (!isTRUE(self$history_state$enabled)) {
        return(invisible(NULL))
      }
      if (!is.null(self$history_state$snapshot)) {
        try(utils::loadhistory(self$history_state$snapshot), silent = TRUE)
      }
      self$history_state$enabled <- FALSE
      self$history_state$path <- NULL
      self$history_state$snapshot <- NULL
      self$history_state$last_entry <- NULL
      invisible(NULL)
    },
    # @description Add a form to the readline history file and buffer.
    # R's readline() does not automatically record lines in the history;
    # that is done by R's own console loop, which we bypass.  We append
    # each evaluated form to the history file, then reload the file so
    # the entry is available via up-arrow recall in the same session.
    add_history = function(text) {
      if (!self$can_use_history()) {
        return(invisible(NULL))
      }
      if (!isTRUE(self$history_state$enabled)) {
        return(invisible(NULL))
      }
      if (trimws(text) == "") { # nolint: object_usage_linter
        return(invisible(NULL))
      }
      entry <- gsub("\n", " ", text)
      if (!is.null(self$history_state$last_entry) &&
          identical(entry, self$history_state$last_entry)) {
        return(invisible(NULL))
      }
      path <- self$history_state$path
      if (!is.null(path)) {
        hist_dir <- dirname(path)
        if (!dir.exists(hist_dir)) dir.create(hist_dir, recursive = TRUE)
        tryCatch({
          cat(entry, "\n", sep = "", file = path, append = TRUE)
          utils::loadhistory(path)
        }, error = function(...) NULL)
      }
      self$history_state$last_entry <- entry
      invisible(NULL)
    },
    # @description Run the interactive REPL loop.
    run = function() {
      private$require_engine()

      arl_version <- as.character(utils::packageVersion(.pkg_name))

      quiet <- isTRUE(.pkg_option("repl_quiet", FALSE))
      if (! quiet) {
        self$output_fn(paste0("Arl REPL ", arl_version), "\n", sep = "")
        self$output_fn("Type '(license)' w/o quotes for legal information.\n")
        self$output_fn("Type '(quit)' w/o quotes or press Ctrl+C to exit.\n")
        self$output_fn(
          "Builtin readline/libedit: ",
          ifelse(isTRUE(capabilities("cledit")), "yes", "no (try rlwrap)"),
          "\n\n",
          sep = ""
        )
      }

      use_bpm <- isTRUE(.pkg_option("repl_bracketed_paste", TRUE)) &&
        isTRUE(interactive()) &&
        isTRUE(capabilities("cledit"))
      if (use_bpm) {
        self$output_fn("\033[?2004h", sep = "")
        utils::flush.console()
        on.exit({
          self$output_fn("\033[?2004l", sep = "")
          utils::flush.console()
        }, add = TRUE)
      }

      self$load_history()
      on.exit(self$save_history(), add = TRUE)

      repeat {
        form <- tryCatch(
          self$read_form(),
          error = function(e) {
            self$engine$print_error(e, file = stderr())
            utils::flush.console()
            list(error = TRUE)
          }
        )

        if (is.null(form)) {
          break
        }

        if (isTRUE(form$error)) {
          next
        }

        input_text <- form$text
        if (trimws(input_text) %in% c("(quit)", "(exit)", "quit", "exit")) { # nolint: object_usage_linter
          break
        }

        self$add_history(input_text)

        tryCatch({
          self$eval_and_print_exprs(form$exprs)
        }, error = function(e) {
          self$engine$print_error(e, file = stderr())
          utils::flush.console()
        })
      }
    }
  ),
  private = list(
    history_max_entries = 1000L,
    require_engine = function() {
      if (is.null(self$engine)) {
        stop("Must provide Engine instance")
      }
    },
    ensure_history_state = function() {
      if (!exists("enabled", envir = self$history_state, inherits = FALSE)) {
        self$history_state$enabled <- FALSE
      }
      if (!exists("path", envir = self$history_state, inherits = FALSE)) {
        self$history_state$path <- NULL
      }
      if (!exists("snapshot", envir = self$history_state, inherits = FALSE)) {
        self$history_state$snapshot <- NULL
      }
      if (!exists("last_entry", envir = self$history_state, inherits = FALSE)) {
        self$history_state$last_entry <- NULL
      }
    }
  )
)
