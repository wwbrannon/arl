#' Stateful Rye REPL
#'
#' Provides an interactive loop for reading Rye forms, evaluating them, and
#' printing results with optional readline history support.
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom R6 R6Class
#' @field engine RyeEngine instance used for parsing/evaluation.
#' @field prompt Primary prompt string.
#' @field cont_prompt Continuation prompt for multi-line input.
#' @field input_fn Function used to read a single line of input.
#' @field output_fn Function used to emit output messages.
#' @field history_state Environment tracking history state.
#' @field history_path Path to history file used for readline integration.
#' @param engine RyeEngine instance; required for evaluation.
#' @param prompt Prompt string shown for new forms and passed to `input_line`.
#' @param cont_prompt Prompt string shown for continuation lines.
#' @param input_fn Optional function to read a line (defaults to `input_line`).
#' @param output_fn Output function for REPL banners/messages.
#' @param history_state Environment storing history state.
#' @param history_path Optional history file path; defaults to `~/.rye_history`.
#' @param e Condition object to inspect for incomplete input.
#' @param exprs List of Rye expressions to evaluate.
#' @param value Value to print using the engine formatter.
#' @param path History file path to load.
#' @param text Line of input to add to readline history.
RyeREPL <- R6::R6Class(
  "RyeREPL",
  public = list(
    engine = NULL,
    prompt = "rye> ",
    cont_prompt = "...> ",
    input_fn = NULL,
    output_fn = NULL,
    history_state = NULL,
    history_path = NULL,
    #' @description
    #' Initialize the REPL state.
    initialize = function(
      engine = NULL,
      prompt = "rye> ",
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
    #' @description
    #' Read a single input line, using readline when available.
    input_line = function(prompt) {
      if (isTRUE(interactive()) && isTRUE(capabilities("readline"))) {
        return(readline(prompt))
      }
      cat(prompt)
      utils::flush.console()
      line <- readLines("stdin", n = 1, warn = FALSE)
      if (length(line) == 0) {
        return(NULL)
      }
      line
    },
    #' @description
    #' Return the default history file path.
    history_path_default = function() {
      path.expand("~/.rye_history")
    },
    #' @description
    #' Determine whether readline history can be used.
    can_use_history = function() {
      override <- getOption("rye.repl_can_use_history_override", NULL)
      if (!is.null(override)) {
        if (is.function(override)) {
          return(isTRUE(override()))
        }
        return(isTRUE(override))
      }
      isTRUE(interactive()) && isTRUE(capabilities("readline"))
    },
    #' @description
    #' Detect parse errors that indicate incomplete input.
    is_incomplete_error = function(e) {
      msg <- conditionMessage(e)
      grepl("Unexpected end of input|Unclosed parenthesis", msg)
    },
    #' @description
    #' Read a complete Rye form from the input stream.
    read_form = function() {
      private$require_engine()
      override <- getOption("rye.repl_read_form_override", NULL)
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
    #' @description
    #' Evaluate expressions and print each resulting value.
    eval_and_print_exprs = function(exprs) {
      private$require_engine()
      self$engine$source_tracker$with_error_context(function() {
        result <- NULL
        for (expr in exprs) {
          result <- self$engine$eval(expr)
          self$print_value(result)
        }
        invisible(result)
      })
    },
    #' @description
    #' Print a formatted value using the engine formatter.
    print_value = function(value) {
      private$require_engine()
      if (is.null(value)) {
        return(invisible(NULL))
      }
      cat(self$engine$env$format_value(value), "\n", sep = "")
      invisible(value)
    },
    #' @description
    #' Load readline history into the current session.
    load_history = function(path = self$history_path) {
      if (!self$can_use_history()) {
        return(invisible(FALSE))
      }
      snapshot <- tempfile("rye_rhistory_")
      saved <- tryCatch({
        utils::savehistory(snapshot)
        TRUE
      }, error = function(...) FALSE)
      if (!saved) {
        return(invisible(FALSE))
      }
      if (file.exists(path)) {
        try(utils::loadhistory(path), silent = TRUE)
      }
      self$history_state$enabled <- TRUE
      self$history_state$path <- path
      self$history_state$snapshot <- snapshot
      invisible(TRUE)
    },
    #' @description
    #' Save readline history for the current session.
    save_history = function() {
      if (!self$can_use_history()) {
        return(invisible(NULL))
      }
      if (!isTRUE(self$history_state$enabled)) {
        return(invisible(NULL))
      }
      try(utils::savehistory(self$history_state$path), silent = TRUE)
      if (!is.null(self$history_state$snapshot)) {
        try(utils::loadhistory(self$history_state$snapshot), silent = TRUE)
      }
      self$history_state$enabled <- FALSE
      self$history_state$path <- NULL
      self$history_state$snapshot <- NULL
      invisible(NULL)
    },
    #' @description
    #' Add a line of input to readline history.
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
      add_history <- tryCatch(
        utils::getFromNamespace("addHistory", "utils"),
        error = function(...) NULL
      )
      if (is.null(add_history)) {
        return(invisible(NULL))
      }
      try(add_history(text), silent = TRUE)
      invisible(NULL)
    },
    #' @description
    #' Run the interactive REPL loop.
    run = function() {
      private$require_engine()

      self$output_fn(paste0("Rye REPL ", rye_version()), "\n", sep = "")
      self$output_fn("Type (quit) or press Ctrl+C to exit\n")
      self$output_fn(
        "Builtin readline support:",
        ifelse(isTRUE(capabilities("readline")), "yes", "no"),
        "\n\n",
        sep = ""
      )

      self$load_history()
      on.exit(self$save_history(), add = TRUE)

      repeat {
        form <- tryCatch(
          self$read_form(),
          error = function(e) {
            self$engine$source_tracker$print_error(e, file = stdout())
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
          self$engine$source_tracker$print_error(e, file = stdout())
        })
      }
    }
  ),
  private = list(
    require_engine = function() {
      if (is.null(self$engine)) {
        stop("Must provide RyeEngine instance")
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
    }
  )
)
