#' Stateful Rye REPL
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
    history_path_default = function() {
      path.expand("~/.rye_history")
    },
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
    is_incomplete_error = function(e) {
      msg <- conditionMessage(e)
      grepl("Unexpected end of input|Unclosed parenthesis", msg)
    },
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
    print_value = function(value) {
      private$require_engine()
      if (is.null(value)) {
        return(invisible(NULL))
      }
      cat(self$engine$env$format_value(value), "\n", sep = "")
      invisible(value)
    },
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
