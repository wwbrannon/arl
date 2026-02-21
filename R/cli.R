# Condition constructor for CLI errors. Signalled instead of quit() so that
# CRAN policy ("packages must not terminate the R process") is satisfied.
# The exec/arl wrapper script catches this and exits with status 1.
arl_cli_error <- function(message, show_help = FALSE) {
  structure(
    class = c("arl_cli_error", "error", "condition"),
    list(message = message, show_help = show_help)
  )
}

CLI_HELP_TEXT <- paste(
  "Arl: A Lisp dialect for R.",
  "",
  "Usage:",
  "  arl [--file <path>...] [--eval <expr>] [--quiet] [<files>...]",
  "  arl --version",
  "  arl --help",
  "",
  "Options:",
  "  -f, --file <path>    Evaluate an Arl source file (repeatable).",
  "  -e, --eval <expr>    Evaluate a single Arl expression.",
  "  -q, --quiet          Start REPL without banner.",
  "  -n, --no-stdlib      Don't load stdlib modules (bare engine).",
  "  -v, --version        Print version and exit.",
  "  -h, --help           Show this help message.",
  "",
  "Examples:",
  "  arl",
  "  arl -q",
  "  arl --file script.arl",
  "  arl script.arl",
  "  arl --eval \"(+ 1 2)\"",
  sep = "\n"
)

# CLI: Command-line interface for the arl script. Parses args (--file, --eval, --quiet,
# positional files), creates an engine, and either runs the REPL or evaluates files/expressions.
#
# @field args Raw command-line args (character vector).
# @field parsed Result of parse() (list with file, eval, help, version, etc.).
#
#' @keywords internal
#' @noRd
CLI <- R6::R6Class(
  "ArlCLI",
  public = list(
    args = NULL,
    parsed = NULL,
    # @description Create CLI with optional args (default: commandArgs(trailingOnly = TRUE)).
    # @param args Character vector of command-line arguments.
    initialize = function(args = commandArgs(trailingOnly = TRUE)) {
      self$args <- args
    },
    # @description Print error and optionally help text; exit via arl.cli_exit_fn or error condition.
    # @param message Error message string.
    # @param show_help If TRUE, print CLI help after the message.
    cli_exit_with_error = function(message, show_help = TRUE) {
      exit_fn <- .pkg_option("cli_exit_fn")
      if (!is.null(exit_fn) && is.function(exit_fn)) {
        exit_fn(message, show_help)
        return(invisible(NULL))
      }
      message("Error: ", message)
      if (isTRUE(show_help)) {
        cat(CLI_HELP_TEXT, "\n", sep = "")
      }
      stop(arl_cli_error(message, show_help))
    },
    # @description Parse remaining args for --eval/-e, --quiet/-q, and positional args.
    # @param args Character vector of arguments (after -f/--file extraction).
    # @return List with elements options (list) and args (character).
    parse_remaining = function(args) {
      opt <- list(eval = NULL, quiet = FALSE, no_stdlib = FALSE)
      positional <- character(0)
      i <- 1L
      while (i <= length(args)) {
        a <- args[i]
        if (a %in% c("-e", "--eval")) {
          if (i < length(args)) {
            opt$eval <- args[i + 1L]
            i <- i + 2L
          } else {
            stop("--eval requires an expression.")
          }
        } else if (a %in% c("-q", "--quiet")) {
          opt$quiet <- TRUE
          i <- i + 1L
        } else if (a %in% c("-n", "--no-stdlib")) {
          opt$no_stdlib <- TRUE
          i <- i + 1L
        } else if (startsWith(a, "-")) {
          stop("Unrecognized flag: ", a)
        } else {
          positional <- c(positional, a)
          i <- i + 1L
        }
      }
      list(options = opt, args = positional)
    },
    # @description Extract file paths from -f/--file and positional args; validate and return list(files, error).
    # @param args Character vector of arguments.
    # @return List with elements files (character) and error (character, optional).
    extract_file_args = function(args) {
      files <- character(0)
      error <- character(0)
      i <- 1L
      while (i <= length(args)) {
        if (args[i] %in% c("-f", "--file")) {
          if (i < length(args)) {
            files <- c(files, args[i + 1L])
            i <- i + 2L
          } else {
            error <- c(error, "--file requires a path.")
            i <- i + 1L
          }
        } else {
          i <- i + 1L
        }
      }
      omit <- logical(length(args))
      i <- 1L
      while (i <= length(args)) {
        if (args[i] %in% c("-f", "--file")) {
          omit[i] <- TRUE
          if (i < length(args)) {
            omit[i + 1L] <- TRUE
            i <- i + 2L
          } else {
            i <- i + 1L
          }
        } else {
          i <- i + 1L
        }
      }
      args_for_parse <- args[!omit]
      list(files = files, args_for_parse = args_for_parse, error = error)
    },
    # @description Parse self$args with cli_parser(); set self$parsed. Exits on help/version or parse error.
    # @return Invisible; may exit.
    parse = function() {
      state <- list(
        action = "repl",
        files = character(0),
        expr = NULL,
        errors = character(0)
      )

      args <- self$args
      args <- args[args != "--args"]
      terminator_index <- match("--", args)
      args_after <- character(0)
      if (!is.na(terminator_index)) {
        if (terminator_index < length(args)) {
          args_after <- args[(terminator_index + 1):length(args)]
        }
        args <- if (terminator_index > 1) {
          args[1:(terminator_index - 1)]
        } else {
          character(0)
        }
      }

      if (length(args) > 0) {
        if (any(c("-h", "--help") %in% args)) {
          state$action <- "help"
          self$parsed <- state
          return(state)
        }
        if (any(c("-v", "--version") %in% args)) {
          state$action <- "version"
          self$parsed <- state
          return(state)
        }
      }

      n_eval_flags <- sum(args %in% c("--eval", "-e"))
      if (n_eval_flags > 1) {
        state$errors <- c(state$errors, "Multiple --eval flags not allowed.")
        self$parsed <- state
        return(state)
      }

      file_extract <- self$extract_file_args(args)
      state$files <- c(state$files, file_extract$files)
      state$errors <- c(state$errors, file_extract$error)
      args_for_parse <- file_extract$args_for_parse
      if (length(state$errors) > 0) {
        self$parsed <- state
        return(state)
      }

      parsed <- tryCatch(
        self$parse_remaining(args_for_parse),
        error = function(e) {
          state$errors <<- c(state$errors, conditionMessage(e))
          return(NULL)
        }
      )

      if (is.null(parsed)) {
        self$parsed <- state
        return(state)
      }

      opt <- parsed$options
      positional <- parsed$args

      if (isTRUE(opt$quiet)) {
        .set_pkg_option("repl_quiet", TRUE)
      }

      if (isTRUE(opt$no_stdlib)) {
        state$no_stdlib <- TRUE
      }

      if (length(positional) > 0) {
        state$files <- c(state$files, as.character(positional))
      }
      if (length(args_after) > 0) {
        state$files <- c(state$files, args_after)
      }
      if (!is.null(opt$eval) && !is.na(opt$eval) && nzchar(opt$eval)) {
        state$expr <- opt$eval
      }

      if (length(state$files) > 0 && !is.null(state$expr)) {
        state$errors <- c(state$errors, "Use only one of --file/files or --eval.")
      }

      if (isTRUE(opt$help)) {
        state$action <- "help"
      } else if (isTRUE(opt$version)) {
        state$action <- "version"
      } else if (!is.null(state$expr)) {
        state$action <- "eval"
      } else if (length(state$files) > 0) {
        state$action <- "file"
      }

      self$parsed <- state
      state
    },
    # @description Print CLI help text and exit.
    do_help = function() {
      cat(CLI_HELP_TEXT, "\n", sep = "")
      invisible(NULL)
    },
    # @description Print package version and exit.
    do_version = function() {
      version <- tryCatch(
        as.character(utils::packageVersion(.pkg_name)),
        error = function(...) "unknown"
      )
      cat(.pkg_name, " ", version, "\n", sep = "")
      invisible(NULL)
    },
    # @description Start the Arl REPL (interactive loop).
    do_repl = function() {
      load_prelude <- !isTRUE(self$parsed$no_stdlib)
      if (!self$cli_isatty()) {
        engine <- Engine$new(load_prelude = load_prelude)
        text <- paste(self$cli_read_stdin(), collapse = "\n")
        if (trimws(text) != "") {
          self$cli_eval_text(text, engine, source_name = "<stdin>")
        }
        return(invisible(NULL))
      }
      engine <- Engine$new(load_prelude = load_prelude)
      engine$repl()
      invisible(NULL)
    },
    # @description Evaluate files from parsed (--file and positional). Uses shared engine env.
    # @param parsed Result of parse().
    do_file = function(parsed) {
      engine <- Engine$new(load_prelude = !isTRUE(self$parsed$no_stdlib))
      for (path in parsed$files) {
        if (!file.exists(path)) {
          self$cli_exit_with_error(paste0("File not found: ", path), show_help = TRUE)
        }
      }
      # Run all files in the same engine env so definitions in one file are visible in the next
      for (path in parsed$files) {
        self$cli_eval_with_engine(engine, function() {
          engine$load_file_in_env(path)
        })
      }
      invisible(NULL)
    },
    # @description Evaluate --eval expression(s) and exit.
    # @param parsed Result of parse().
    do_eval = function(parsed) {
      engine <- Engine$new(load_prelude = !isTRUE(self$parsed$no_stdlib))
      self$cli_eval_text(parsed$expr, engine, source_name = "<cli>")
      invisible(NULL)
    },
    # @description Run fn(engine) with engine; on error, call cli_exit_with_error.
    # @param engine Engine instance.
    # @param fn Function(engine) to run.
    cli_eval_with_engine = function(engine, fn) {
      result_with_vis <- tryCatch(
        withVisible(fn()),
        error = function(e) {
          engine$print_error(e, file = stderr())
          self$cli_exit_with_error(conditionMessage(e), show_help = FALSE)
          return(list(value = NULL, visible = FALSE))
        }
      )
      result <- result_with_vis$value
      if (!is.null(result) && result_with_vis$visible) {
        cat(engine$format_value(result), "\n", sep = "")
      }
      invisible(result)
    },
    # @description Parse and evaluate text in engine; return result. Used by do_eval and tests.
    # @param text Character string of Arl code.
    # @param engine Engine instance.
    # @param source_name Source name for errors.
    # @return Result of evaluation.
    cli_eval_text = function(text, engine, source_name = "<cli>") {
      self$cli_eval_with_engine(
        engine,
        function() engine$eval_text(text, source_name = source_name)
      )
    },
    # @description Check if stdin is a terminal (for REPL vs batch).
    # @return Logical.
    cli_isatty = function() {
      override <- .pkg_option("cli_isatty_override")
      if (!is.null(override)) {
        if (is.function(override)) {
          return(isTRUE(override()))
        }
        return(isTRUE(override))
      }
      tty <- tryCatch(isatty(stdin()), error = function(...) FALSE)
      if (!isTRUE(tty)) {
        tty <- tryCatch(isatty(0), error = function(...) FALSE)
      }
      isTRUE(tty)
    },
    # @description Read all lines from stdin (for piping script into arl).
    # @return Character vector of lines.
    cli_read_stdin = function() {
      override <- .pkg_option("cli_read_stdin_override")
      if (!is.null(override)) {
        if (is.function(override)) {
          return(override())
        }
        return(override)
      }
      readLines("stdin", warn = FALSE)
    },
    # @description Main entry: parse args, then dispatch to do_repl, do_file, or do_eval.
    # @return Invisible; may exit.
    run = function() {
      parsed <- self$parsed
      if (is.null(parsed)) {
        parsed <- self$parse()
      }

      if (length(parsed$errors) > 0) {
        if (!isTRUE(.pkg_option("cli_quiet", FALSE))) {
          for (err in parsed$errors) {
            message("Error: ", err)
          }
          cat(CLI_HELP_TEXT, "\n", sep = "")
        }
        exit_fn <- .pkg_option("cli_exit_fn")
        if (!is.null(exit_fn) && is.function(exit_fn)) {
          exit_fn(paste(parsed$errors, collapse = "; "), TRUE)
        } else {
          stop(arl_cli_error(paste(parsed$errors, collapse = "; "), show_help = TRUE))
        }
        return(invisible(NULL))
      }

      handlers <- list(
        help = self$do_help,
        version = self$do_version,
        repl = self$do_repl,
        file = function() self$do_file(parsed),
        eval = function() self$do_eval(parsed)
      )

      handler <- handlers[[parsed$action]]
      if (is.null(handler)) {
        self$cli_exit_with_error(
          paste0("Unknown action: ", parsed$action),
          show_help = FALSE
        )
      }
      handler()
    }
  )
)

#' Run the Arl CLI
#'
#' Entry point for the Arl command-line interface. Parses arguments and runs
#' the requested action (REPL, file evaluation, or expression evaluation).
#'
#' @param args Command-line arguments to parse (defaults to \code{commandArgs(trailingOnly = TRUE)}).
#' @return Invisibly returns \code{NULL}.
#' @export
cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  CLI$new(args)$run()
}

#' Install the Arl CLI wrapper
#'
#' Prints platform-appropriate instructions for making the packaged CLI
#' wrapper available from the shell.  No files are written or copied --
#' the user follows the printed instructions themselves.
#'
#' @param quiet If \code{TRUE}, suppress printed instructions and return
#'   the script path invisibly.
#' @return The path to the CLI wrapper script, invisibly.
#' @export
install_cli <- function(quiet = FALSE) {
  is_windows <- .Platform$OS.type == "windows"

  script <- if (is_windows) {
    system.file("bin", "windows", "arl.bat", package = .pkg_name)
  } else {
    system.file("bin", "posix", "arl", package = .pkg_name)
  }

  if (!nzchar(script)) {
    stop("CLI script not found. Is the ", .pkg_name, " package installed?")
  }

  if (isTRUE(quiet)) {
    return(invisible(script))
  }

  if (is_windows) {
    message("Arl CLI wrapper script: ", script)
    message("")
    message("To make it available on your PATH, copy it to a directory on")
    message("your PATH or add its directory to PATH:")
    message("")
    message("  copy \"", script, "\" \"%USERPROFILE%\\bin\\arl.bat\"")
    message("")
    message("Then ensure %USERPROFILE%\\bin is on your PATH.")
  } else {
    message("Arl CLI wrapper script: ", script)
    message("")
    message("To make it available on your PATH, create a symlink:")
    message("")
    message("  mkdir -p ~/.local/bin")
    message("  ln -s \"", script, "\" ~/.local/bin/arl")
    message("")
    message("Then ensure ~/.local/bin is on your PATH.")
  }

  invisible(script)
}
