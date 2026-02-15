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
  "CLI",
  public = list(
    args = NULL,
    parsed = NULL,
    # @description Create CLI with optional args (default: commandArgs(trailingOnly = TRUE)).
    # @param args Character vector of command-line arguments.
    initialize = function(args = commandArgs(trailingOnly = TRUE)) {
      self$args <- args
    },
    # @description Print error and optionally help text; exit via arl.cli_exit_fn or quit().
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
      quit(save = "no", status = 1)
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
      load_stdlib <- !isTRUE(self$parsed$no_stdlib)
      if (!self$cli_isatty()) {
        engine <- Engine$new(env = new.env(parent = .GlobalEnv), load_stdlib = load_stdlib)
        text <- paste(self$cli_read_stdin(), collapse = "\n")
        if (trimws(text) != "") {
          self$cli_eval_text(text, engine, source_name = "<stdin>")
        }
        return(invisible(NULL))
      }
      engine <- Engine$new(env = new.env(parent = .GlobalEnv), load_stdlib = load_stdlib)
      engine$repl()
      invisible(NULL)
    },
    # @description Evaluate files from parsed (--file and positional). Uses shared engine env.
    # @param parsed Result of parse().
    do_file = function(parsed) {
      engine <- Engine$new(env = new.env(parent = .GlobalEnv),
                           load_stdlib = !isTRUE(self$parsed$no_stdlib))
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
      engine <- Engine$new(env = new.env(parent = .GlobalEnv),
                           load_stdlib = !isTRUE(self$parsed$no_stdlib))
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
          quit(save = "no", status = 1)
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

# Check if two file paths refer to the same file (handles symlinks and hardlinks)
# @param path1 First file path
# @param path2 Second file path
# @return TRUE if both paths refer to the same file, FALSE otherwise
# @keywords internal
# @noRd
same_file <- function(path1, path2) {
  # If either file doesn't exist, they can't be the same
  if (!file.exists(path1) || !file.exists(path2)) {
    return(FALSE)
  }

  # Resolve symlinks and compare canonical paths
  path1_real <- normalizePath(path1, winslash = "/", mustWork = TRUE)
  path2_real <- normalizePath(path2, winslash = "/", mustWork = TRUE)

  if (path1_real == path2_real) {
    return(TRUE)
  }

  # Also check inodes for hardlinks (Unix-like systems only)
  info1 <- file.info(path1_real)
  info2 <- file.info(path2_real)

  if (!is.null(info1$inode) && !is.null(info2$inode)) {
    return(info1$inode == info2$inode)
  }

  FALSE
}

# Warn if the given directory is not on the current PATH.
# @param dir_path Expanded directory path.
# @keywords internal
# @noRd
install_cli_path_warning <- function(dir_path) {
  path_entries <- strsplit(Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE)[[1]]
  normalized_entries <- normalizePath(path_entries, winslash = "/", mustWork = FALSE)
  normalized_target <- normalizePath(dir_path, winslash = "/", mustWork = FALSE)
  if (!any(normalized_entries == normalized_target)) {
    message("Add ", dir_path, " to your PATH to run `arl` from the shell.")
  }
}

#' Install the Arl CLI wrapper
#'
#' Copies the packaged CLI wrapper into a writable bin directory and makes it
#' executable so it can be run from the shell.
#'
#' @param target_dir Directory for the `arl` executable. Defaults to the
#'   \code{arl.bin_dir} option or \code{ARL_BIN_DIR} environment variable,
#'   then \code{~/.local/bin}, then \code{~/bin}.
#' @param overwrite Whether to overwrite an existing `arl` executable.
#' @return The installed path, invisibly.
#' @export
install_cli <- function(target_dir = .pkg_option("bin_dir", ""), overwrite = FALSE) {
  source <- system.file("exec", .pkg_name, package = .pkg_name)
  if (!nzchar(source)) {
    stop(paste0("CLI script not found. Is the ", .pkg_name, " package installed?"))
  }

  candidates <- if (nzchar(target_dir)) {
    target_dir
  } else {
    c("~/.local/bin", "~/bin")
  }

  chosen <- NULL
  for (dir in candidates) {
    dir_path <- path.expand(dir)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }
    if (dir.exists(dir_path) && file.access(dir_path, 2) == 0) {
      chosen <- dir_path
      break
    }
  }

  if (is.null(chosen)) {
    stop(
      "No writable bin directory found. Set the arl.bin_dir option or ",
      "ARL_BIN_DIR env var, or create one of: ",
      paste(candidates, collapse = ", ")
    )
  }

  target <- file.path(chosen, .pkg_name)

  # Check if source and target are the same file (can happen in dev mode)
  # This includes symlinks and hardlinks
  if (same_file(source, target)) {
    message("Target already points to source, skipping copy")
    message("Already installed at ", target)
    install_cli_path_warning(chosen)
    return(invisible(target))
  }

  if (file.exists(target) && !isTRUE(overwrite)) {
    stop("CLI already exists at ", target, ". Use overwrite = TRUE to replace it.")
  }

  # If target is a symlink, remove it first to avoid following the link
  # and overwriting the source file
  if (file.exists(target)) {
    file.remove(target)
  }

  if (!file.copy(source, target, overwrite = FALSE)) {
    stop("Failed to install CLI to ", target)
  }

  Sys.chmod(target, mode = "0755")

  message("Installed ", .pkg_name, " CLI to ", target)

  install_cli_path_warning(chosen)

  invisible(target)
}
