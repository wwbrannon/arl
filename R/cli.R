CLI_HELP_TEXT <- paste(
  "Rye: A Lisp dialect for R.",
  "",
  "Usage:",
  "  rye [--file <path>...] [--eval <expr>] [<files>...]",
  "  rye --version",
  "  rye --help",
  "",
  "Options:",
  "  -f, --file <path>    Evaluate a Rye source file (repeatable).",
  "  -e, --eval <expr>    Evaluate a single Rye expression.",
  "  -v, --version        Print version and exit.",
  "  -h, --help           Show this help message.",
  "",
  "Examples:",
  "  rye",
  "  rye --file script.rye",
  "  rye script.rye",
  "  rye --eval \"(+ 1 2)\"",
  sep = "\n"
)

parse_cli_args <- function(args) {
  state <- list(
    action = "repl",
    files = character(0),
    expr = NULL,
    errors = character(0)
  )

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

  parsed <- tryCatch(
    {
      value <- NULL
      utils::capture.output(
        {
          value <- docopt::docopt(
            CLI_HELP_TEXT,
            args = args,
            help = FALSE,
            version = NULL,
            strict = TRUE
          )
        },
        type = "output"
      )
      value
    },
    error = function(e) {
      state$errors <<- c(state$errors, conditionMessage(e))
      return(NULL)
    }
  )

  if (is.null(parsed)) {
    return(state)
  }

  files_flag <- parsed[["--file"]]
  files_pos <- parsed[["<files>"]]
  expr <- parsed[["--eval"]]

  if (!is.null(files_flag) && length(files_flag) > 0) {
    state$files <- c(state$files, files_flag)
  }
  if (!is.null(files_pos) && length(files_pos) > 0) {
    state$files <- c(state$files, files_pos)
  }
  if (length(args_after) > 0) {
    state$files <- c(state$files, args_after)
  }
  if (!is.null(expr) && !identical(expr, FALSE) && length(expr) > 0) {
    state$expr <- expr[[1]]
  }

  if (length(state$files) > 0 && !is.null(state$expr)) {
    state$errors <- c(state$errors, "Use only one of --file/files or --eval.")
  }

  if (isTRUE(parsed[["--help"]])) {
    state$action <- "help"
  } else if (isTRUE(parsed[["--version"]])) {
    state$action <- "version"
  } else if (!is.null(state$expr)) {
    state$action <- "eval"
  } else if (length(state$files) > 0) {
    state$action <- "file"
  }

  state
}

cli_load_env <- function() {
  engine <- RyeEngine$new(env = new.env(parent = .GlobalEnv))
  engine$load_stdlib()
  engine
}

cli_eval_with_engine <- function(engine, fn) {
  result <- tryCatch(
    fn(),
    error = function(e) {
      cli_print_error(e, engine)
      quit(save = "no", status = 1)
      return(invisible(NULL))
    }
  )
  if (!is.null(result)) {
    cat(engine$env$format_value(result), "\n", sep = "")
  }
  invisible(result)
}

cli_eval_text <- function(text, engine, source_name = "<cli>") {
  cli_eval_with_engine(
    engine,
    function() engine$eval_text(text, engine$env$env, source_name = source_name)
  )
}

cli_isatty <- function() {
  override <- getOption("rye.cli_isatty_override", NULL)
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
}

cli_read_stdin <- function() {
  override <- getOption("rye.cli_read_stdin_override", NULL)
  if (!is.null(override)) {
    if (is.function(override)) {
      return(override())
    }
    return(override)
  }
  readLines("stdin", warn = FALSE)
}

cli_print_error <- function(e, engine) {
  engine$source_tracker$print_error(e, file = stderr())
}

rye_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  parsed <- parse_cli_args(args)

  if (length(parsed$errors) > 0) {
    if (!isTRUE(getOption("rye.cli_quiet", FALSE))) {
      for (err in parsed$errors) {
        cat("Error: ", err, "\n", sep = "", file = stderr())
      }
      cat(CLI_HELP_TEXT, "\n", sep = "")
    }
    quit(save = "no", status = 1)
    return(invisible(NULL))
  }

  action_handlers <- list(
    help = function() {
      cat(CLI_HELP_TEXT, "\n", sep = "")
      invisible(NULL)
    },
    version = function() {
      version <- tryCatch(
        as.character(utils::packageVersion("rye")),
        error = function(...) "unknown"
      )
      cat("rye ", version, "\n", sep = "")
      invisible(NULL)
    },
    repl = function() {
      if (!cli_isatty()) {
        engine <- cli_load_env()
        text <- paste(cli_read_stdin(), collapse = "\n")
        if (trimws(text) != "") {
          cli_eval_text(text, engine, source_name = "<stdin>")
        }
        return(invisible(NULL))
      }
      rye_repl()
      invisible(NULL)
    },
    file = function() {
      engine <- cli_load_env()
      for (path in parsed$files) {
        if (!file.exists(path)) {
          cat("Error: File not found: ", path, "\n", sep = "", file = stderr())
          quit(save = "no", status = 1)
        }
      }
      for (path in parsed$files) {
        cli_eval_with_engine(
          engine,
          function() engine$load_file(path, engine$env$env)
        )
      }
      invisible(NULL)
    },
    eval = function() {
      engine <- cli_load_env()
      cli_eval_text(parsed$expr, engine, source_name = "<cli>")
      invisible(NULL)
    }
  )

  handler <- action_handlers[[parsed$action]]
  if (is.null(handler)) {
    cat("Error: Unknown action: ", parsed$action, "\n", sep = "", file = stderr())
    quit(save = "no", status = 1)
  }
  handler()
}

#' Install the Rye CLI wrapper
#'
#' Copies the packaged CLI wrapper into a writable bin directory and makes it
#' executable so it can be run from the shell.
#'
#' @param target_dir Directory for the `rye` executable. Defaults to the
#'   `RYE_BIN_DIR` environment variable, then `~/.local/bin`, then `~/bin`.
#' @param overwrite Whether to overwrite an existing `rye` executable.
#' @return The installed path, invisibly.
#' @export
rye_install_cli <- function(target_dir = Sys.getenv("RYE_BIN_DIR", unset = ""), overwrite = FALSE) {
  source <- system.file("exec", "rye", package = "rye")
  if (!nzchar(source)) {
    stop("CLI script not found. Is the rye package installed?")
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
      "No writable bin directory found. Set RYE_BIN_DIR or create one of: ",
      paste(candidates, collapse = ", ")
    )
  }

  target <- file.path(chosen, "rye")
  if (file.exists(target) && !isTRUE(overwrite)) {
    stop("CLI already exists at ", target, ". Use overwrite = TRUE to replace it.")
  }

  if (!file.copy(source, target, overwrite = TRUE)) {
    stop("Failed to install CLI to ", target)
  }

  Sys.chmod(target, mode = "0755")

  message("Installed rye CLI to ", target)

  path_entries <- strsplit(Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE)[[1]]
  normalized_entries <- normalizePath(path_entries, winslash = "/", mustWork = FALSE)
  normalized_target <- normalizePath(chosen, winslash = "/", mustWork = FALSE)
  if (!any(normalized_entries == normalized_target)) {
    message("Add ", chosen, " to your PATH to run `rye` from the shell.")
  }

  invisible(target)
}
