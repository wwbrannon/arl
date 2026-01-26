cli_help_text <- function() {
  paste(
    "Usage: rye [--repl] [--file <path>] [--eval <expr>]",
    "            [--version] [--help] [files...]",
    "",
    "Options:",
    "  -r, --repl           Start the Rye REPL (default).",
    "  -f, --file <path>    Evaluate a Rye source file.",
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
}

parse_cli_args <- function(args) {
  state <- list(
    action = "repl",
    files = character(0),
    expr = NULL,
    errors = character(0)
  )

  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]

    if (arg == "--") {
      if (i < length(args)) {
        state$files <- c(state$files, args[(i + 1):length(args)])
      }
      break
    }

    if (arg %in% c("--help", "-h")) {
      state$action <- "help"
      i <- i + 1
      next
    }

    if (arg %in% c("--version", "-v")) {
      state$action <- "version"
      i <- i + 1
      next
    }

    if (arg %in% c("--repl", "-r")) {
      state$action <- "repl"
      i <- i + 1
      next
    }

    if (arg %in% c("--file", "-f")) {
      if (i + 1 > length(args)) {
        state$errors <- c(state$errors, "--file requires a path.")
        i <- i + 1
        next
      }
      state$files <- c(state$files, args[[i + 1]])
      i <- i + 2
      next
    }

    if (arg %in% c("--eval", "-e")) {
      if (!is.null(state$expr)) {
        state$errors <- c(state$errors, "Only one --eval value is allowed.")
        i <- i + 1
        next
      }
      if (i + 1 > length(args)) {
        state$errors <- c(state$errors, "--eval requires an expression.")
        i <- i + 1
        next
      }
      state$expr <- args[[i + 1]]
      i <- i + 2
      next
    }

    if (startsWith(arg, "-")) {
      state$errors <- c(state$errors, paste("Unknown argument:", arg))
      i <- i + 1
      next
    }

    state$files <- c(state$files, arg)
    i <- i + 1
  }

  if (length(state$files) > 0 && !is.null(state$expr)) {
    state$errors <- c(state$errors, "Use only one of --file/files or --eval.")
  }

  if (length(state$files) > 0) {
    state$action <- "file"
  }
  if (!is.null(state$expr)) {
    state$action <- "eval"
  }

  state
}

cli_print_version <- function() {
  version <- tryCatch(
    as.character(utils::packageVersion("rye")),
    error = function(...) "unknown"
  )
  cat("rye ", version, "\n", sep = "")
}

cli_load_env <- function() {
  env <- new.env(parent = .GlobalEnv)
  rye_load_stdlib_base(env)
  env
}

cli_eval_exprs <- function(exprs, env) {
  result <- rye_eval_exprs(exprs, env)
  if (!is.null(result)) {
    print(result)
  }
  invisible(result)
}

cli_eval_text <- function(text, env) {
  result <- rye_eval_text(text, env)
  if (!is.null(result)) {
    print(result)
  }
  invisible(result)
}

cli_isatty <- function() {
  override <- getOption("rye.cli_isatty_override", NULL)
  if (!is.null(override)) {
    if (is.function(override)) {
      return(isTRUE(override()))
    }
    return(isTRUE(override))
  }
  isatty(0)
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

cli_error <- function(message) {
  cat("Error: ", message, "\n", sep = "", file = stderr())
}

rye_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  parsed <- parse_cli_args(args)

  if (length(parsed$errors) > 0) {
    if (!isTRUE(getOption("rye.cli_quiet", FALSE))) {
      for (err in parsed$errors) {
        cli_error(err)
      }
      cat(cli_help_text(), "\n", sep = "")
    }
    quit(save = "no", status = 1)
    return(invisible(NULL))
  }

  if (parsed$action == "help") {
    cat(cli_help_text(), "\n", sep = "")
    return(invisible(NULL))
  }

  if (parsed$action == "version") {
    cli_print_version()
    return(invisible(NULL))
  }

  if (parsed$action == "repl") {
    if (!cli_isatty()) {
      env <- cli_load_env()
      text <- paste(cli_read_stdin(), collapse = "\n")
      if (rye_trimws_shim(text) != "") { # nolint: object_usage_linter
        cli_eval_text(text, env)
      }
      return(invisible(NULL))
    }
    rye_repl()
    return(invisible(NULL))
  }

  env <- cli_load_env()

  if (parsed$action == "file") {
    for (path in parsed$files) {
      if (!file.exists(path)) {
        cli_error(paste("File not found:", path))
        quit(save = "no", status = 1)
      }
    }
    for (path in parsed$files) {
      result <- rye_load_file(path, env)
      if (!is.null(result)) {
        print(result)
      }
    }
    return(invisible(NULL))
  }

  if (parsed$action == "eval") {
    cli_eval_text(parsed$expr, env)
    return(invisible(NULL))
  }

  cli_error(paste("Unknown action:", parsed$action))
  quit(save = "no", status = 1)
}
