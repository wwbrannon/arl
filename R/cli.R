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

#' Core CLI implementation class
#'
#' @keywords internal
#' @noRd
#' @importFrom optparse OptionParser make_option parse_args
RyeCLI <- R6::R6Class(
  "RyeCLI",
  public = list(
    args = NULL,
    parsed = NULL,
    initialize = function(args = commandArgs(trailingOnly = TRUE)) {
      self$args <- args
    },
    cli_exit_with_error = function(message, show_help = TRUE) {
      exit_fn <- getOption("rye.cli_exit_fn", NULL)
      if (!is.null(exit_fn) && is.function(exit_fn)) {
        exit_fn(message, show_help)
        return(invisible(NULL))
      }
      cli::cli_alert_danger(message)
      if (isTRUE(show_help)) {
        cli::cat_line(CLI_HELP_TEXT)
      }
      quit(save = "no", status = 1)
    },
    cli_parser = function() {
      option_list <- list(
        optparse::make_option(
          c("-e", "--eval"),
          type = "character",
          default = NULL,
          metavar = "expr",
          help = "Evaluate a single Rye expression."
        )
      )
      optparse::OptionParser(
        usage = "usage: rye [options] [files...]",
        option_list = option_list,
        description = "Rye: A Lisp dialect for R. Use --help for help, --version for version."
      )
    },
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

      parser <- self$cli_parser()
      parsed <- tryCatch(
        {
          optparse::parse_args(
            parser,
            args = args_for_parse,
            positional_arguments = TRUE
          )
        },
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
    do_help = function() {
      cli::cat_line(CLI_HELP_TEXT)
      invisible(NULL)
    },
    do_version = function() {
      version <- tryCatch(
        as.character(utils::packageVersion("rye")),
        error = function(...) "unknown"
      )
      cli::cat_line("rye ", version)
      invisible(NULL)
    },
    do_repl = function() {
      if (!self$cli_isatty()) {
        engine <- RyeEngine$new(env = new.env(parent = .GlobalEnv))
        text <- paste(self$cli_read_stdin(), collapse = "\n")
        if (trimws(text) != "") {
          self$cli_eval_text(text, engine, source_name = "<stdin>")
        }
        return(invisible(NULL))
      }
      engine <- RyeEngine$new(env = new.env(parent = .GlobalEnv))
      engine$repl()
      invisible(NULL)
    },
    do_file = function(parsed) {
      engine <- RyeEngine$new(env = new.env(parent = .GlobalEnv))
      for (path in parsed$files) {
        if (!file.exists(path)) {
          self$cli_exit_with_error(paste0("File not found: ", path), show_help = TRUE)
        }
      }
      for (path in parsed$files) {
        self$cli_eval_with_engine(engine, function() engine$load_file(path))
      }
      invisible(NULL)
    },
    do_eval = function(parsed) {
      engine <- RyeEngine$new(env = new.env(parent = .GlobalEnv))
      self$cli_eval_text(parsed$expr, engine, source_name = "<cli>")
      invisible(NULL)
    },
    cli_eval_with_engine = function(engine, fn) {
      result_with_vis <- tryCatch(
        withVisible(fn()),
        error = function(e) {
          engine$source_tracker$print_error(e, file = stderr())
          self$cli_exit_with_error(conditionMessage(e), show_help = FALSE)
          return(list(value = NULL, visible = FALSE))
        }
      )
      result <- result_with_vis$value
      if (!is.null(result) && result_with_vis$visible) {
        cli::cat_line(engine$env$format_value(result))
      }
      invisible(result)
    },
    cli_eval_text = function(text, engine, source_name = "<cli>") {
      self$cli_eval_with_engine(
        engine,
        function() engine$eval_text(text, source_name = source_name)
      )
    },
    cli_isatty = function() {
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
    },
    cli_read_stdin = function() {
      override <- getOption("rye.cli_read_stdin_override", NULL)
      if (!is.null(override)) {
        if (is.function(override)) {
          return(override())
        }
        return(override)
      }
      readLines("stdin", warn = FALSE)
    },
    run = function() {
      parsed <- self$parsed
      if (is.null(parsed)) {
        parsed <- self$parse()
      }

      if (length(parsed$errors) > 0) {
        if (!isTRUE(getOption("rye.cli_quiet", FALSE))) {
          for (err in parsed$errors) {
            cli::cli_alert_danger(err)
          }
          cli::cat_line(CLI_HELP_TEXT)
        }
        exit_fn <- getOption("rye.cli_exit_fn", NULL)
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

#' Run the Rye CLI
#'
#' Entry point for the Rye command-line interface. Parses arguments and runs
#' the requested action (REPL, file evaluation, or expression evaluation).
#'
#' @param args Command-line arguments to parse (defaults to \code{commandArgs(trailingOnly = TRUE)}).
#' @return Invisibly returns \code{NULL}.
#' @export
rye_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  RyeCLI$new(args)$run()
}

#' Install the Rye CLI wrapper
#'
#' Copies the packaged CLI wrapper into a writable bin directory and makes it
#' executable so it can be run from the shell.
#'
#' @param target_dir Directory for the `rye` executable. Defaults to the
#'   \code{RYE_BIN_DIR} environment variable, then \code{~/.local/bin}, then \code{~/bin}.
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
