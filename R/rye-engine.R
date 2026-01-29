#' Core Rye engine
#'
#' Provides class-based organization for tokenization, parsing, macro expansion,
#' evaluation, and environment management.
#'
#' @importFrom R6 R6Class
#' @field tokenizer Tokenizer instance used to lex source text.
#' @field parser Parser instance used to read expressions.
#' @field macro_expander Macro expander for Rye macros.
#' @field evaluator Evaluator used for expression execution.
#' @field help_system Help system for Rye topics.
#' @field env RyeEnv backing the engine.
#' @field source_tracker Source tracker used for error context.
#' @param env Optional environment or RyeEnv used as the engine base.
#' @param source Character string containing Rye source.
#' @param source_name Optional source name for error reporting.
#' @param tokens Token list produced by the tokenizer.
#' @param expr Rye expression (symbol/call/atomic value).
#' @param exprs List of Rye expressions to evaluate.
#' @param text Character string of Rye code to read/eval.
#' @param path File path to load.
#' @param preserve_src Logical; keep source metadata when macroexpanding.
#' @param topic Help topic as a single string.
#' @export
RyeEngine <- R6::R6Class(
  "RyeEngine",
  public = list(
    tokenizer = NULL,
    parser = NULL,
    macro_expander = NULL,
    evaluator = NULL,
    help_system = NULL,
    env = NULL,
    source_tracker = NULL,
    #' @description
    #' Initialize engine components and base environment.
    initialize = function(env = NULL) {
      self$env <- RyeEnv$new(env)
      self$source_tracker <- SourceTracker$new()
      self$tokenizer <- Tokenizer$new()
      self$parser <- Parser$new(self$source_tracker)
      self$macro_expander <- MacroExpander$new(self$env, self$source_tracker)
      self$evaluator <- Evaluator$new(self$env, self$macro_expander, self$source_tracker, engine = self)
      self$macro_expander$evaluator <- self$evaluator
      self$help_system <- HelpSystem$new(self)
    },
    #' @description
    #' Tokenize and parse source into expressions.
    read = function(source, source_name = NULL) {
      tokens <- self$tokenizer$tokenize(source)
      self$parser$parse(tokens, source_name = source_name)
    },
    #' @description
    #' Tokenize source into Rye tokens.
    tokenize = function(source) {
      self$tokenizer$tokenize(source)
    },
    #' @description
    #' Parse tokens into expressions.
    parse = function(tokens, source_name = NULL) {
      self$parser$parse(tokens, source_name = source_name)
    },
    #' @description
    #' Evaluate a single expression.
    eval = function(expr, env = NULL) {
      self$evaluator$eval(expr, env)
    },
    #' @description
    #' Evaluate expressions in order.
    eval_seq = function(exprs, env = NULL) {
      self$evaluator$eval_seq(exprs, env)
    },
    #' @description
    #' Evaluate expressions with source tracking.
    eval_exprs = function(exprs, env = NULL) {
      target_env <- rye_env_resolve(env, fallback = self$env)
      self$source_tracker$with_error_context(function() {
        self$eval_seq(exprs, target_env)
      })
    },
    #' @description
    #' Read and evaluate text.
    eval_text = function(text, env = NULL, source_name = "<eval>") {
      exprs <- self$read(text, source_name = source_name)
      self$eval_exprs(exprs, env = env)
    },
    #' @description
    #' Populate standard bindings in an environment.
    initialize_environment = function(env = NULL) {
      if (inherits(env, "RyeEnv")) {
        env <- env$env
      }
      if (is.null(env)) {
        env <- new.env(parent = baseenv())
      }

      if (!exists(".rye_env", envir = env, inherits = FALSE)) {
        assign(".rye_env", TRUE, envir = env)
      }
      rye_env_registry(env, ".rye_module_registry", create = TRUE)
      rye_env_registry(env, ".rye_macros", create = TRUE)

      env$apply <- function(fn, args) {
        args <- rye_as_list(args)
        if (length(args) > 2 &&
              (identical(fn, base::`+`) || identical(fn, base::`*`) ||
                 identical(fn, base::`-`) || identical(fn, base::`/`))) {
          return(Reduce(fn, args))
        }
        self$evaluator$do_call(fn, args)
      }
      attr(env$apply, "rye_doc") <- attr(rye_stdlib_apply, "rye_doc", exact = TRUE)

      env$`try*` <- rye_stdlib_try

      env$gensym <- function(prefix = "G") {
        self$macro_expander$gensym(prefix = prefix)
      }

      env$capture <- function(symbol, expr) {
        self$macro_expander$capture(symbol, expr)
      }

      env$`macro?` <- function(x) {
        is.symbol(x) && self$macro_expander$is_macro(x, env = env)
      }

      env$macroexpand <- function(expr, preserve_src = FALSE) {
        self$macro_expander$macroexpand(expr, env = env, preserve_src = preserve_src)
      }

      env$`macroexpand-1` <- function(expr, preserve_src = FALSE) {
        self$macro_expander$macroexpand_1(expr, env = env, preserve_src = preserve_src)
      }

      env$`macroexpand-all` <- env$macroexpand

      env$eval <- function(expr, env = parent.frame()) {
        self$eval(expr, env)
      }
      attr(env$eval, "rye_doc") <- attr(rye_stdlib_eval, "rye_doc", exact = TRUE)

      env$`current-env` <- rye_stdlib_current_env
      env$`promise?` <- rye_stdlib_promise_p
      env$force <- rye_stdlib_force
      attr(env$force, "rye_doc") <- attr(rye_stdlib_force, "rye_doc", exact = TRUE)
      attr(env$`promise?`, "rye_doc") <- attr(rye_stdlib_promise_p, "rye_doc", exact = TRUE)

      env$rye_read <- function(source, source_name = NULL) {
        self$read(source, source_name = source_name)
      }
      env$rye_parse <- function(tokens, source_name = NULL) {
        self$parse(tokens, source_name = source_name)
      }
      env$rye_tokenize <- function(source) {
        self$tokenize(source)
      }

      stdlib_env <- env
      env$`r/call` <- function(fn, args = list()) {
        fn_name <- if (is.symbol(fn)) {
          as.character(fn)
        } else if (is.character(fn)) {
          fn
        } else {
          stop("r/call requires a symbol or string function name")
        }
        fn_obj <- rye_resolve_r_callable(fn_name, stdlib_env = stdlib_env, max_frames = 5)
        self$evaluator$do_call(fn_obj, rye_as_list(args))
      }
      attr(env$`r/call`, "rye_doc") <- attr(rye_stdlib_r_call, "rye_doc", exact = TRUE)

      env$`r/eval` <- function(expr, env = NULL) {
        if (is.null(env)) {
          env <- rye_stdlib_current_env()
        }
        expr <- self$macro_expander$hygiene_unwrap(expr)
        saved <- list()
        if (is.call(expr) && length(expr) > 0) {
          op <- expr[[1]]
          if (is.symbol(op)) {
            op_name <- as.character(op)
            if (op_name == "while" && exists("while", envir = env, inherits = FALSE)) {
              saved[["while"]] <- get("while", envir = env, inherits = FALSE)
              rm("while", envir = env, inherits = FALSE)
            }
            if (op_name == "for" && exists("for", envir = env, inherits = FALSE)) {
              saved[["for"]] <- get("for", envir = env, inherits = FALSE)
              rm("for", envir = env, inherits = FALSE)
            }
          }
        }
        on.exit({
          if (!is.null(saved[["while"]])) {
            assign("while", saved[["while"]], envir = env)
          }
          if (!is.null(saved[["for"]])) {
            assign("for", saved[["for"]], envir = env)
          }
        }, add = TRUE)
        eval(expr, env)
      }
      attr(env$`r/eval`, "rye_no_quote") <- TRUE
      attr(env$`r/eval`, "rye_doc") <- attr(rye_stdlib_r_eval, "rye_doc", exact = TRUE)

      loader_path <- rye_resolve_module_path("_stdlib_loader")
      self$eval(self$read(sprintf('(load "%s")', loader_path))[[1]], env)

      env
    },
    #' @description
    #' Load and evaluate a Rye source file.
    load_file = function(path, env = NULL) {
      if (!is.character(path) || length(path) != 1) {
        stop("load requires a single file path string")
      }
      if (!file.exists(path)) {
        stop(sprintf("File not found: %s", path))
      }
      text <- paste(readLines(path, warn = FALSE), collapse = "\n")
      target_env <- rye_env_resolve(env, fallback = self$env)
      self$source_tracker$with_error_context(function() {
        self$eval_seq(self$read(text, source_name = path), target_env)
      })
    },
    #' @description
    #' Initialize the standard library in an environment.
    load_stdlib = function(env = NULL) {
      target_env <- rye_env_resolve(env, fallback = self$env)
      self$initialize_environment(target_env)
    },
    #' @description
    #' Expand macros recursively.
    macroexpand = function(expr, env = NULL, preserve_src = FALSE) {
      target_env <- rye_env_resolve(env, fallback = self$env)
      self$macro_expander$macroexpand(expr, env = target_env, preserve_src = preserve_src)
    },
    #' @description
    #' Expand a single macro layer.
    macroexpand_1 = function(expr, env = NULL, preserve_src = FALSE) {
      target_env <- rye_env_resolve(env, fallback = self$env)
      self$macro_expander$macroexpand_1(expr, env = target_env, preserve_src = preserve_src)
    },
    #' @description
    #' Show help for a topic.
    help = function(topic, env = NULL) {
      self$help_system$help(topic, env = env)
    },
    #' @description
    #' Start the Rye REPL using this engine.
    repl = function() {
      rye_repl(engine = self)
    }
  )
)

.rye_engine_state <- new.env(parent = emptyenv())

#' Get the default Rye engine
#'
#' @export
rye_default_engine <- function() {
  engine <- get0("engine", envir = .rye_engine_state, inherits = FALSE)
  if (is.null(engine)) {
    engine <- RyeEngine$new()
    assign("engine", engine, envir = .rye_engine_state)
  }
  engine
}
