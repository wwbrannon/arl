#' Core Arl engine
#'
#' @description
#' Provides class-based organization for tokenization, parsing, macro expansion,
#' evaluation, and environment management.
#'
#' @importFrom R6 R6Class
#' @field use_env_cache Logical. If TRUE, enables the env cache (full serialized
#'   environment) which is faster but only safe when dependencies don't change.
#'   If FALSE (default), only the expr cache (compiled expressions) is used, which
#'   is always safe. Can be set via engine initialization parameter or global
#'   option `arl.use_env_cache`.
#' @param env Optional environment or Env used as the engine base.
#' @param parent Optional parent environment for the new environment.
#' @param source Character string containing Arl source.
#' @param source_name Optional source name for error reporting.
#' @param tokens Token list produced by the tokenizer.
#' @param expr Arl expression (symbol/call/atomic value).
#' @param ... Additional Arl expressions to evaluate (variadic).
#' @param text Character string of Arl code to read/eval.
#' @param path File path to load.

#' @param preserve_src Logical; keep source metadata when macroexpanding.
#' @param depth Number of expansion steps (NULL for full expansion).
#' @param topic Help topic as a single string.
#' @param load_stdlib Logical; if TRUE (the default), load all stdlib modules.
#' @param value Value to format for display.
#' @param fn A zero-argument function to call with error context.
#' @param file Connection to print to.
#' @param e A condition object.
#' @examples
#' engine <- Engine$new()
#' engine$eval_text("(+ 1 2 3)")
#' @export
Engine <- R6::R6Class(
  "Engine",

  cloneable = FALSE,

  public = list(
    use_env_cache = NULL,

    #' @description
    #' Initialize engine components and base environment.
    #' @param env Optional existing environment to use. If NULL, creates a new environment.
    #' @param parent Optional parent environment for the new environment. Only used if env is NULL.
    #'   Defaults to baseenv(). Cannot be specified together with env.
    #' @param use_env_cache Optional logical. If TRUE, enables the env cache for 4x faster
    #'   module loading. Only safe when dependencies don't change. Defaults to NULL, which
    #'   inherits from global option `getOption("arl.use_env_cache", FALSE)`.
    #' @param coverage_tracker Optional CoverageTracker instance to enable coverage tracking
    #'   from the start. If provided, coverage will be tracked during stdlib loading.
    #' @param load_stdlib Logical. If TRUE (the default), loads all stdlib modules
    #'   during initialization. Set to FALSE to create a bare engine with only
    #'   builtins — useful for testing or when you want to import specific modules.
    initialize = function(env = NULL, parent = NULL, use_env_cache = NULL,
                          coverage_tracker = NULL, load_stdlib = TRUE) {
      # Priority: explicit parameter > global option > default FALSE
      if (is.null(use_env_cache)) {
        self$use_env_cache <- .pkg_option("use_env_cache", FALSE)
      } else {
        self$use_env_cache <- isTRUE(use_env_cache)
      }

      # Show one-time warning if enabled
      if (self$use_env_cache && !.pkg_option("env_cache_warning_shown", FALSE)) {
        message(
          "Note: Environment cache is enabled. ",
          "This provides 4x speedup but is only safe when dependencies don't change. ",
          paste0("Disable with options(", .pkg_name, ".use_env_cache = FALSE) if working with changing code.")
        )
        .set_pkg_option("env_cache_warning_shown", TRUE)
      }

      private$.env <- Env$new(env = env, parent = parent)
      private$.source_tracker <- SourceTracker$new()
      private$.tokenizer <- Tokenizer$new()
      private$.parser <- Parser$new(private$.source_tracker)
      private$.module_cache <- ModuleCache$new()

      # Create shared evaluation context
      context <- EvalContext$new(private$.env, private$.source_tracker, self$use_env_cache, coverage_tracker)

      # Create components with context
      private$.macro_expander <- MacroExpander$new(context)
      # Link components in context
      context$macro_expander <- private$.macro_expander

      private$.compiler <- Compiler$new(context)
      # Disable constant folding when coverage is active — folding evaluates
      # via base:: and bypasses Arl function bodies, defeating instrumentation.
      if (!is.null(coverage_tracker)) {
        private$.compiler$enable_constant_folding <- FALSE
      }
      context$compiler <- private$.compiler

      private$.compiled_runtime <- CompiledRuntime$new(
        context,
        load_file_fn = function(path, env) self$load_file_in_env(path, env),
        run_file_fn = function(path, env) self$load_file_under_env(path, env),
        help_fn = function(topic, env) self$help(topic, env),
        module_cache = private$.module_cache
      )
      context$compiled_runtime <- private$.compiled_runtime
      private$.help_system <- HelpSystem$new(private$.env, private$.macro_expander)

      private$.initialize_environment(load_stdlib = isTRUE(load_stdlib))
    },

    #' @description
    #' Tokenize and parse source into expressions. The format returned by this
    #' method is not guaranteed to be stable across package versions.
    read = function(source, source_name = NULL) {
      tokens <- private$.tokenizer$tokenize(source)
      private$.parser$parse(tokens, source_name = source_name)
    },

    #' @description
    #' Convert an Arl expression to its string representation. Inverse of read().
    #' The format returned by this method is not guaranteed to be stable across
    #' package versions.
    write = function(expr) {
      private$.parser$write(expr)
    },

    #' @description
    #' Evaluate one or more expressions.
    eval = function(expr, ..., env = NULL) {
      target_env <- private$resolve_env_arg(env)
      exprs <- c(list(expr), list(...))
      private$.source_tracker$with_error_context(function() {
        compiler <- private$.compiler
        source_tracker <- private$.source_tracker
        compiled_runtime <- private$.compiled_runtime
        coverage_tracker <- compiled_runtime$context$coverage_tracker
        result <- NULL
        result_visible <- FALSE
        for (e in exprs) {
          if (!is.null(coverage_tracker) && coverage_tracker$enabled) {
            src_cov <- source_tracker$src_get(e)
            if (!is.null(src_cov) && !is.null(src_cov$file) && !is.null(src_cov$start_line)) {
              coverage_tracker$register_coverable(src_cov$file, src_cov$start_line, src_cov$start_line)
              coverage_tracker$track(list(
                file = src_cov$file,
                start_line = src_cov$start_line,
                end_line = src_cov$start_line
              ))
            }
          }
          expanded <- self$macroexpand(e, env = target_env, preserve_src = TRUE)
          compiled <- compiler$compile(expanded, target_env, strict = TRUE)
          src <- source_tracker$src_get(e)
          if (!is.null(src)) {
            source_tracker$push(src)
          }
          result_with_vis <- withVisible(compiled_runtime$eval_compiled(compiled, target_env))
          if (!is.null(src)) {
            source_tracker$pop()
          }
          result <- result_with_vis$value
          result_visible <- isTRUE(result_with_vis$visible)
        }
        result <- source_tracker$strip_src(result)
        if (isTRUE(result_visible)) {
          return(result)
        }
        invisible(result)
      })
    },

    #' @description
    #' Read and evaluate Arl source text. Convenience wrapper around
    #' \code{read()} and \code{eval()}.
    eval_text = function(text, env = NULL, source_name = "<eval>") {
      exprs <- self$read(text, source_name = source_name)
      if (length(exprs) == 0L) return(invisible(NULL))
      # Stash raw text so module_compiled can parse ;;' annotations from strings
      private$.compiled_runtime$context$compiler$source_text <- text
      on.exit(private$.compiled_runtime$context$compiler$source_text <- NULL)
      do.call(self$eval, c(exprs, list(env = env)), quote = TRUE)
    },


    #' @description
    #' Load and evaluate an Arl source file in an isolated scope. The file runs in a
    #' child of \code{env}, so definitions and imports in the file are not visible
    #' in \code{env} or to subsequent code. For source-like behavior (definitions
    #' visible in the engine), use \code{load_file_in_env(path, env)}.
    load_file_under_env = function(path, env = NULL) {
      resolved <- private$resolve_env_arg(env)
      self$load_file_in_env(path, new.env(parent = resolved))
    },

    #' @description
    #' Load and evaluate an Arl source file in the given environment. Definitions
    #' and imports in the file are visible in \code{env}. For isolated execution
    #' (definitions not visible), use \code{load_file_under_env(path, env)}.
    load_file_in_env = function(path, env) {
      if (!is.character(path) || length(path) != 1) {
        stop("load requires a single file path string")
      }
      resolved <- private$resolve_env_arg(env)
      module_registry <- private$.env$module_registry
      if (!grepl("[/\\\\]", path)) {
        if (module_registry$exists(path)) {
          return(invisible(NULL))
        }
        stdlib_path <- resolve_stdlib_path(path)
        if (!is.null(stdlib_path)) {
          path <- stdlib_path
        }
      }
      if (!file.exists(path)) {
        stop(sprintf("File not found: %s", path))
      }

      # Try cache loading for module files
      # Skip cache when coverage is enabled — cached expressions lack source info for instrumentation
      coverage_active <- !is.null(private$.compiled_runtime$context$coverage_tracker) &&
                         private$.compiled_runtime$context$coverage_tracker$enabled
      if (!coverage_active) {
        cache_paths <- private$.module_cache$get_paths(path)
        if (!is.null(cache_paths)) {
          target_env <- resolved

          # Try env cache first (fastest - full serialized environment)
          # ONLY if use_env_cache is enabled
          if (isTRUE(self$use_env_cache) && file.exists(cache_paths$env_cache)) {
            cache_data <- private$.module_cache$load_env(cache_paths$env_cache, target_env, path)
            if (!is.null(cache_data)) {
              # Register the cached module
              module_env <- cache_data$module_env
              module_name <- cache_data$module_name
              exports <- cache_data$exports

              module_registry$register(module_name, module_env, exports)

              # Also register by absolute path
              absolute_path <- normalize_path_absolute(path)
              module_registry$alias(absolute_path, module_name)

              return(invisible(NULL))
            }
          }

          # Fallback to expr cache (compiled expressions)
          if (file.exists(cache_paths$code_cache)) {
            cache_data <- private$.module_cache$load_code(cache_paths$code_cache, path)
            if (!is.null(cache_data)) {
              # Recreate module environment (like module_compiled does)
              module_name <- cache_data$module_name
              exports <- cache_data$exports
              export_all <- cache_data$export_all

              module_env <- new.env(parent = target_env)
              assign(".__module", TRUE, envir = module_env)
              lockBinding(".__module", module_env)

              module_registry$register(module_name, module_env, exports)

              # Register by absolute path
              absolute_path <- normalize_path_absolute(path)
              module_registry$alias(absolute_path, module_name)

              # Install helpers and setup
              private$.compiled_runtime$install_helpers(module_env)

              # Evaluate cached compiled expressions in module environment
              if (length(cache_data$compiled_body) == 1L) {
                result <- private$.compiled_runtime$eval_compiled(cache_data$compiled_body[[1L]], module_env)
              } else {
                block <- as.call(c(list(quote(`{`)), cache_data$compiled_body))
                result <- private$.compiled_runtime$eval_compiled(block, module_env)
              }

              # Handle export_all
              if (export_all) {
                exports <- setdiff(ls(module_env, all.names = TRUE), ".__module")
                module_registry$update_exports(module_name, exports)
              }

              return(invisible(result))
            }
          }
        }
      }

      # Cache miss - full load
      text <- paste(readLines(path, warn = FALSE), collapse = "\n")
      exprs <- self$read(text, source_name = path)
      if (length(exprs) == 0L) return(invisible(NULL))
      target_env <- resolved
      do.call(self$eval, c(exprs, list(env = target_env)), quote = TRUE)
    },

    #' @description
    #' Expand macros in an expression. With \code{depth = NULL} (the default),
    #' fully and recursively expand all macros. With \code{depth = N}, expand
    #' only the top-level macro up to N times without walking into subexpressions.
    macroexpand = function(expr, env = NULL, depth = NULL, preserve_src = FALSE) {
      target_env <- private$resolve_env_arg(env)
      private$.macro_expander$macroexpand(expr, env = target_env,
                                          preserve_src = preserve_src, depth = depth)
    },

    #' @description
    #' Inspect expansion and compilation for debugging. Parse text, expand macros in env,
    #' then compile to R. Returns parsed AST, expanded form, compiled R expression, and
    #' deparsed R code so you can see exactly what an Arl program becomes.
    #' @param text Character; Arl source (single expression or multiple).
    #' @param env Environment or NULL (use engine env). Must have macros/stdlib if needed.
    #' @param source_name Name for parse errors.
    #' @return List with \code{parsed} (first expr), \code{expanded}, \code{compiled} (R expr or NULL), \code{compiled_deparsed} (character, or NULL).
    inspect_compilation = function(text, env = NULL, source_name = "<inspect>") {
      target_env <- private$resolve_env_arg(env)
      exprs <- self$read(text, source_name = source_name)
      if (length(exprs) == 0) {
        return(list(parsed = NULL, expanded = NULL, compiled = NULL, compiled_deparsed = NULL))
      }
      parsed <- exprs[[1]]
      expanded <- self$macroexpand(parsed, env = target_env)
      compiled <- tryCatch(
        private$.compiler$compile(expanded, target_env, strict = FALSE),
        error = function(e) NULL
      )
      compiled_deparsed <- if (!is.null(compiled)) deparse(compiled) else NULL
      list(parsed = parsed, expanded = expanded, compiled = compiled, compiled_deparsed = compiled_deparsed)
    },

    #' @description
    #' Show help for a topic.
    help = function(topic, env = NULL) {
      target_env <- private$resolve_env_arg(env)
      private$.help_system$help_in_env(topic, target_env)
    },

    #' @description
    #' Start the Arl REPL using this engine.
    repl = function() {
      REPL$new(engine = self)$run()
    },

    #' @description
    #' Enable coverage tracking.
    #'
    #' Creates a coverage tracker and installs it in the eval context.
    #' Should be called before running code you want to track.
    enable_coverage = function() {
      if (!requireNamespace("R6", quietly = TRUE)) {
        stop("R6 package required for coverage tracking")
      }

      # Create tracker if needed
      if (is.null(private$.compiled_runtime$context$coverage_tracker)) {
        private$.compiled_runtime$context$coverage_tracker <- CoverageTracker$new()
      }

      private$.compiled_runtime$context$coverage_tracker$set_enabled(TRUE)
      invisible(self)
    },

    #' @description
    #' Disable coverage tracking.
    disable_coverage = function() {
      if (!is.null(private$.compiled_runtime$context$coverage_tracker)) {
        private$.compiled_runtime$context$coverage_tracker$set_enabled(FALSE)
      }
      invisible(self)
    },

    #' @description
    #' Get coverage data as a data frame.
    #'
    #' @return A data frame with columns \code{file}, \code{total_lines},
    #'   \code{covered_lines}, and \code{coverage_pct} (one row per tracked file),
    #'   with a \code{"total"} attribute containing aggregate stats.
    #'   Returns NULL if coverage tracking is not enabled.
    get_coverage = function() {
      tracker <- private$.compiled_runtime$context$coverage_tracker
      if (is.null(tracker)) return(NULL)

      if (length(tracker$all_files) == 0) {
        tracker$discover_files()
      }

      summary <- tracker$get_summary()

      files <- character(0)
      total_lines <- integer(0)
      covered_lines <- integer(0)

      for (file in tracker$all_files) {
        if (!file.exists(file)) next

        coverable <- tracker$coverable_lines[[file]]
        if (is.null(coverable)) coverable <- tracker$code_lines[[file]]
        n_total <- if (!is.null(coverable)) length(coverable) else 0L

        file_cov <- summary[[file]]
        n_covered <- if (!is.null(file_cov) && !is.null(coverable)) {
          length(intersect(as.integer(names(file_cov)), coverable))
        } else if (!is.null(file_cov)) {
          length(file_cov)
        } else {
          0L
        }

        files <- c(files, file)
        total_lines <- c(total_lines, n_total)
        covered_lines <- c(covered_lines, n_covered)
      }

      result <- data.frame(
        file = files,
        total_lines = total_lines,
        covered_lines = covered_lines,
        coverage_pct = ifelse(total_lines > 0, covered_lines / total_lines * 100, 0),
        stringsAsFactors = FALSE
      )

      attr(result, "total") <- list(
        total_lines = sum(total_lines),
        covered_lines = sum(covered_lines),
        coverage_pct = if (sum(total_lines) > 0) {
          sum(covered_lines) / sum(total_lines) * 100
        } else {
          0
        }
      )

      result
    },

    #' @description
    #' Reset coverage data.
    reset_coverage = function() {
      if (!is.null(private$.compiled_runtime$context$coverage_tracker)) {
        private$.compiled_runtime$context$coverage_tracker$reset()
      }
      invisible(self)
    },

    #' @description
    #' Get the top-level R environment backing this engine.
    #' @return An R environment.
    get_env = function() {
      private$.env$env
    },

    #' @description
    #' Format a value for display using the engine's formatter.
    #' @param value Value to format.
    #' @return Character string.
    format_value = function(value) {
      private$.env$format_value(value)
    },

    #' @description
    #' Run a function with source-tracking error context.
    #' @param fn A zero-argument function to call.
    #' @return The return value of \code{fn}.
    with_error_context = function(fn) {
      private$.source_tracker$with_error_context(fn)
    },

    #' @description
    #' Format and print an Arl error with source context.
    #' @param e A condition object.
    #' @param file Connection to print to (default \code{stderr()}).
    print_error = function(e, file = stderr()) {
      private$.source_tracker$print_error(e, file = file)
    }
  ),

  private = list(
    .tokenizer = NULL,
    .parser = NULL,
    .macro_expander = NULL,
    .compiled_runtime = NULL,
    .compiler = NULL,
    .help_system = NULL,
    .env = NULL,
    .source_tracker = NULL,
    .module_cache = NULL,

    resolve_env_arg = function(env) {
      resolve_env(env, private$.env$env)
    },

    .initialize_environment = function(load_stdlib = TRUE) {
      env <- private$.env$env

      private$.env$get_registry(".__module_registry", create = TRUE)
      private$.env$get_registry(".__macros", create = TRUE)

      #
      # Cons-cell primitives (bound in top-level env so no globalenv/package lookup)
      #

      env$`.__cons` <- function(car, cdr) Cons$new(car, cdr)
      env$`.__cons-as-list` <- function(x) if (r6_isinstance(x, "Cons")) x$as_list() else list()
      env$`.__cons-parts` <- function(x) if (r6_isinstance(x, "Cons")) x$parts() else list(prefix = list(), tail = x)

      env$`pair?` <- function(x) r6_isinstance(x, "Cons")

      #
      # Macro builtins
      #

      # these depend on internal state and mostly just wrap the macro
      # expander's functionality for users to call

      env$gensym <- function(prefix = "G") {
        private$.macro_expander$gensym(prefix = prefix)
      }

      env$capture <- function(symbol, expr) {
        private$.macro_expander$capture(symbol, expr)
      }

      env$`macro?` <- function(x) {
        is.symbol(x) && private$.macro_expander$is_macro(x, env = env)
      }

      env$macroexpand <- function(expr, depth = NULL, preserve_src = FALSE) {
        private$.macro_expander$macroexpand(expr, env = env,
                                            preserve_src = preserve_src, depth = depth)
      }

      #
      # Evaluation and environments
      #

      # these need to either expose the compile functionality or close over the
      # engine environment

      env$eval <- function(expr, env = parent.frame()) {
        self$eval(expr, env = env)
      }

      env$read <- function(source) {
        exprs <- self$read(source)
        if (length(exprs) > 0L) exprs[[1L]] else NULL
      }

      env$write <- function(expr) {
        private$.parser$write(expr)
      }

      env$`toplevel-env` <- function() env
      env$`current-env` <- function() {
        if (exists(".__env", envir = parent.frame(), inherits = TRUE)) {
          return(get(".__env", envir = parent.frame(), inherits = TRUE))
        }
        private$.env$current_env()
      }

      #
      # Promises: delay / force / promise-expr
      #

      # delay is a compiler special form, but these can be defined here

      env$`promise?` <- function(x) {
        r6_isinstance(x, "Promise")
      }
      env$force <- function(x) {
        if (!r6_isinstance(x, "Promise")) {
          return(x)
        }
        x$value()
      }

      env$`promise-expr` <- function(p) {
        if (!r6_isinstance(p, "Promise")) {
          stop("promise-expr requires a promise (created with delay)")
        }
        p$get_expr()
      }

      #
      # Documentation helpers
      #

      # doc! — attach documentation fields to a function.
      # Uses substitute() to capture the first arg (symbol name) unevaluated.
      # Keyword args set specific fields and merge with existing documentation.
      # Positional string sets the description (backward compatible).
      env$`doc!` <- function(sym, ...) {
        arl_env <- parent.frame()
        name <- as.character(substitute(sym))
        fn <- get(name, envir = arl_env, inherits = TRUE)

        args <- list(...)

        # Primitive wrapping (can't set attributes on primitives)
        if (is.primitive(fn)) {
          prim <- fn
          fn <- function(...) prim(...)
        }

        doc <- attr(fn, "arl_doc", exact = TRUE)
        if (is.null(doc)) doc <- list()

        # Dispatch: positional "string" = description, named = keyword fields
        arg_names <- names(args)
        if (is.null(arg_names) || all(!nzchar(arg_names))) {
          # (doc! fn "docstring") — backward compat
          if (length(args) >= 1L) doc[["description"]] <- args[[1L]]
        } else {
          for (i in seq_along(args)) {
            nm <- arg_names[i]
            if (nzchar(nm)) doc[[nm]] <- args[[i]]
          }
        }

        attr(fn, "arl_doc") <- doc

        # Assign back into the environment where the binding lives
        target <- arl_env
        while (!exists(name, envir = target, inherits = FALSE)) {
          target <- parent.env(target)
        }
        base::assign(name, fn, envir = target)
        invisible(fn)
      }

      # doc — retrieve documentation from a function.
      # With no field argument, returns the description.
      # Pass a field name string to get a specific field, or "all" for the
      # full documentation list.
      env$doc <- function(fn, field = "description") {
        doc_attr <- attr(fn, "arl_doc", exact = TRUE)
        if (is.null(doc_attr)) return(NULL)
        if (identical(field, "all")) return(doc_attr)
        doc_attr[[field]]
      }

      #
      # r/eval
      #

      # Execute an R expression via R's own eval(), bypassing Arl's compiler
      # entirely. This is the escape hatch for R constructs that Arl can't
      # compile.
      #
      # Why it exists: Arl overrides R's while, for, and other control-flow
      # keywords with its own implementations in the top-level environment. But
      # sometimes you need R's actual while/for -- for example, the try/catch
      # macro in control.arl builds tryCatch calls that R needs to evaluate
      # natively, and r-interop.arl uses it for suppressWarnings,
      # suppressMessages, withCallingHandlers, etc. These are R special forms
      # that can't be expressed through Arl's compiler.
      #
      # The complexity, piece by piece:
      #
      # 1. substitute/value dance: Tries to get the unevaluated expression.
      #    Since Arl's compiler may have already evaluated the argument, it
      #    checks whether the result is a call/symbol (use as-is) or something
      #    else (use the substituted form).
      #
      # 2. hygiene_unwrap: Strips Arl's macro hygiene wrappers so R sees clean
      #    R code.
      #
      # 3. quote unwrapping: If you write (r/eval (quote (seq_len 5))), the
      #    compiler passes a quote(seq_len(5)) call. R's eval(quote(x), env)
      #    would just look up x, so this unwraps one layer.
      #
      # 4. R reserved word unshadowing: Arl defines while and for as regular
      #    functions in the environment, shadowing R's keywords. Before calling
      #    R's eval(), we temporarily remove any Arl shadows of R reserved
      #    words so R's native control flow works, then restore them afterward.
      #
      # Where it's used: try/catch/finally, suppressWarnings,
      # suppressMessages, withCallingHandlers, signal, the do looping macro,
      # and anywhere stdlib needs to build and evaluate raw R calls.

      env$`r/eval` <- function(expr, env = NULL) {
        if (is.null(env)) {
          # Use caller's environment directly (no .__env needed)
          env <- parent.frame()
        }
        expr_expr <- substitute(expr)
        expr_value <- expr
        expr <- if (is.call(expr_value) || is.symbol(expr_value)) expr_value else expr_expr
        expr <- private$.macro_expander$hygiene_unwrap(expr)
        # Unwrap (quote x) so we evaluate x in env; R's eval(quote(x), env) would look up x
        if (is.call(expr) && length(expr) == 2L && identical(expr[[1L]], quote(quote))) {
          expr <- expr[[2L]]
        }
        # Temporarily remove any Arl shadows of R reserved words so that
        # R's eval() sees the built-in syntax rather than Arl's overrides.
        r_reserved <- c("if", "else", "repeat", "while", "function",
                        "for", "in", "next", "break", "return")
        saved <- list()
        for (kw in r_reserved) {
          if (exists(kw, envir = env, inherits = FALSE)) {
            saved[[kw]] <- get(kw, envir = env, inherits = FALSE)
            rm(list = kw, envir = env)
          }
        }
        on.exit({
          for (kw in names(saved)) {
            assign(kw, saved[[kw]], envir = env)
          }
        }, add = TRUE)
        # R's eval(symbol, envir) can look up in the wrong env when called from
        # do.call; use get() for symbols so lookup is explicitly in env.
        if (is.symbol(expr)) {
          get(as.character(expr), envir = env, inherits = TRUE)
        } else {
          eval(expr, envir = env)
        }
      }
      # Load arl_doc attributes from builtins-docs.dcf (single source of truth
      # shared with the vignette generator)
      private$load_builtin_docs(env)

      if (load_stdlib) {
        private$.load_stdlib_into_env(env)
      }

      env
    },

    .load_stdlib_into_env = function(env) {
      resolved <- private$resolve_env_arg(env)
      stdlib_dir <- system.file("arl", package = "arl")
      if (!dir.exists(stdlib_dir)) {
        stop("stdlib directory not found")
      }
      cache_path <- system.file("arl", "load-order.txt", package = "arl")
      if (nzchar(cache_path) && file.exists(cache_path)) {
        load_order <- readLines(cache_path, warn = FALSE)
        load_order <- load_order[nzchar(load_order)]
        if (length(load_order) == 0L) {
          load_order <- NULL
        }
      } else {
        load_order <- NULL
      }
      if (is.null(load_order)) {
        deps <- FileDeps$new(dir = stdlib_dir)
        load_order <- deps$get_load_order()
      }
      target_arl <- Env$new(resolved)
      for (name in load_order) {
        if (!target_arl$module_registry$exists(name)) {
          path <- resolve_stdlib_path(name)
          if (is.null(path)) {
            stop("stdlib module not found: ", name)
          }
          tryCatch(
            self$load_file_in_env(path, resolved),
            error = function(e) {
              stop(sprintf("Failed to load stdlib module '%s': %s", name, conditionMessage(e)), call. = FALSE)
            }
          )
        }
        tryCatch(
          target_arl$module_registry$attach_into(name, resolved),
          error = function(e) {
            stop(sprintf("Failed to attach stdlib module '%s': %s", name, conditionMessage(e)), call. = FALSE)
          }
        )
      }
      invisible(NULL)
    },

    load_builtin_docs = function(env) {
      dcf_path <- system.file("builtins-docs.dcf", package = "arl")
      if (!nzchar(dcf_path) || !file.exists(dcf_path)) return(invisible(NULL))
      m <- read_dcf_with_comments(dcf_path)
      doc_fields <- c("Description", "Signature", "Examples", "Seealso", "Note")
      for (i in seq_len(nrow(m))) {
        name <- m[i, "Name"]
        obj <- tryCatch(get(name, envir = env, inherits = FALSE), error = function(e) NULL)
        if (is.null(obj)) next
        doc <- list()
        for (field in doc_fields) {
          val <- m[i, field]
          if (!is.na(val) && nzchar(val)) {
            doc[[tolower(field)]] <- val
          }
        }
        # Boolean metadata flags
        for (flag in c("Internal", "Noeval")) {
          val <- if (flag %in% colnames(m)) m[i, flag] else NA
          if (!is.na(val) && nzchar(val) && tolower(val) %in% c("yes", "true")) {
            doc[[tolower(flag)]] <- TRUE
          }
        }
        if (length(doc) > 0L) {
          attr(obj, "arl_doc") <- doc
          assign(name, obj, envir = env)
        }
      }
      invisible(NULL)
    }
  )
)
