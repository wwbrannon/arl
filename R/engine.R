#' Core Arl engine
#'
#' @description
#' Provides class-based organization for tokenization, parsing, macro expansion,
#' evaluation, and environment management.
#'
#' @importFrom R6 R6Class
#' @param env Optional environment or Env used as the engine base.
#' @param parent Optional parent environment for the new environment.
#' @param source Character string containing Arl source.
#' @param source_name Optional source name for error reporting.
#' @param tokens Token list produced by the tokenizer.
#' @param expr Arl expression (symbol/call/atomic value).
#' @param ... Additional Arl expressions to evaluate (variadic).
#' @param text Character string of Arl code to read/eval.
#' @param path File path to load.
#' @param cache Logical; if TRUE (the default), use the module cache.

#' @param preserve_src Logical; keep source metadata when macroexpanding.
#' @param depth Number of expansion steps (NULL for full expansion).
#' @param topic Help topic as a single string.
#' @param package Optional package name (string or symbol) for package-qualified
#'   R help lookup in \code{$help()}.
#' @param load_prelude Logical; if TRUE (the default), load prelude modules.
#' @param disable_tco Logical; if TRUE, disable self-tail-call optimization.
#' @param value Value to format for display.
#' @param fn A zero-argument function to call with error context.
#' @param file Connection to print to.
#' @param e A condition object.
#' @examples
#' engine <- Engine$new()
#' engine$eval_text("(+ 1 2 3)")
#' engine$eval_string("(+ 4 5)")
#' @export
Engine <- R6::R6Class(
  "ArlEngine",

  cloneable = FALSE,

  public = list(

    #' @description
    #' Initialize engine components and base environment.
    #' @param coverage_tracker Optional CoverageTracker instance to enable coverage tracking
    #'   from the start. If provided, coverage will be tracked during stdlib
    #'   loading. Intended for internal development use.
    #' @param load_prelude Logical. If TRUE (the default), loads prelude modules
    #'   during initialization. Set to FALSE to create a bare engine with only
    #'   builtins — useful for testing or when you want to import specific modules.
    #' @param disable_tco Optional logical. If TRUE, disables self-tail-call optimization
    #'   in the compiler, preserving natural call stacks for debugging. Defaults to NULL,
    #'   which inherits from global option `getOption("arl.disable_tco", FALSE)`.
    #' @param r_packages Controls which R packages are visible to Arl code.
    #'   `"search_path"` (default) tracks R's `search()` dynamically;
    #'   a character vector pins a fixed set; `NULL` exposes only `baseenv()`.
    initialize = function(coverage_tracker = NULL, load_prelude = TRUE,
                          disable_tco = NULL, r_packages = "search_path") {
      private$.env <- Env$new()
      private$.source_tracker <- SourceTracker$new()
      private$.tokenizer <- Tokenizer$new()
      private$.parser <- Parser$new(private$.source_tracker)
      private$.module_cache <- ModuleCache$new()

      # Create shared evaluation context
      context <- EvalContext$new(private$.env, private$.source_tracker, coverage_tracker = coverage_tracker)

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
      # Disable self-TCO when requested (useful for debugging).
      # Priority: explicit parameter > R option > env var > default FALSE.
      if (is.null(disable_tco)) {
        disable_tco <- .pkg_option("disable_tco", FALSE)
      }
      if (isTRUE(disable_tco)) {
        private$.compiler$enable_tco <- FALSE
      }
      context$compiler <- private$.compiler

      private$.compiled_runtime <- CompiledRuntime$new(
        context,
        load_file_fn = function(path, env, cache = TRUE) self$load_file_in_env(path, env, cache = cache),
        help_fn = function(topic, env, package = NULL) self$help(topic, env, package),
        module_cache = private$.module_cache
      )
      context$compiled_runtime <- private$.compiled_runtime
      private$.help_system <- HelpSystem$new(private$.env, private$.macro_expander)

      private$.r_packages_mode <- r_packages

      private$.initialize_environment(load_prelude = isTRUE(load_prelude))
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
      private$.refresh_pkg_chain_if_needed()
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
    #' Alias for \code{eval_text()}.
    eval_string = function(text, env = NULL, source_name = "<eval>") {
      self$eval_text(text, env = env, source_name = source_name)
    },

    #' @description
    #' Load and evaluate an Arl source file in the given environment. Definitions
    #' and imports in the file are visible in \code{env}. To evaluate in an
    #' isolated child scope, create one explicitly:
    #' \code{load_file_in_env(path, new.env(parent = env))}.
    load_file_in_env = function(path, env = NULL, cache = TRUE) {
      if (!is.character(path) || length(path) != 1) {
        stop("load requires a single file path string")
      }
      resolved <- private$resolve_env_arg(env)
      if (!file.exists(path)) {
        stop(sprintf("File not found: %s", path))
      }

      # Set current_source_file for relative import resolution
      ctx <- private$.compiled_runtime$context
      old_source_file <- ctx$current_source_file
      ctx$current_source_file <- normalizePath(path, mustWork = TRUE)
      on.exit(ctx$current_source_file <- old_source_file, add = TRUE)

      # Try cache loading for module files
      # Skip cache when coverage is enabled — cached expressions lack source info for instrumentation
      coverage_active <- !is.null(ctx$coverage_tracker) && ctx$coverage_tracker$enabled
      if (isTRUE(cache) && !coverage_active) {
        module_registry <- private$.env$module_registry
        cache_paths <- private$.module_cache$get_paths(path)
        if (!is.null(cache_paths)) {
          target_env <- resolved

          # Env cache disabled: proxy-based imports use active bindings
          # in the parent chain which can't survive serialization/deserialization.
          # Code cache continues to work since it re-evaluates .__import() calls.

          # Expr cache (compiled expressions)
          if (file.exists(cache_paths$code_cache)) {
            cache_data <- private$.module_cache$load_code(cache_paths$code_cache, path, file_hash = cache_paths$file_hash)
            if (!is.null(cache_data)) {
              # Recreate module environment (like module_compiled does)
              module_name <- cache_data$module_name
              exports <- cache_data$exports
              export_all <- cache_data$export_all
              re_export <- isTRUE(cache_data$re_export)

              # Module environments inherit from prelude_env (not the
              # engine env with all stdlib) for proper lexical scoping.
              prelude_env <- private$.compiled_runtime$context$prelude_env
              builtins_env <- private$.compiled_runtime$context$builtins_env
              module_parent <- if (!is.null(prelude_env)) prelude_env
                               else if (!is.null(builtins_env)) builtins_env
                               else target_env
              module_env <- new.env(parent = module_parent)
              assign(".__module", TRUE, envir = module_env)
              lockBinding(".__module", module_env)

              module_registry$register(module_name, module_env, exports)

              # Register by absolute path
              absolute_path <- normalize_path_absolute(path)
              module_registry$alias(absolute_path, module_name)

              # Install helpers and setup
              private$.compiled_runtime$install_helpers(module_env)

              # Inflate resolved ref placeholders back to live values
              reg_env <- module_registry$arl_env$module_registry_env(create = FALSE)
              if (!is.null(reg_env)) {
                cache_data$compiled_body <- lapply(cache_data$compiled_body, inflate_resolved_refs, reg_env)
              }

              # Evaluate cached compiled expressions in module environment
              if (length(cache_data$compiled_body) == 1L) {
                result <- private$.compiled_runtime$eval_compiled(cache_data$compiled_body[[1L]], module_env)
              } else {
                block <- as.call(c(list(quote(`{`)), cache_data$compiled_body))
                result <- private$.compiled_runtime$eval_compiled(block, module_env)
              }

              finalize_module_env(module_env, module_name, exports, export_all,
                                 re_export, module_registry)

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
    #' @param topic Help topic as a single string.
    #' @param env Optional environment/Env to resolve Arl bindings against.
    #' @param package Optional package name (string or symbol) to force R help
    #'   lookup in a specific package.
    help = function(topic, env = NULL, package = NULL) {
      target_env <- private$resolve_env_arg(env)
      private$.help_system$help_in_env(topic, target_env, package = package)
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
    #' Define a binding in the engine's top-level environment. This is the
    #' supported way to inject R objects for use in Arl code.
    #' @param name Character string; the binding name.
    #' @param value The value to bind.
    #' @return Invisible self (for chaining).
    define = function(name, value) {
      if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
        stop("define() requires a single non-empty character string as name")
      }
      assign(name, value, envir = private$.env$env)
      invisible(self)
    },

    #' @description
    #' Format a value for display using the engine's formatter.
    #' @param value Value to format.
    #' @return Character string.
    format_value = function(value) {
      # Lazily load display module if format-value isn't available yet
      env <- private$.env$env
      if (!is.function(get0("format-value", envir = env, inherits = TRUE))) {
        tryCatch(
          self$eval_text("(import display :refer :all)", env = env),
          error = function(e) NULL
        )
      }
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
    .r_packages_mode = NULL,
    .r_pkg_names = NULL,

    resolve_env_arg = function(env) {
      resolve_env(env, private$.env$env)
    },

    .build_pkgs_chain = function(pkg_names) {
      if (is.null(pkg_names) || length(pkg_names) == 0L) return(baseenv())

      # Build bottom-up: last package is closest to baseenv().
      parent <- baseenv()
      for (pkg in rev(pkg_names)) {
        ns <- tryCatch(asNamespace(pkg), error = function(e) NULL)
        if (is.null(ns)) next
        pkg_env <- new.env(parent = parent)
        exports <- getNamespaceExports(ns)
        # Also include lazy data (e.g. datasets package uses this mechanism)
        nsinfo <- get(".__NAMESPACE__.", envir = ns)
        if (exists("lazydata", envir = nsinfo)) {
          ld <- get("lazydata", envir = nsinfo)
          exports <- unique(c(exports, ls(ld)))
        }
        for (name in exports) {
          tryCatch(
            assign(name, getExportedValue(pkg, name), envir = pkg_env),
            error = function(e) NULL
          )
        }
        attr(pkg_env, "name") <- paste0("package:", pkg)
        parent <- pkg_env
      }
      parent
    },

    .resolve_r_packages = function() {
      mode <- private$.r_packages_mode
      if (is.null(mode)) return(character(0))
      if (identical(mode, "search_path")) {
        raw <- search()
        raw <- raw[startsWith(raw, "package:")]
        raw <- sub("^package:", "", raw)
        return(setdiff(raw, "base"))
      }
      mode  # character vector
    },

    .refresh_pkg_chain_if_needed = function() {
      if (!identical(private$.r_packages_mode, "search_path")) return(invisible(NULL))
      current <- private$.resolve_r_packages()
      if (identical(current, private$.r_pkg_names)) return(invisible(NULL))
      private$.r_pkg_names <- current
      builtins_env <- private$.compiled_runtime$context$builtins_env
      parent.env(builtins_env) <- private$.build_pkgs_chain(private$.r_pkg_names)
    },

    .initialize_environment = function(load_prelude = TRUE) {
      env <- private$.env$env

      # Create builtins environment: modules inherit from this (not from
      # engine_env which has all stdlib attached).
      # Chain: engine_env -> builtins_env -> pkg:stats -> ... -> pkg:methods -> baseenv()
      # Always build on baseenv(), even if the user passed an explicit env
      # with a different parent.
      private$.r_pkg_names <- private$.resolve_r_packages()
      builtins_env <- new.env(parent = private$.build_pkgs_chain(private$.r_pkg_names))
      prelude_env <- new.env(parent = builtins_env)
      parent.env(env) <- prelude_env

      # Move registries from engine_env to builtins_env so module envs
      # (which parent to builtins_env) can find them via inherits = TRUE.
      macro_reg <- private$.env$get_registry(".__macros", create = TRUE)
      module_reg <- private$.env$get_registry(".__module_registry", create = TRUE)
      if (exists(".__macros", envir = env, inherits = FALSE)) {
        rm(".__macros", envir = env)
      }
      if (exists(".__module_registry", envir = env, inherits = FALSE)) {
        rm(".__module_registry", envir = env)
      }
      assign(".__macros", macro_reg, envir = builtins_env)
      assign(".__module_registry", module_reg, envir = builtins_env)

      # Store builtins_env and prelude_env on the eval context so runtime
      # can use them as parents for module environments.
      private$.compiled_runtime$context$builtins_env <- builtins_env
      private$.compiled_runtime$context$prelude_env <- prelude_env

      #
      # Equality operators — R's = is assignment, not comparison. Arl's
      # = and == are NULL-safe variadic comparison. These must be builtins
      # because every module needs them and R's baseenv semantics conflict.
      #

      null_safe_eq <- compiler::cmpfun(function(a, b) {
        if (is.null(a)) return(is.null(b))
        if (is.null(b)) return(FALSE)
        a == b
      })
      variadic_eq <- compiler::cmpfun(function(...) {
        args <- list(...)
        n <- length(args)
        if (n < 2L) return(TRUE)
        for (i in seq_len(n - 1L)) {
          if (!isTRUE(null_safe_eq(args[[i]], args[[i + 1L]]))) return(FALSE)
        }
        TRUE
      })
      builtins_env$`=` <- variadic_eq
      builtins_env$`==` <- variadic_eq
      builtins_env$`!=` <- compiler::cmpfun(function(a, b) {
        if (is.null(a)) return(!is.null(b))
        if (is.null(b)) return(TRUE)
        a != b
      })

      #
      # Variadic arithmetic operators — the compiler folds these at compile time
      # for statically-known call sites, but runtime variadic versions are needed
      # for funcall/apply and higher-order usage.
      #

      builtins_env$`+` <- compiler::cmpfun(function(...) {
        args <- list(...)
        n <- length(args)
        if (n == 0L) return(0)
        if (n == 1L) return(args[[1L]])
        Reduce(`+`, args)
      })
      builtins_env$`*` <- compiler::cmpfun(function(...) {
        args <- list(...)
        n <- length(args)
        if (n == 0L) return(1)
        if (n == 1L) return(args[[1L]])
        Reduce(`*`, args)
      })
      builtins_env$`-` <- compiler::cmpfun(function(...) {
        args <- list(...)
        n <- length(args)
        if (n == 0L) stop("- requires at least one argument")
        if (n == 1L) return(-args[[1L]])
        Reduce(`-`, args)
      })
      builtins_env$`/` <- compiler::cmpfun(function(...) {
        args <- list(...)
        n <- length(args)
        if (n == 0L) stop("/ requires at least one argument")
        if (n == 1L) return(1 / args[[1L]])
        Reduce(`/`, args)
      })

      #
      # Variadic comparison operators — chained pairwise comparison.
      #

      make_variadic_cmp <- function(op) {
        force(op)
        compiler::cmpfun(function(...) {
          args <- list(...)
          n <- length(args)
          if (n < 2L) return(TRUE)
          # 2-arg case: return base R's element-wise result directly
          # (needed for NSE contexts like subset where vectors are expected)
          if (n == 2L) return(op(args[[1L]], args[[2L]]))
          # 3+ args: chained pairwise scalar comparison
          for (i in seq_len(n - 1L)) {
            if (!isTRUE(op(args[[i]], args[[i + 1L]]))) return(FALSE)
          }
          TRUE
        })
      }
      builtins_env$`<`  <- make_variadic_cmp(`<`)
      builtins_env$`<=` <- make_variadic_cmp(`<=`)
      builtins_env$`>`  <- make_variadic_cmp(`>`)
      builtins_env$`>=` <- make_variadic_cmp(`>=`)

      #
      # Logical negation — Arl truthiness (0, #f, #nil are falsy).
      # R's ! works on logical/numeric but not on arbitrary values.
      #

      builtins_env$`not` <- compiler::cmpfun(function(x) {
        if (.__true_p(x)) FALSE else TRUE
      })

      #
      # Cons-cell primitives — installed into builtins_env
      #

      builtins_env$`.__cons` <- function(car, cdr) Cons$new(car, cdr)
      builtins_env$`.__cons-as-list` <- function(x) if (inherits(x, "ArlCons")) x$as_list() else list()
      builtins_env$`.__cons-parts` <- function(x) if (inherits(x, "ArlCons")) x$parts() else list(prefix = list(), tail = x)

      builtins_env$`pair?` <- function(x) inherits(x, "ArlCons")

      #
      # Macro builtins
      #

      # these depend on internal state and mostly just wrap the macro
      # expander's functionality for users to call

      builtins_env$gensym <- function(prefix = "G") {
        private$.macro_expander$gensym(prefix = prefix)
      }

      builtins_env$capture <- function(symbol, expr) {
        private$.macro_expander$capture(symbol, expr)
      }

      builtins_env$`macro?` <- function(x, .env = NULL) {
        # Use provided env, or current Arl eval env (from env stack), fallback to engine env
        lookup_env <- if (!is.null(.env)) .env
          else { e <- private$.env$current_env(); if (identical(e, globalenv())) env else e }
        is.symbol(x) && private$.macro_expander$is_macro(x, env = lookup_env)
      }

      builtins_env$macroexpand <- function(expr, depth = NULL, preserve_src = FALSE, .env = NULL) {
        lookup_env <- if (!is.null(.env)) .env
          else { e <- private$.env$current_env(); if (identical(e, globalenv())) env else e }
        private$.macro_expander$macroexpand(expr, env = lookup_env,
                                            preserve_src = preserve_src, depth = depth)
      }

      #
      # Evaluation and environments
      #

      # these need to either expose the compile functionality or close over the
      # engine environment

      builtins_env$eval <- function(expr, env = parent.frame()) {
        self$eval(expr, env = env)
      }

      builtins_env$read <- function(source) {
        exprs <- self$read(source)
        if (length(exprs) > 0L) exprs[[1L]] else NULL
      }

      builtins_env$write <- function(expr) {
        private$.parser$write(expr)
      }

      builtins_env$help <- function(topic, package = NULL) {
        target_env <- if (exists(".__env", envir = parent.frame(), inherits = TRUE)) {
          get(".__env", envir = parent.frame(), inherits = TRUE)
        } else {
          private$.env$current_env()
        }

        topic_expr <- substitute(topic)
        if (is.symbol(topic_expr)) {
          topic <- as.character(topic_expr)
        } else {
          topic <- topic_expr
        }

        if (missing(package)) {
          package <- NULL
        } else {
          package_expr <- substitute(package)
          if (is.symbol(package_expr)) {
            package <- as.character(package_expr)
          } else {
            package <- package_expr
          }
        }

        self$help(topic, env = target_env, package = package)
      }

      load_fn <- function(path, env = NULL) {
        target_env <- if (is.null(env)) {
          if (exists(".__env", envir = parent.frame(), inherits = TRUE)) {
            get(".__env", envir = parent.frame(), inherits = TRUE)
          } else {
            private$.env$current_env()
          }
        } else {
          private$resolve_env_arg(env)
        }
        self$load_file_in_env(path, target_env, cache = FALSE)
      }
      builtins_env$load <- load_fn

      builtins_env$`toplevel-env` <- function() env
      builtins_env$`builtins-env` <- function() builtins_env
      builtins_env$`prelude-env` <- function() prelude_env
      builtins_env$`current-env` <- function() {
        if (exists(".__env", envir = parent.frame(), inherits = TRUE)) {
          return(get(".__env", envir = parent.frame(), inherits = TRUE))
        }
        private$.env$current_env()
      }

      #
      # Promises: delay / force / promise-expr
      #

      # delay is a compiler special form, but these can be defined here

      builtins_env$`promise?` <- function(x) {
        inherits(x, "ArlPromise")
      }
      builtins_env$force <- function(x) {
        if (!inherits(x, "ArlPromise")) {
          return(x)
        }
        x$value()
      }

      builtins_env$`promise-expr` <- function(p) {
        if (!inherits(p, "ArlPromise")) {
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
      builtins_env$`doc!` <- function(sym, ...) {
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

        # Assign back into the environment where the binding lives.
        # Stop at builtins_env — never walk into base R package environments.
        boundary <- parent.env(builtins_env)
        target <- arl_env
        while (!identical(target, boundary) &&
               !exists(name, envir = target, inherits = FALSE)) {
          target <- parent.env(target)
        }
        if (identical(target, boundary)) {
          stop(sprintf("doc!: '%s' is not defined in an Arl environment", name), call. = FALSE)
        }
        # Handle active bindings (from proxy imports)
        if (bindingIsActive(name, target)) {
          if (bindingIsLocked(name, target)) unlock_binding(name, target)
          rm(list = name, envir = target)
        }
        base::assign(name, fn, envir = target)
        invisible(fn)
      }

      # doc — retrieve documentation from a function.
      # With no field argument, returns the description.
      # Pass a field name string to get a specific field, or "all" for the
      # full documentation list.
      builtins_env$doc <- function(fn, field = "description") {
        doc_attr <- attr(fn, "arl_doc", exact = TRUE)
        if (is.null(doc_attr)) return(NULL)
        if (identical(field, "all")) return(doc_attr)
        doc_attr[[field]]
      }

      #
      # r-eval
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
      # 3. quote unwrapping: If you write (r-eval (quote (seq_len 5))), the
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

      builtins_env$`r-eval` <- function(expr, env = NULL) {
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
      #
      # module-ref — qualified access: (module-ref mod sym)
      #
      module_registry <- private$.env$module_registry
      builtins_env$`module-ref` <- function(obj, name) {
        name_str <- as.character(substitute(name))
        if (is.environment(obj) && isTRUE(get0(".__module", envir = obj, inherits = FALSE))) {
          # Module env: look up exported binding
          if (!exists(name_str, envir = obj, inherits = FALSE)) {
            stop(sprintf("module has no export '%s'", name_str), call. = FALSE)
          }
          return(get(name_str, envir = obj, inherits = FALSE))
        }
        if (inherits(obj, "arl_namespace")) {
          # Namespace node: extend prefix and look up in module registry
          prefix <- get(".__namespace_prefix", envir = obj, inherits = FALSE)
          full_path <- paste0(prefix, "/", name_str)
          if (module_registry$exists(full_path)) {
            return(module_registry$get(full_path)$env)
          }
          # Check if it's a valid sub-namespace prefix
          if (module_registry$has_prefix(full_path)) {
            return(make_namespace_node(full_path))
          }
          stop(sprintf("no module or namespace '%s'", full_path), call. = FALSE)
        }
        stop(sprintf("module-ref: first argument must be a module or namespace, got %s",
                      paste(class(obj), collapse = "/")), call. = FALSE)
      }

      #
      # module? — is x a module environment?
      #
      builtins_env$`module?` <- function(x) {
        is.environment(x) && isTRUE(get0(".__module", envir = x, inherits = FALSE))
      }

      #
      # namespace? — is x a namespace node?
      #
      builtins_env$`namespace?` <- function(x) {
        inherits(x, "arl_namespace")
      }

      #
      # module-exports — get export list from a module env
      #
      builtins_env$`module-exports` <- function(mod) {
        if (!is.environment(mod) || !isTRUE(get0(".__module", envir = mod, inherits = FALSE))) {
          stop("module-exports: argument must be a module", call. = FALSE)
        }
        registry_env <- module_registry$arl_env$module_registry_env(create = FALSE)
        if (!is.null(registry_env)) {
          for (key in ls(registry_env, all.names = TRUE)) {
            entry <- base::get(key, envir = registry_env, inherits = FALSE)
            if (identical(entry$env, mod)) {
              return(as.list(entry$exports))
            }
          }
        }
        stop("module-exports: module not found in registry", call. = FALSE)
      }

      #
      # module-name — get canonical name of a module env
      #
      builtins_env$`module-name` <- function(mod) {
        if (!is.environment(mod) || !isTRUE(get0(".__module", envir = mod, inherits = FALSE))) {
          stop("module-name: argument must be a module", call. = FALSE)
        }
        registry_env <- module_registry$arl_env$module_registry_env(create = FALSE)
        if (!is.null(registry_env)) {
          # Collect all keys matching this module, return shortest (canonical name)
          matching <- character(0)
          for (key in ls(registry_env, all.names = TRUE)) {
            entry <- base::get(key, envir = registry_env, inherits = FALSE)
            if (identical(entry$env, mod)) {
              matching <- c(matching, key)
            }
          }
          if (length(matching) > 0) {
            return(matching[which.min(nchar(matching))])
          }
        }
        stop("module-name: module not found in registry", call. = FALSE)
      }

      # Load arl_doc attributes from reference-docs.dcf (single source of truth
      # shared with the vignette generator)
      private$load_builtin_docs(builtins_env)

      if (load_prelude) {
        # Load prelude modules into prelude_env with squash mode: active
        # bindings are created directly in prelude_env instead of inserting
        # proxy environments into the parent chain.
        prelude_path <- system.file("arl", "prelude-modules.txt", package = "arl")
        if (nzchar(prelude_path) && file.exists(prelude_path)) {
          prelude_modules <- readLines(prelude_path, warn = FALSE)
          prelude_modules <- prelude_modules[nzchar(prelude_modules)]
        } else {
          stop("prelude-modules.txt not found")
        }
        private$.compiled_runtime$context$squash_imports <- TRUE
        on.exit(private$.compiled_runtime$context$squash_imports <- FALSE, add = TRUE)
        private$.load_modules(prelude_modules, prelude_env)
        private$.compiled_runtime$context$squash_imports <- FALSE
      }

      env
    },

    # Load and attach modules into a target environment using (import ...).
    # module_names should be in topological order for efficiency (avoids
    # redundant registry lookups), but correctness doesn't depend on it
    # since the import system resolves dependencies transitively.
    .load_modules = function(module_names, env) {
      for (name in module_names) {
        tryCatch(
          self$eval_text(sprintf("(import %s :refer :all)", name), env = env),
          error = function(e) {
            stop(sprintf("Failed to load module '%s': %s", name, conditionMessage(e)), call. = FALSE)
          }
        )
      }
      invisible(NULL)
    },

    load_builtin_docs = function(env) {
      dcf_path <- system.file("reference-docs.dcf", package = "arl")
      if (!nzchar(dcf_path) || !file.exists(dcf_path)) return(invisible(NULL))
      m <- read_dcf_with_comments(dcf_path)
      doc_fields <- c("Description", "Signature", "Examples", "Seealso", "Note")
      for (i in seq_len(nrow(m))) {
        kind <- if ("Kind" %in% colnames(m)) m[i, "Kind"] else NA
        if (!is.na(kind) && nzchar(kind) &&
            tolower(trimws(kind)) == "special-form") {
          next
        }
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
