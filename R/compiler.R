# Compiler: Compiles macro-expanded Arl AST to R expressions for single eval().
#
# Reserved name: .__env is used for the current environment in compiled code.
# (define .__env x) and (set! .__env x) are not compiled (return NULL).

# Module-level hashed environment for O(1) pure-function lookup in constant folding
.PURE_FUNCTIONS <- (function() {
  e <- new.env(hash = TRUE, parent = emptyenv())
  for (fn in c(
    # Arithmetic
    "+", "-", "*", "/", "^", "%%", "%/%",
    # Comparison
    "<", ">", "<=", ">=", "==", "!=",
    # Logical (element-wise)
    "&", "|", "!",
    # Math functions
    "abs", "sqrt", "exp", "log", "log10", "log2",
    "sin", "cos", "tan", "asin", "acos", "atan",
    "floor", "ceiling", "round", "trunc",
    "sign", "signif",
    # Other pure functions
    "length", "nchar", "toupper", "tolower"
  )) {
    e[[fn]] <- TRUE
  }
  e
})()

#' @keywords internal
#' @noRd
Compiler <- R6::R6Class(
  "ArlCompiler",
  public = list(
    context = NULL,
    # Reserved name for current env in compiled code. User code that defines/sets this is not compiled.
    env_var_name = ".__env",
    # Env used as parent for compiled closures (so they see .__true_p etc.). Set by engine before compile.
    current_env = NULL,
    # When TRUE, invalid forms raise errors instead of returning NULL.
    strict = FALSE,
    # When TRUE, compile quasiquote via macro-expander helper (for macro evaluation).
    macro_eval = FALSE,
    # When FALSE, disable constant folding optimization (for preserving expression structure).
    enable_constant_folding = TRUE,
    # When FALSE, disable self-tail-call optimization (useful for debugging).
    enable_tco = TRUE,
    # When FALSE, disable dead code elimination in if/tail-if (constant-test early returns).
    enable_dead_code_elim = TRUE,
    # When FALSE, disable strength reduction (e.g., (* x 2) → (+ x x)).
    enable_strength_reduction = TRUE,
    # When FALSE, disable identity lambda elimination (e.g., ((lambda (x) x) v) → v).
    enable_identity_elim = TRUE,
    # When FALSE, always use .__true_p wrapper in if tests (skip returns_r_logical shortcut).
    enable_truthiness_opt = TRUE,
    # When FALSE, keep block wrapper for single-expression begin.
    enable_begin_simplify = TRUE,
    # When FALSE, skip flattening of nested and/or expressions.
    enable_boolean_flatten = TRUE,
    # Nesting depth: 0 means top-level. Incremented inside lambda, if, while, etc.
    nesting_depth = 0L,
    # Last compilation error (message) when compile() returns NULL.
    last_error = NULL,
    # Annotation map: name -> annotation data (set by module_compiled before compiling).
    annotations = NULL,
    # Raw source text for annotation parsing when no file is available (set by eval_text).
    source_text = NULL,
    # Monotonic counter for generating unique symbols when macro expander is unavailable.
    gensym_counter = 0L,
    # Generate a unique symbol. Delegates to the macro expander's gensym when
    # available (for globally unique names); falls back to an instance counter.
    gensym = function(prefix) {
      if (!is.null(self$context) && !is.null(self$context$macro_expander)) {
        return(self$context$macro_expander$gensym(prefix))
      }
      self$gensym_counter <- self$gensym_counter + 1L
      as.symbol(paste0(prefix, self$gensym_counter))
    },
    # @param context EvalContext (for source_tracker).
    initialize = function(context) {
      if (!inherits(context, "ArlEvalContext")) {
        stop("Compiler requires an EvalContext")
      }
      self$context <- context
    },
    # @description Get compiler optimization flags as a named logical vector.
    # @return Named logical vector of compiler flags.
    get_flags = function() {
      c(
        enable_tco = self$enable_tco,
        enable_constant_folding = self$enable_constant_folding,
        enable_dead_code_elim = self$enable_dead_code_elim,
        enable_strength_reduction = self$enable_strength_reduction,
        enable_identity_elim = self$enable_identity_elim,
        enable_truthiness_opt = self$enable_truthiness_opt,
        enable_begin_simplify = self$enable_begin_simplify,
        enable_boolean_flatten = self$enable_boolean_flatten
      )
    },
    # @description Compile one expression. Returns R expression or NULL (cannot compile).
    # @param expr Macro-expanded Arl expression.
    # @return R expression (language object) or NULL.
    compile = function(expr, env = NULL, strict = NULL) {
      if (!is.null(env)) {
        self$current_env <- env
      }
      if (!is.null(strict)) {
        self$strict <- isTRUE(strict)
      }
      self$last_error <- NULL
      if (isTRUE(self$strict)) {
        return(private$compile_impl(expr))
      }
      tryCatch(private$compile_impl(expr), error = function(e) {
        if (is.null(self$last_error)) {
          self$last_error <- conditionMessage(e)
        }
        NULL
      })
    },
    # @description Compile a sequence of expressions to a single R expression (block).
    # @param exprs List of Arl expressions.
    # @return R expression or NULL.
    compile_seq = function(exprs, env = NULL, strict = NULL) {
      if (length(exprs) == 0) {
        return(private$fail("empty expression list"))
      }
      if (!is.null(env)) {
        self$current_env <- env
      }
      if (!is.null(strict)) {
        self$strict <- isTRUE(strict)
      }
      self$last_error <- NULL
      if (length(exprs) == 1) {
        return(self$compile(exprs[[1]], env, strict = self$strict))
      }
      compiled <- vector("list", length(exprs))
      for (i in seq_along(exprs)) {
        compiled[[i]] <- private$compile_impl(exprs[[i]])
        if (is.null(compiled[[i]])) {
          return(private$fail("unable to compile expression sequence"))
        }
      }
      private$src_inherit(as.call(c(list(quote(`{`)), compiled)), exprs[[1]])
    }
  ),
  private = list(
    # Parse a list of symbols from an import modifier value.
    # Returns a character vector of symbol names, or "ERROR:..." on failure.
    parse_symbol_list = function(val, modifier_name) {
      # val is a parsed list form like (sym1 sym2 ...) — an R call object
      if (is.call(val)) {
        syms <- as.list(val)
        result <- character(length(syms))
        for (j in seq_along(syms)) {
          if (!is.symbol(syms[[j]])) {
            return(paste0("ERROR:import: ", modifier_name, " elements must be symbols"))
          }
          result[j] <- as.character(syms[[j]])
        }
        return(result)
      }
      # Empty list case: NULL from parser or list() from parser for ()
      if (is.null(val) || (is.list(val) && length(val) == 0L)) {
        return(character(0))
      }
      paste0("ERROR:import: ", modifier_name, " requires a list of symbols")
    },
    # Parse a rename list: ((old new) ...) → named character vector (names=old, values=new)
    parse_rename_list = function(val) {
      if (!is.call(val)) {
        return("ERROR:import: :rename requires a list of (old new) pairs")
      }
      pairs <- as.list(val)
      old_names <- character(length(pairs))
      new_names <- character(length(pairs))
      for (j in seq_along(pairs)) {
        pair <- pairs[[j]]
        if (!is.call(pair) || length(pair) != 2) {
          return("ERROR:import: :rename entries must be (old-name new-name) pairs")
        }
        if (!is.symbol(pair[[1]]) || !is.symbol(pair[[2]])) {
          return("ERROR:import: :rename entries must contain symbols")
        }
        old_names[j] <- as.character(pair[[1]])
        new_names[j] <- as.character(pair[[2]])
      }
      names(new_names) <- old_names
      new_names
    },
    # Shared implementation for compile_and / compile_or.
    # identity: value when no args (TRUE for and, FALSE for or)
    # op_sym: quoted symbol to flatten (quote(and) or quote(or))
    # on_true_continue: if TRUE, truthy branch continues to next arg (and);
    #   if FALSE, truthy branch short-circuits with current value (or)
    compile_short_circuit = function(expr, identity, op_sym, on_true_continue) {
      if (length(expr) == 1) return(identity)

      op_name <- as.character(op_sym)

      # Flatten nested ops: (and (and a b) c) → (and a b c)
      flatten <- function(e) {
        result <- list()
        for (i in if (length(e) >= 2) 2:length(e) else integer(0)) {
          arg <- e[[i]]
          if (is.call(arg) && length(arg) >= 1 && identical(arg[[1]], op_sym)) {
            result <- c(result, flatten(arg))
          } else {
            result[[length(result) + 1L]] <- arg
          }
        }
        result
      }
      flat_args <- if (isTRUE(self$enable_boolean_flatten)) {
        flatten(expr)
      } else {
        # Without flattening, just extract direct arguments
        args_list <- list()
        for (i in if (length(expr) >= 2) 2:length(expr) else integer(0)) {
          args_list[[length(args_list) + 1L]] <- expr[[i]]
        }
        args_list
      }

      # Compile all flattened arguments
      args <- vector("list", length(flat_args))
      for (i in seq_along(flat_args)) {
        compiled <- private$compile_impl(flat_args[[i]])
        if (is.null(compiled)) {
          return(private$fail(paste(op_name, "argument could not be compiled")))
        }
        args[[i]] <- compiled
      }

      gensym_tmp <- function() self$gensym(".__tmp")

      build <- function(idx) {
        if (idx == length(args)) return(args[[idx]])

        arg <- args[[idx]]
        continue_expr <- build(idx + 1L)

        make_if <- function(test_val, return_val) {
          if (on_true_continue) {
            as.call(list(quote(`if`),
              as.call(list(as.symbol(".__true_p"), test_val)),
              continue_expr, return_val))
          } else {
            as.call(list(quote(`if`),
              as.call(list(as.symbol(".__true_p"), test_val)),
              return_val, continue_expr))
          }
        }

        # Simple values don't need temps - use directly
        if (private$is_simple_value(arg)) return(make_if(arg, arg))

        # Complex expressions need temps to avoid double evaluation
        tmp_sym <- gensym_tmp()
        as.call(c(list(quote(`{`)),
          list(as.call(list(quote(`<-`), tmp_sym, arg))),
          list(make_if(tmp_sym, tmp_sym))
        ))
      }
      build(1L)
    },
    compiled_nil = function() {
      as.call(list(quote(quote), NULL))
    },
    fail = function(msg) {
      self$last_error <- msg
      if (isTRUE(self$strict)) {
        stop(msg, call. = FALSE)
      }
      NULL
    },
    src_get = function(expr) {
      if (is.null(self$context) || is.null(self$context$source_tracker)) {
        return(NULL)
      }
      self$context$source_tracker$src_get(expr)
    },
    src_set = function(expr, src) {
      if (is.null(self$context) || is.null(self$context$source_tracker) || is.null(src)) {
        return(expr)
      }
      if (is.symbol(expr)) {
        return(expr)
      }
      self$context$source_tracker$src_set(expr, src)
    },
    src_inherit = function(expr, from) {
      src <- private$src_get(from)
      if (is.null(src)) {
        return(expr)
      }
      private$src_set(expr, src)
    },
    strip_src = function(expr) {
      if (is.null(self$context) || is.null(self$context$source_tracker)) {
        return(expr)
      }
      self$context$source_tracker$strip_src(expr)
    },
    compile_impl = function(expr) {
      if (is.null(expr)) {
        return(private$compiled_nil())
      }
      # Resolved cross-module reference: extract the captured value
      if (is_resolved_ref(expr)) {
        val <- expr$value
        # Tag functions so cache writer can deflate them
        if (is.function(val)) {
          attr(val, "arl_resolved_from") <- list(
            module_name = expr$module_name,
            source_symbol = expr$source_symbol
          )
        }
        return(val)
      }
      # Atoms (self-evaluating)
      if (!is.call(expr) && !is.symbol(expr)) {
        return(private$strip_src(expr))
      }
      if (inherits(expr, "arl_keyword")) {
        return(expr)
      }
      # Symbol: .__nil is the parser sentinel for #nil; compile to NULL
      if (is.symbol(expr)) {
        if (identical(expr, as.symbol(".__nil"))) {
          return(private$compiled_nil())
        }
        return(expr)
      }
      # Empty list
      if (is.call(expr) && length(expr) == 0) {
        return(expr)
      }
      op <- expr[[1]]
      op_name <- if (is.symbol(op)) as.character(op) else NULL
      # Special forms
      if (!is.null(op_name)) {
        out <- switch(op_name,
          quote = private$compile_quote(expr),
          quasiquote = if (isTRUE(self$macro_eval))
            private$compile_macro_quasiquote(expr) else private$compile_quasiquote(expr),
          `if` = private$compile_if(expr),
          begin = private$compile_begin(expr),
          define = private$compile_define(expr),
          `set!` = private$compile_set(expr),
          lambda = private$compile_lambda(expr),
          import = private$compile_import(expr),
          `import-runtime` = private$compile_import_runtime(expr),
          `while` = private$compile_while(expr),
          and = private$compile_and(expr),
          or = private$compile_or(expr),
          delay = private$compile_delay(expr),
          defmacro = private$compile_defmacro(expr),
          module = private$compile_module(expr),
          private$compile_application(expr)
        )
        if (!is.null(out)) {
          return(private$src_inherit(out, expr))
        }
        return(out)
      }
      # Application (unknown call)
      private$src_inherit(private$compile_application(expr), expr)
    },
    compile_quote = function(expr) {
      if (length(expr) != 2) {
        return(private$fail("quote requires exactly 1 argument"))
      }
      # Return R (quote x) so that eval(compiled, env) yields the quoted value, not a lookup of x.
      as.call(list(quote(quote), private$strip_src(expr[[2]])))
    },
    compile_quasiquote = function(expr) {
      if (length(expr) != 2) {
        return(private$fail("quasiquote requires exactly 1 argument"))
      }
      private$compile_quasiquote_impl(expr[[2]], 1L)
    },
    compile_macro_quasiquote = function(expr) {
      if (length(expr) != 2) {
        return(private$fail("quasiquote requires exactly 1 argument"))
      }
      as.call(list(
        as.symbol(".__macro_quasiquote"),
        as.call(list(quote(quote), private$strip_src(expr[[2]]))),
        as.symbol(self$env_var_name)
      ))
    },
    # Wrap an inner compiled expression in a quasiquote form (unquote, quasiquote, etc.)
    # When inner is static (quote(...)), wraps statically. Otherwise builds at runtime.
    wrap_qq_form = function(form_name, inner) {
      sym <- as.symbol(form_name)
      if (is.call(inner) && length(inner) == 2L && identical(inner[[1]], quote(quote))) {
        # Inner is a quoted constant — wrap statically
        return(as.call(list(quote(quote), as.call(list(sym, inner[[2]])))))
      }
      # Inner has code that needs evaluation — build (form_name <inner>) at runtime
      as.call(list(quote(as.call), as.call(list(quote(list), as.call(list(quote(quote), sym)), inner))))
    },
    # Check if a compiled R expression references the env var (.__env).
    # Used to skip the .__env <- environment() preamble in simple lambdas.
    references_env_var = function(expr) {
      env_sym <- self$env_var_name
      check <- function(x) {
        if (is.symbol(x)) return(identical(as.character(x), env_sym))
        if (is.call(x) || is.pairlist(x)) {
          for (i in seq_along(x)) {
            if (!is.null(x[[i]]) && check(x[[i]])) return(TRUE)
          }
        }
        FALSE
      }
      check(expr)
    },
    compile_quasiquote_impl = function(template, depth) {
      env_sym <- as.symbol(self$env_var_name)
      eval_in_env <- function(compiled) {
        as.call(list(quote(base::eval), as.call(list(quote(quote), compiled)), envir = env_sym))
      }

      # Helper: Check if template contains any unquotes at current depth
      has_unquotes <- function(tmpl, d) {
        if (!is.call(tmpl)) return(FALSE)
        if (length(tmpl) == 0) return(FALSE)

        # Check operator
        op <- tmpl[[1]]
        if (is.symbol(op)) {
          op_char <- as.character(op)
          # At depth 1, unquote/unquote-splicing are active
          if (d == 1L && (op_char == "unquote" || op_char == "unquote-splicing")) {
            return(TRUE)
          }
          # Nested quasiquote increases depth; unquote/splicing at depth>1 decreases it
          if (op_char == "quasiquote") {
            return(has_unquotes(tmpl[[2]], d + 1L))
          }
          if (d > 1L && (op_char == "unquote" || op_char == "unquote-splicing")) {
            return(has_unquotes(tmpl[[2]], d - 1L))
          }
        }

        # Recursively check all elements
        for (i in seq_along(tmpl)) {
          if (has_unquotes(tmpl[[i]], d)) return(TRUE)
        }
        FALSE
      }

      if (!is.call(template)) {
        return(as.call(list(quote(quote), template)))
      }

      # Optimization: If no unquotes at all, return simple quoted structure
      if (!has_unquotes(template, depth)) {
        # For all-quoted templates, just quote the whole thing
        # This is much simpler than building with as.call(c(...))
        return(as.call(list(quote(quote), template)))
      }
      op <- template[[1]]
      op_char <- if (is.symbol(op)) as.character(op) else ""
      if (op_char == "unquote") {
        if (length(template) != 2) {
          return(private$fail("unquote requires exactly 1 argument"))
        }
        if (depth == 1L) {
          inner <- private$compile_impl(template[[2]])
          if (is.null(inner)) return(NULL)
          return(eval_in_env(inner))
        }
        inner <- private$compile_quasiquote_impl(template[[2]], depth - 1L)
        return(private$wrap_qq_form("unquote", inner))
      }
      if (op_char == "unquote-splicing") {
        if (depth == 1L) {
          return(private$fail("unquote-splicing can only appear in list context"))
        }
        if (length(template) != 2) {
          return(private$fail("unquote-splicing requires exactly 1 argument"))
        }
        inner <- private$compile_quasiquote_impl(template[[2]], depth - 1L)
        return(private$wrap_qq_form("unquote-splicing", inner))
      }
      if (op_char == "quasiquote") {
        if (length(template) != 2) return(private$fail("quasiquote requires exactly 1 argument"))
        inner <- private$compile_quasiquote_impl(template[[2]], depth + 1L)
        return(private$wrap_qq_form("quasiquote", inner))
      }
      if (length(template) == 0) {
        return(as.call(list(quote(quote), template)))
      }
      segments <- vector("list", length(template))
      for (i in seq_along(template)) {
        elem <- template[[i]]
        if (is.call(elem) && length(elem) >= 1L && is.symbol(elem[[1]])) {
          elem_op <- as.character(elem[[1]])
          if (elem_op == "unquote-splicing" && depth == 1L) {
            if (length(elem) != 2) return(private$fail("unquote-splicing requires exactly 1 argument"))
            compiled <- private$compile_impl(elem[[2]])
            if (is.null(compiled)) return(private$fail("unquote-splicing could not be compiled"))
            segments[[i]] <- as.call(list(quote(as.list), eval_in_env(compiled)))
            next
          }
          if (elem_op == "unquote" && depth == 1L) {
            if (length(elem) != 2) return(private$fail("unquote requires exactly 1 argument"))
            compiled <- private$compile_impl(elem[[2]])
            if (is.null(compiled)) return(private$fail("unquote could not be compiled"))
            segments[[i]] <- as.call(list(quote(list), eval_in_env(compiled)))
            next
          }
        }
        part <- private$compile_quasiquote_impl(elem, depth)
        segments[[i]] <- as.call(list(quote(list), part))
      }
      as.call(c(list(quote(as.call), as.call(c(list(quote(c)), segments)))))
    },
    compile_if = function(expr) {
      if (length(expr) < 3 || length(expr) > 4) {
        return(private$fail("if requires 2 or 3 arguments: (if test then [else])"))
      }
      self$nesting_depth <- self$nesting_depth + 1L
      on.exit(self$nesting_depth <- self$nesting_depth - 1L, add = TRUE)
      test <- private$compile_impl(expr[[2]])
      if (is.null(test)) {
        return(private$fail("if test could not be compiled"))
      }

      # Dead code elimination: if test is a compile-time constant, return only the taken branch.
      # Both branches are still compiled to catch structural errors (e.g., import in non-top-level
      # position) even in dead code.
      if (isTRUE(self$enable_dead_code_elim)) {
        constant_test <- private$eval_constant_test(test)
        if (!is.null(constant_test)) {
          if (isTRUE(constant_test)) {
            # Test is TRUE - return then-branch, but still compile else for validation
            then_expr <- private$compile_impl(expr[[3]])
            if (is.null(then_expr)) {
              return(private$fail("if then-branch could not be compiled"))
            }
            if (length(expr) == 4) {
              private$compile_impl(expr[[4]])
            }
            return(then_expr)
          } else {
            # Test is FALSE or NULL - return else-branch, but still compile then for validation
            private$compile_impl(expr[[3]])
            if (length(expr) == 4) {
              else_expr <- private$compile_impl(expr[[4]])
              if (is.null(else_expr)) {
                return(private$fail("if else-branch could not be compiled"))
              }
              return(else_expr)
            } else {
              return(private$compiled_nil())
            }
          }
        }
      }

      # Non-constant test - compile normally
      then_expr <- private$compile_impl(expr[[3]])
      if (is.null(then_expr)) {
        return(private$fail("if then-branch could not be compiled"))
      }
      then_expr <- private$wrap_branch_coverage(then_expr, expr[[3]])
      # Arl: only #f and #nil are false
      # Skip .__true_p wrapper if test is known to return proper R logical
      test_pred <- if (isTRUE(self$enable_truthiness_opt) && private$returns_r_logical(test)) {
        test
      } else {
        as.call(list(as.symbol(".__true_p"), test))
      }

      if (length(expr) == 4) {
        else_expr <- private$compile_impl(expr[[4]])
        if (is.null(else_expr)) {
          return(private$fail("if else-branch could not be compiled"))
        }
        else_expr <- private$wrap_branch_coverage(else_expr, expr[[4]])
        as.call(list(quote(`if`), test_pred, then_expr, else_expr))
      } else {
        as.call(list(quote(`if`), test_pred, then_expr, private$compiled_nil()))
      }
    },
    compile_begin = function(expr) {
      if (length(expr) <= 1) {
        return(quote(invisible(NULL)))
      }
      parts <- as.list(expr)[-1]

      # Optimization: single expression doesn't need block wrapper
      if (isTRUE(self$enable_begin_simplify) && length(parts) == 1) {
        return(private$compile_impl(parts[[1]]))
      }

      # Multiple expressions - use block
      # Use early-exit loop with preallocation
      compiled <- vector("list", length(parts))
      for (i in seq_along(parts)) {
        compiled[[i]] <- private$compile_impl(parts[[i]])
        if (is.null(compiled[[i]])) {
          return(NULL)  # Early exit on failure
        }
      }
      as.call(c(list(quote(`{`)), compiled))
    },
    compile_define = function(expr) {
      if (length(expr) != 3) {
        return(private$fail("define requires exactly 2 arguments: (define name value)"))
      }
      name <- expr[[2]]
      val <- private$compile_assignment_value(expr, "define")
      if (is.null(val)) return(NULL)

      # Simple symbol: emit .__define (fast path, no destructuring overhead)
      # Destructuring: emit .__assign_pattern (slow path)
      is_simple <- is.symbol(name)
      pattern_arg <- if (is_simple) as.character(name) else as.call(list(as.symbol(".__quote"), name))

      tmp_sym <- self$gensym(".__define_value")
      assign_tmp <- as.call(list(quote(`<-`), tmp_sym, val))

      if (is_simple) {
        assign_call <- as.call(list(
          as.symbol(".__define"),
          as.symbol(self$env_var_name),
          pattern_arg,
          tmp_sym
        ))
      } else {
        assign_call <- as.call(list(
          as.symbol(".__assign_pattern"),
          as.symbol(self$env_var_name),
          pattern_arg,
          tmp_sym,
          "define"
        ))
      }

      # Check if annotations exist for this name (from ;;' blocks)
      ann <- NULL
      name_str <- private$as_name_string(name)
      if (!is.null(name_str) && !is.null(self$annotations)) {
        ann <- self$annotations[[name_str]]
      }
      if (!is.null(ann)) {
        doc_list <- private$build_doc_list(ann)
        if (!is.null(doc_list)) {
          # Use .__attach_doc helper (handles primitive wrapping)
          return(as.call(list(
            quote(`{`),
            as.call(list(quote(`<-`), tmp_sym,
              as.call(list(as.symbol(".__attach_doc"), val, doc_list))
            )),
            assign_call,
            as.call(list(quote(invisible), tmp_sym))
          )))
        }
      }
      # (define x v) returns invisible(v); evaluate v once.
      as.call(list(quote(`{`), assign_tmp, assign_call, as.call(list(quote(invisible), tmp_sym))))
    },
    compile_set = function(expr) {
      if (length(expr) != 3) {
        return(private$fail("set! requires exactly 2 arguments: (set! name value)"))
      }
      name <- expr[[2]]
      val <- private$compile_assignment_value(expr, "set!")
      if (is.null(val)) return(NULL)

      # Simple symbol: emit .__set (fast path, bounded walk)
      # Destructuring: emit .__assign_pattern (slow path)
      if (is.symbol(name)) {
        as.call(list(
          as.symbol(".__set"),
          as.symbol(self$env_var_name),
          as.character(name),
          val
        ))
      } else {
        pattern_arg <- as.call(list(as.symbol(".__quote"), name))
        as.call(list(
          as.symbol(".__assign_pattern"),
          as.symbol(self$env_var_name),
          pattern_arg,
          val,
          "set"
        ))
      }
    },
    compile_lambda = function(expr, self_name = NULL) {
      if (length(expr) < 3) {
        return(private$fail("lambda requires at least 2 arguments: (lambda (args...) body...)"))
      }
      args_expr <- expr[[2]]
      params <- private$lambda_params(args_expr)
      if (is.null(params)) {
        return(private$fail("lambda arguments must be a list"))
      }
      self$nesting_depth <- self$nesting_depth + 1L
      on.exit(self$nesting_depth <- self$nesting_depth - 1L, add = TRUE)
      body_exprs <- if (length(expr) >= 3) {
        as.list(expr)[-(1:2)]
      } else {
        list()
      }
      if (length(body_exprs) == 0) {
        body_exprs <- list(private$compiled_nil())
      }
      # Check for self-tail-call optimization opportunity
      use_tco <- FALSE
      param_names <- NULL
      rest_param_name <- NULL
      if (!is.null(self_name) && self$enable_tco) {
        param_names <- setdiff(names(params$formals_list), "...")
        rest_param_name <- params$rest_param  # NULL if no rest param
        # For pattern rest params, generate a temp name for collection
        if (is.null(rest_param_name) && !is.null(params$rest_param_spec) &&
            identical(params$rest_param_spec$type, "pattern")) {
          rest_param_name <- as.character(self$gensym(".__rest"))
        }
        if (private$has_self_tail_calls(body_exprs, self_name)) {
          use_tco <- TRUE
        }
      }
      if (use_tco) {
        # TCO path: compile all but last normally, last via tail-position compiler
        compiled_body <- vector("list", length(body_exprs))
        for (i in seq_len(length(body_exprs) - 1L)) {
          compiled_body[[i]] <- private$compile_impl(body_exprs[[i]])
          if (is.null(compiled_body[[i]])) {
            return(private$fail("lambda body could not be compiled"))
          }
        }
        last_compiled <- private$compile_tail_position(
          body_exprs[[length(body_exprs)]], self_name, param_names,
          rest_param_name = rest_param_name
        )
        if (is.null(last_compiled)) {
          return(private$fail("lambda body could not be compiled"))
        }
        compiled_body[[length(body_exprs)]] <- last_compiled
        # Interleave coverage tracking calls (fires at call time, not definition time)
        compiled_body <- private$interleave_coverage(body_exprs, compiled_body)
        # Prepend destructuring bindings inside the loop (they must re-run each iteration
        # because the temp formals get reassigned by self-tail-calls)
        if (length(params$param_bindings) > 0) {
          pattern_stmts <- vector("list", length(params$param_bindings))
          for (pb_i in seq_along(params$param_bindings)) {
            binding <- params$param_bindings[[pb_i]]
            pattern_arg <- as.call(list(as.symbol(".__quote"), binding$pattern))
            pattern_stmts[[pb_i]] <- as.call(list(
              as.symbol(".__assign_pattern"),
              as.symbol(self$env_var_name),
              pattern_arg,
              as.symbol(binding$name),
              "define"
            ))
          }
          compiled_body <- c(pattern_stmts, compiled_body)
        }
        # Add pattern rest binding inside the loop (re-runs each iteration)
        if (!is.null(params$rest_param_spec) && identical(params$rest_param_spec$type, "pattern")) {
          pattern_arg <- as.call(list(as.symbol(".__quote"), params$rest_param_spec$pattern))
          rest_pattern_stmt <- as.call(list(
            as.symbol(".__assign_pattern"),
            as.symbol(self$env_var_name),
            pattern_arg,
            as.symbol(rest_param_name),
            "define"
          ))
          compiled_body <- c(list(rest_pattern_stmt), compiled_body)
        }
        # Wrap compiled body in while(TRUE) { ... }
        # Use while(TRUE) instead of repeat because Arl stdlib defines a `repeat` function
        # that shadows R's repeat keyword in the engine environment.
        loop_body <- private$src_inherit(
          as.call(c(list(quote(`{`)), compiled_body)),
          body_exprs[[length(body_exprs)]])
        # If there's a rest param, bind it from ... before the loop
        pre_loop <- list()
        if (!is.null(rest_param_name)) {
          pre_loop <- list(as.call(list(quote(`<-`), as.symbol(rest_param_name), quote(list(...)))))
        }
        compiled_body <- c(pre_loop, list(private$src_inherit(
          as.call(list(quote(`while`), TRUE, loop_body)), expr)))
      } else {
        # Normal path: compile all body expressions
        compiled_body <- vector("list", length(body_exprs))
        for (i in seq_along(body_exprs)) {
          compiled_body[[i]] <- private$compile_impl(body_exprs[[i]])
          if (is.null(compiled_body[[i]])) {
            return(private$fail("lambda body could not be compiled"))
          }
        }
        # Interleave coverage tracking calls (fires at call time, not definition time)
        compiled_body <- private$interleave_coverage(body_exprs, compiled_body)
      }
      # Prepend .__env <- environment() so closure body sees correct current env.
      # Skip this for simple lambdas whose body never references .__env (avoids overhead).
      env_bind <- as.call(list(quote(`<-`), as.symbol(self$env_var_name), quote(environment())))
      # When use_tco is TRUE, rest binding and param_bindings are already handled
      # inside/around the TCO loop, so skip them here.
      n_rest <- if (!use_tco && params$has_rest && !is.null(params$rest_param)) 1L else 0L
      n_bindings <- if (!use_tco) length(params$param_bindings) else 0L
      has_rest_pattern <- !use_tco && !is.null(params$rest_param_spec) &&
        identical(params$rest_param_spec$type, "pattern")
      n_rest_spec <- if (has_rest_pattern) 1L else 0L
      # Check if body (or param bindings) reference .__env; if not, skip the assignment
      needs_env <- n_bindings > 0L || n_rest_spec > 0L || use_tco
      if (!needs_env) {
        body_block <- as.call(c(list(quote(`{`)), compiled_body))
        needs_env <- private$references_env_var(body_block)
      }
      n_env <- if (needs_env) 1L else 0L
      size <- n_env + n_rest + n_bindings + n_rest_spec + length(compiled_body)
      body_parts <- vector("list", size)
      idx <- 1
      if (needs_env) {
        body_parts[[idx]] <- env_bind
        idx <- idx + 1
      }
      if (n_rest > 0L) {
        body_parts[[idx]] <- as.call(list(quote(`<-`), as.symbol(params$rest_param), quote(list(...))))
        idx <- idx + 1
      }
      if (n_bindings > 0L) {
        for (binding in params$param_bindings) {
          pattern_arg <- as.call(list(as.symbol(".__quote"), binding$pattern))
          body_parts[[idx]] <- as.call(list(
            as.symbol(".__assign_pattern"),
            as.symbol(self$env_var_name),
            pattern_arg,
            as.symbol(binding$name),
            "define"
          ))
          idx <- idx + 1
        }
      }
      if (n_rest_spec > 0L) {
        pattern_arg <- as.call(list(as.symbol(".__quote"), params$rest_param_spec$pattern))
        rest_val <- as.call(list(quote(list), quote(...)))
        body_parts[[idx]] <- as.call(list(
          as.symbol(".__assign_pattern"),
          as.symbol(self$env_var_name),
          pattern_arg,
          rest_val,
          "define"
        ))
        idx <- idx + 1
      }
      for (i in seq_along(compiled_body)) {
        body_parts[[idx]] <- compiled_body[[i]]
        idx <- idx + 1
      }
      body_call <- as.call(c(list(quote(`{`)), body_parts))
      formals_list <- params$formals_list
      fn_expr <- as.call(list(
        quote(as.function),
        c(formals_list, list(body_call)),
        envir = as.symbol(self$env_var_name)
      ))
      fn_expr
    },
    lambda_params = function(args_expr) {
      arg_items <- if (is.null(args_expr)) {
        list()
      } else if (inherits(args_expr, "ArlCons")) {
        parts <- args_expr$parts()
        c(parts$prefix, list(as.symbol(".")), list(parts$tail))
      } else if (is.call(args_expr) && length(args_expr) > 0) {
        as.list(args_expr)
      } else if (is.list(args_expr)) {
        args_expr
      } else {
        return(private$fail("lambda arguments must be a list"))
      }
      rest_param <- NULL
      rest_param_spec <- NULL
      if (length(arg_items) > 0) {
        dot_idx <- which(vapply(arg_items, function(a) {
          is.symbol(a) && identical(as.character(a), ".")
        }, logical(1)))
        if (length(dot_idx) > 1) {
          return(private$fail("Dotted parameter list can only contain one '.'"))
        }
        if (length(dot_idx) == 1) {
          if (dot_idx != length(arg_items) - 1) {
            return(private$fail("Dotted parameter list must have exactly one parameter after '.'"))
          }
          rest_arg <- arg_items[[dot_idx + 1]]
          if (is.symbol(rest_arg)) {
            rest_param <- as.character(rest_arg)
            rest_param_spec <- list(type = "name", name = rest_param, pattern = NULL)
          } else {
            rest_list <- if (is.call(rest_arg)) as.list(rest_arg) else if (is.list(rest_arg)) rest_arg else NULL
            if (is.null(rest_list) || length(rest_list) < 2 ||
                !is.symbol(rest_list[[1]]) ||
                !(as.character(rest_list[[1]]) %in% c("pattern", "destructure"))) {
              return(private$fail("Rest parameter must be a symbol or (pattern <pat>)"))
            }
            if (length(rest_list) != 2) {
              return(private$fail("Rest pattern must be (pattern <pat>)"))
            }
            rest_pattern <- rest_list[[2]]
            rest_display <- paste(deparse(rest_pattern, width.cutoff = 500), collapse = " ")
            rest_param_spec <- list(
              type = "pattern",
              name = NULL,
              pattern = rest_pattern,
              display = rest_display
            )
          }
          arg_items <- if (dot_idx > 1) arg_items[1:(dot_idx - 1)] else list()
        }
      }
      formals_list <- list()
      param_bindings <- list()
      for (arg in arg_items) {
        if (is.symbol(arg)) {
          name <- as.character(arg)
          formals_list[[name]] <- base::quote(expr = )
        } else if (is.call(arg) && length(arg) >= 2 && is.symbol(arg[[1]])) {
          op_char <- as.character(arg[[1]])
          if (op_char %in% c("pattern", "destructure")) {
            if (length(arg) != 2 && length(arg) != 3) {
              return(private$fail("pattern wrapper must be (pattern <pat>) or (pattern <pat> <default>)"))
            }
            pattern <- arg[[2]]
            tmp_sym <- self$gensym(".__arg")
            tmp_name <- as.character(tmp_sym)
            if (length(arg) == 3) {
              default_expr <- private$compile_impl(arg[[3]])
              if (is.null(default_expr)) {
                return(private$fail("lambda default value could not be compiled"))
              }
              formals_list[[tmp_name]] <- default_expr
            } else {
              formals_list[[tmp_name]] <- base::quote(expr = )
            }
            display <- paste(deparse(pattern, width.cutoff = 500), collapse = " ")
            param_bindings[[length(param_bindings) + 1]] <- list(
              type = "pattern",
              name = tmp_name,
              pattern = pattern,
              display = display
            )
          } else if (length(arg) == 2) {
            name <- op_char
            default_expr <- private$compile_impl(arg[[2]])
            if (is.null(default_expr)) {
              return(private$fail("lambda default value could not be compiled"))
            }
            formals_list[[name]] <- default_expr
          } else {
            return(private$fail("lambda arguments must be symbols, (name default) pairs, or (pattern <pat> [default])"))
          }
        } else {
          return(private$fail("lambda arguments must be symbols, (name default) pairs, or (pattern <pat> [default])"))
        }
      }
      if (!is.null(rest_param) || !is.null(rest_param_spec)) {
        formals_list[["..."]] <- base::quote(expr = )
      }
      list(
        formals_list = formals_list,
        has_rest = !is.null(rest_param) || !is.null(rest_param_spec),
        rest_param = rest_param,
        rest_param_spec = rest_param_spec,
        param_bindings = param_bindings
      )
    },
    compile_import = function(expr) {
      if (self$nesting_depth > 0L) {
        return(private$fail("import is only allowed at the top level of a program or module body"))
      }
      if (length(expr) < 2) {
        return(private$fail("import requires at least 1 argument: (import name)"))
      }
      # Pass argument unevaluated (module name is a symbol/string, not a variable lookup)
      arg_quoted <- as.call(list(quote(quote), expr[[2]]))

      # Parse optional keyword modifiers: :refer, :as, :rename, :reload
      rename <- NULL
      reload <- FALSE
      as_alias <- NULL
      refer <- NULL

      if (length(expr) > 2) {
        rest <- as.list(expr)[-(1:2)]
        i <- 1
        while (i <= length(rest)) {
          kw <- rest[[i]]
          if (!inherits(kw, "arl_keyword")) {
            return(private$fail(sprintf(
              "import: expected keyword modifier, got %s",
              deparse(kw)
            )))
          }
          kw_name <- as.character(kw)

          # :reload is a bare flag (no value)
          if (kw_name == "reload") {
            if (reload) return(private$fail("import: duplicate :reload modifier"))
            reload <- TRUE
            i <- i + 1
            next
          }

          if (i + 1 > length(rest)) {
            return(private$fail(sprintf(
              "import: keyword :%s requires a value", kw_name
            )))
          }
          val <- rest[[i + 1]]
          i <- i + 2

          if (kw_name == "rename") {
            if (!is.null(rename)) {
              return(private$fail("import: duplicate :rename modifier"))
            }
            # val should be a list of 2-element lists: ((old new) ...)
            rename_map <- private$parse_rename_list(val)
            if (is.character(rename_map) && length(rename_map) == 1 && startsWith(rename_map, "ERROR:")) {
              return(private$fail(substring(rename_map, 7)))
            }
            rename <- rename_map
          } else if (kw_name == "as") {
            if (!is.null(as_alias)) {
              return(private$fail("import: duplicate :as modifier"))
            }
            if (!is.symbol(val)) {
              return(private$fail("import: :as requires a symbol"))
            }
            alias_str <- as.character(val)
            if (grepl("/", alias_str, fixed = TRUE)) {
              return(private$fail("import: :as alias must not contain '/'"))
            }
            as_alias <- alias_str
          } else if (kw_name == "refer") {
            if (!is.null(refer)) {
              return(private$fail("import: duplicate :refer modifier"))
            }
            # :refer :all or :refer (sym1 sym2 ...)
            if (inherits(val, "arl_keyword") && identical(as.character(val), "all")) {
              refer <- TRUE
            } else {
              syms <- private$parse_symbol_list(val, ":refer")
              if (is.character(syms) && length(syms) == 1 && startsWith(syms, "ERROR:")) {
                return(private$fail(substring(syms, 7)))
              }
              refer <- syms
            }
          } else {
            return(private$fail(sprintf(
              "import: unknown modifier :%s (expected :as, :refer, :rename, or :reload)",
              kw_name
            )))
          }
        }
      }

      # Build the call: .__import(quote(name), env, ...)
      call_args <- list(
        as.symbol(".__import"),
        arg_quoted,
        as.symbol(self$env_var_name)
      )
      if (!is.null(rename)) {
        call_args$rename <- rename
      }
      if (isTRUE(reload)) {
        call_args$reload <- TRUE
      }
      if (!is.null(as_alias)) {
        call_args$as_alias <- as_alias
      }
      if (!is.null(refer)) {
        if (isTRUE(refer)) {
          call_args$refer <- TRUE
        } else {
          call_args$refer <- refer
        }
      }
      as.call(call_args)
    },
    compile_import_runtime = function(expr) {
      private$fail("import-runtime is reserved for future use and not yet implemented")
    },
    compile_while = function(expr) {
      if (length(expr) < 3) {
        return(private$fail("while requires at least 2 arguments: (while test body...)"))
      }
      self$nesting_depth <- self$nesting_depth + 1L
      on.exit(self$nesting_depth <- self$nesting_depth - 1L, add = TRUE)
      test <- private$compile_impl(expr[[2]])
      if (is.null(test)) {
        return(private$fail("while test could not be compiled"))
      }
      body_expr <- if (length(expr) == 3) expr[[3]] else as.call(c(list(quote(begin)), as.list(expr)[-(1:2)]))
      body_compiled <- private$compile_impl(body_expr)
      if (is.null(body_compiled)) {
        return(private$fail("while body could not be compiled"))
      }
      test_pred <- as.call(list(as.symbol(".__true_p"), test))
      as.call(list(quote(`while`), test_pred, body_compiled))
    },
    compile_delay = function(expr) {
      if (length(expr) != 2) {
        return(private$fail("delay requires exactly 1 argument"))
      }
      self$nesting_depth <- self$nesting_depth + 1L
      # Disable constant folding to preserve expression structure for promise-expr
      old_folding <- self$enable_constant_folding
      self$enable_constant_folding <- FALSE
      on.exit({
        self$enable_constant_folding <- old_folding
        self$nesting_depth <- self$nesting_depth - 1L
      }, add = TRUE)
      compiled <- private$compile_impl(expr[[2]])
      if (is.null(compiled)) {
        return(private$fail("delay expression could not be compiled"))
      }
      # delay uses .__delay for promise creation and memoization.
      as.call(list(
        as.symbol(".__delay"),
        as.call(list(quote(quote), compiled)),
        as.symbol(self$env_var_name)
      ))
    },
    compile_and = function(expr) {
      private$compile_short_circuit(expr, TRUE, quote(and), TRUE)
    },
    compile_or = function(expr) {
      private$compile_short_circuit(expr, FALSE, quote(or), FALSE)
    },
    compile_defmacro = function(expr) {
      if (length(expr) < 4) {
        return(private$fail("defmacro requires at least 3 arguments: (defmacro name (params...) body...)"))
      }
      name <- expr[[2]]
      if (!is.symbol(name)) {
        return(private$fail("defmacro requires a symbol as the first argument"))
      }
      params_expr <- expr[[3]]
      if (inherits(params_expr, "ArlCons")) {
        parts <- params_expr$parts()
        params_expr <- as.call(c(parts$prefix, list(as.symbol(".")), list(parts$tail)))
      }
      # Accept empty parameter list: parser represents () as list() (not a call)
      if (is.list(params_expr) && length(params_expr) == 0L) {
        params_expr <- NULL
      }
      if (!is.call(params_expr) && !is.null(params_expr)) {
        return(private$fail("defmacro parameters must be a list"))
      }
      if (is.call(params_expr) && length(params_expr) > 0) {
        for (i in seq_along(params_expr)) {
          item <- params_expr[[i]]
          if (is.symbol(item)) {
            next
          }
          if (is.call(item)) {
            if (length(item) < 1) {
              return(private$fail("defmacro parameters must be symbols, (name default), or (pattern ...)"))
            }
            op_char <- if (is.symbol(item[[1]])) as.character(item[[1]]) else NULL
            if (!is.null(op_char) && op_char %in% c("pattern", "destructure")) {
              if (length(item) != 2 && length(item) != 3) {
                return(private$fail("pattern must be (pattern <pat>) or (pattern <pat> <default>)"))
              }
              next
            }
            if (length(item) == 2 && is.symbol(item[[1]])) {
              next
            }
          } else if (is.list(item)) {
            item_list <- item
            if (length(item_list) == 0) {
              return(private$fail("defmacro parameters must be symbols, (name default), or (pattern ...)"))
            }
            op_char <- if (is.symbol(item_list[[1]])) as.character(item_list[[1]]) else NULL
            if (!is.null(op_char) && op_char %in% c("pattern", "destructure")) {
              if (length(item_list) != 2 && length(item_list) != 3) {
                return(private$fail("pattern must be (pattern <pat>) or (pattern <pat> <default>)"))
              }
              next
            }
            if (length(item_list) == 2 && is.symbol(item_list[[1]])) {
              next
            }
          }
          return(private$fail("defmacro parameters must be symbols, (name default), or (pattern ...)"))
        }
      }
      body_exprs <- as.list(expr)[-(1:3)]
      doc_list <- NULL
      if (!is.null(self$annotations)) {
        macro_name_str <- as.character(name)
        ann <- self$annotations[[macro_name_str]]
        if (!is.null(ann)) {
          doc_list <- private$build_doc_list(ann)
        }
      }
      body_quoted <- as.call(list(quote(quote), as.call(c(list(quote(begin)), body_exprs))))
      doc_list_arg <- if (is.null(doc_list)) private$compiled_nil() else doc_list
      as.call(list(
        as.symbol(".__defmacro"),
        as.call(list(quote(quote), name)),
        as.call(list(quote(quote), params_expr)),
        body_quoted,
        doc_list_arg,
        as.symbol(self$env_var_name)
      ))
    },
    compile_module = function(expr) {
      if (length(expr) < 2) {
        return(private$fail("module requires at least: (module name (export ...) body...) or (module (export ...) body...)"))
      }

      # Detect nameless form: (module (export ...) body...)
      # vs named form: (module name (export ...) body...)
      second <- expr[[2]]
      is_nameless <- is.call(second) && length(second) >= 1 && is.symbol(second[[1]]) &&
        as.character(second[[1]]) %in% c("export", "export-all")

      if (is_nameless) {
        name_str <- ""
        exports_expr <- second
        body_exprs <- as.list(expr)[-(1:2)]
      } else {
        if (length(expr) < 3) {
          return(private$fail("named module requires at least 2 arguments: (module name (export ...) body...)"))
        }
        module_name <- second
        name_str <- private$as_name_string(module_name)
        if (is.null(name_str)) {
          return(private$fail("module name must be a symbol or string"))
        }
        exports_expr <- expr[[3]]
        body_exprs <- as.list(expr)[-(1:3)]
      }

      if (!is.call(exports_expr) || length(exports_expr) < 1 || !is.symbol(exports_expr[[1]])) {
        return(private$fail("module requires an export list: (module name (export ...) body...)"))
      }
      export_tag <- as.character(exports_expr[[1]])
      export_all <- FALSE
      re_export <- FALSE
      exports <- character(0)
      if (identical(export_tag, "export")) {
        if (length(exports_expr) > 1) {
          for (i in 2:length(exports_expr)) {
            item <- exports_expr[[i]]
            if (!is.symbol(item)) {
              return(private$fail("module exports must be symbols"))
            }
            exports <- c(exports, as.character(item))
          }
        }
      } else if (identical(export_tag, "export-all")) {
        if (length(exports_expr) > 1) {
          for (i in 2:length(exports_expr)) {
            item <- exports_expr[[i]]
            if (inherits(item, "arl_keyword") && identical(as.character(item), "re-export")) {
              re_export <- TRUE
            } else {
              return(private$fail("export-all only accepts :re-export modifier"))
            }
          }
        }
        export_all <- TRUE
      } else {
        return(private$fail("module requires an export list: (module name (export ...) body...)"))
      }
      body_exprs <- if (length(body_exprs) == 0) list() else body_exprs
      src <- private$src_get(expr)
      src_file <- NULL
      has_file_path <- !is.null(src) && !is.null(src$file) &&
        is.character(src$file) && nzchar(src$file) && grepl("[/\\\\]", src$file)
      if (has_file_path) {
        src_file <- src$file
      }
      compiled_body_quoted <- as.call(list(quote(quote), body_exprs))
      as.call(list(
        as.symbol(".__module"),
        name_str,
        exports,
        export_all,
        re_export,
        compiled_body_quoted,
        src_file,
        as.symbol(self$env_var_name)
      ))
    },
    compile_application = function(expr) {
      op <- private$compile_impl(expr[[1]])
      if (is.null(op)) {
        return(private$fail("attempt to call non-function"))
      }
      # Helper to collect keyword/positional arguments with preallocation
      collect_args <- function() {
        # Preallocate: worst case is all positional (expr_len - 1)
        max_args <- length(expr) - 1
        args <- vector("list", max_args)
        arg_names <- character(max_args)
        arg_idx <- 1
        i <- 2
        while (i <= length(expr)) {
          arg_expr <- expr[[i]]
          if (inherits(arg_expr, "arl_keyword")) {
            if (i + 1 > length(expr)) {
              return(private$fail(sprintf("Keyword :%s requires a value", arg_expr)))
            }
            keyword_name <- as.character(arg_expr)
            val <- private$compile_impl(expr[[i + 1]])
            if (is.null(val)) {
              return(private$fail("argument could not be compiled"))
            }
            args[[arg_idx]] <- val
            arg_names[arg_idx] <- keyword_name
            arg_idx <- arg_idx + 1
            i <- i + 2
          } else {
            val <- private$compile_impl(arg_expr)
            if (is.null(val)) {
              return(private$fail("argument could not be compiled"))
            }
            args[[arg_idx]] <- val
            arg_names[arg_idx] <- ""
            arg_idx <- arg_idx + 1
            i <- i + 1
          }
        }
        # Trim to actual size (handle zero-argument case)
        if (arg_idx == 1) {
          # No arguments
          return(list())
        }
        args <- args[seq_len(arg_idx - 1)]
        names(args) <- arg_names[seq_len(arg_idx - 1)]
        args
      }
      if (is.symbol(op)) {
        op_char <- as.character(op)
        if (op_char %in% c("$", "[", "[[")) {
          args <- collect_args()
          if (is.null(args)) return(NULL)  # Error occurred
          return(as.call(list(
            as.symbol(".__subscript_call"),
            op_char,
            as.call(c(list(quote(list)), args)),
            as.symbol(self$env_var_name)
          )))
        }
      }
      args <- collect_args()
      if (is.null(args)) return(NULL)  # Error occurred

      # Attempt constant folding if operator is a known pure function (and if enabled)
      if (isTRUE(self$enable_constant_folding)) {
        folded <- private$try_constant_fold(op, args)
        if (!is.null(folded)) {
          return(folded)
        }
      }

      # Attempt strength reduction: replace expensive ops with cheaper equivalents
      if (isTRUE(self$enable_strength_reduction)) {
        strength_reduced <- private$try_strength_reduction(op, args)
        if (!is.null(strength_reduced)) {
          return(strength_reduced)
        }
      }

      # Attempt identity elimination: detect ((lambda (x) x) value) and inline
      # Check the original expression to see if operator is a lambda
      if (isTRUE(self$enable_identity_elim) && is.call(expr[[1]]) && length(expr[[1]]) >= 3) {
        if (is.symbol(expr[[1]][[1]]) && as.character(expr[[1]][[1]]) == "lambda") {
          identity_result <- private$try_identity_elimination(expr[[1]], args)
          if (!is.null(identity_result)) {
            return(identity_result)
          }
        }
      }

      as.call(c(list(op), args))
    },

    # Attempt to fold constant expressions at compile time
    # Returns folded literal or NULL if folding not possible
    try_constant_fold = function(op, args) {
      # Only fold if operator is a symbol
      if (!is.symbol(op)) return(NULL)

      op_name <- as.character(op)

      # Check against module-level .PURE_FUNCTIONS hash set (O(1) lookup)
      if (!exists(op_name, envir = .PURE_FUNCTIONS, inherits = FALSE)) {
        return(NULL)
      }

      # Check if all arguments are literals (atomic values)
      # We consider NULL, logical, integer, double, complex, character, and raw as literals
      all_literals <- TRUE
      for (arg in args) {
        if (!private$is_literal(arg)) {
          all_literals <- FALSE
          break
        }
      }

      if (!all_literals) {
        return(NULL)
      }

      # Attempt to evaluate at compile time
      # Use tryCatch to safely handle errors (e.g., division by zero, domain errors)
      tryCatch({
        # Build the call
        call_expr <- as.call(c(list(op), args))
        # Evaluate in a clean environment
        result <- eval(call_expr, envir = baseenv())
        # Return as a literal (quote to preserve as-is)
        if (is.null(result)) {
          return(quote(NULL))
        }
        # For other values, return as-is (they're already literals)
        return(result)
      }, error = function(e) {
        # If evaluation fails, don't fold (return NULL)
        NULL
      })
    },

    # Check if an expression is a compile-time literal
    is_literal = function(expr) {
      # NULL, TRUE, FALSE are symbols but should be treated as literals
      if (is.symbol(expr)) {
        expr_str <- as.character(expr)
        return(expr_str %in% c("NULL", "TRUE", "FALSE", "NA", "NaN", "Inf"))
      }
      # Atomic vectors (numeric, logical, character, etc.)
      if (is.atomic(expr) && !is.symbol(expr)) {
        return(TRUE)
      }
      FALSE
    },

    # Check if an expression is guaranteed to return a proper R logical (TRUE/FALSE)
    # These expressions don't need .__true_p() wrapper
    returns_r_logical = function(expr) {
      # Literal TRUE/FALSE/NULL
      if (is.logical(expr) && length(expr) == 1) {
        return(TRUE)
      }
      if (is.null(expr)) {
        return(TRUE)
      }
      if (is.symbol(expr)) {
        expr_str <- as.character(expr)
        if (expr_str %in% c("TRUE", "FALSE")) {
          return(TRUE)
        }
      }
      # Note: quote(NULL) is how NULL is compiled, but it's NOT a boolean!
      # It's a language object that needs .__true_p() wrapper

      # Check if it's a call to a function that returns logical
      if (!is.call(expr)) {
        return(FALSE)
      }

      op <- expr[[1]]
      if (!is.symbol(op)) {
        return(FALSE)
      }

      op_name <- as.character(op)

      # Comparison operators always return proper R logicals
      if (op_name %in% c("<", ">", "<=", ">=", "==", "!=")) {
        return(TRUE)
      }

      # Logical operators return proper R logicals
      if (op_name %in% c("&", "|", "!", "&&", "||")) {
        return(TRUE)
      }

      # Type checking functions return logical
      if (op_name %in% c("is.null", "is.na", "is.nan", "is.finite", "is.infinite",
                         "is.numeric", "is.character", "is.logical", "is.list",
                         "is.vector", "is.matrix", "is.array", "is.function")) {
        return(TRUE)
      }

      # Other functions known to return logical
      if (op_name %in% c("all", "any", "identical", "exists")) {
        return(TRUE)
      }

      FALSE
    },

    # Evaluate a test expression if it's a compile-time constant
    # Returns NULL if not constant, TRUE if truthy, FALSE if falsy (according to Arl semantics)
    eval_constant_test = function(test) {
      # Literal TRUE/FALSE
      if (is.logical(test) && length(test) == 1 && !is.na(test)) {
        return(test)  # TRUE or FALSE
      }

      # Literal NULL (compiled as quote(NULL)) - falsy in Arl
      if (is.call(test) && length(test) == 2) {
        if (identical(test[[1]], quote(quote)) && is.null(test[[2]])) {
          return(FALSE)  # NULL is falsy in Arl
        }
      }

      # Symbol TRUE/FALSE/NULL
      if (is.symbol(test)) {
        test_str <- as.character(test)
        if (test_str == "TRUE") return(TRUE)
        if (test_str == "FALSE") return(FALSE)
        if (test_str == "NULL") return(FALSE)  # NULL is falsy in Arl
      }

      # Not a compile-time constant
      NULL
    },

    # Try strength reduction: replace expensive operations with cheaper ones
    # Returns the reduced expression if applicable, NULL otherwise
    try_strength_reduction = function(op, args) {
      if (!is.symbol(op)) return(NULL)
      op_name <- as.character(op)

      # Multiplication by 2: (* x 2) → (+ x x)
      # Only reduce when the duplicated operand is a symbol or literal (no side effects)
      if (op_name == "*" && length(args) == 2) {
        # Check if second arg is literal 2
        if (is.numeric(args[[2]]) && args[[2]] == 2 &&
            (is.symbol(args[[1]]) || is.atomic(args[[1]]))) {
          # Replace with addition: x + x
          return(as.call(list(quote(`+`), args[[1]], args[[1]])))
        }
        # Also handle (* 2 x)
        if (is.numeric(args[[1]]) && args[[1]] == 2 &&
            (is.symbol(args[[2]]) || is.atomic(args[[2]]))) {
          return(as.call(list(quote(`+`), args[[2]], args[[2]])))
        }
      }

      # Power of 2: (^ x 2) → (* x x)
      # Only reduce when the duplicated operand is a symbol or literal (no side effects)
      if (op_name == "^" && length(args) == 2) {
        # Check if second arg is literal 2
        if (is.numeric(args[[2]]) && args[[2]] == 2 &&
            (is.symbol(args[[1]]) || is.atomic(args[[1]]))) {
          # Replace with multiplication: x * x
          return(as.call(list(quote(`*`), args[[1]], args[[1]])))
        }
      }

      # No reduction applicable
      NULL
    },

    # Try to eliminate identity lambda: ((lambda (x) x) value) → value
    # Returns the appropriate argument if lambda is identity, NULL otherwise
    try_identity_elimination = function(lambda_expr, compiled_args) {
      # lambda_expr is the original (lambda (...) body) expression
      # compiled_args are the already-compiled arguments

      # Lambda must have at least (lambda params body)
      if (length(lambda_expr) < 3) {
        return(NULL)
      }

      params <- lambda_expr[[2]]
      body <- lambda_expr[[3]]

      # Handle simple parameter list (not pattern matching, not dotted)
      if (!is.call(params)) {
        return(NULL)  # Should be a list
      }

      # Extract parameter names from the call structure
      # For (x), params is x()
      # For (a b), params is a(b)
      # For (a b c), params is a(b, c)
      # So we iterate through all elements of the call
      param_count <- length(params)
      param_names <- character(param_count)

      for (i in seq_len(param_count)) {
        p <- params[[i]]
        if (is.symbol(p)) {
          param_names[i] <- as.character(p)
        } else {
          # Complex param (default value, pattern, etc.) - don't optimize
          return(NULL)
        }
      }


      # Body must be a single symbol
      if (!is.symbol(body)) {
        return(NULL)
      }

      body_name <- as.character(body)

      # Find which parameter the body references
      param_index <- match(body_name, param_names)
      if (is.na(param_index)) {
        return(NULL)  # Body doesn't reference a parameter
      }

      # Check we have the right number of arguments
      if (length(compiled_args) != length(param_names)) {
        return(NULL)
      }

      # Return the corresponding argument
      compiled_args[[param_index]]
    },

    # Self-tail-call optimization: check if body has self-tail-calls (Arl AST level)
    has_self_tail_calls = function(body_exprs, self_name) {
      if (length(body_exprs) == 0) return(FALSE)
      # Only the last body expression is in tail position
      private$expr_has_self_tail_call(body_exprs[[length(body_exprs)]], self_name)
    },

    # Recursive walk of tail positions in Arl AST to find self-calls
    expr_has_self_tail_call = function(expr, self_name) {
      if (!is.call(expr) || length(expr) == 0) return(FALSE)
      op <- expr[[1]]
      if (!is.symbol(op)) {
        # IIFE: ((lambda (params...) body...) args...)
        if (private$is_iife(expr)) {
          iife_body <- as.list(expr[[1]])[-(1:2)]
          if (length(iife_body) > 0) {
            return(private$expr_has_self_tail_call(iife_body[[length(iife_body)]], self_name))
          }
        }
        return(FALSE)
      }
      op_name <- as.character(op)
      # Direct call to self_name in tail position
      if (identical(op_name, self_name)) return(TRUE)
      # (if test then else) -> recurse into both branches
      if (identical(op_name, "if")) {
        if (length(expr) >= 3 && private$expr_has_self_tail_call(expr[[3]], self_name)) return(TRUE)
        if (length(expr) >= 4 && private$expr_has_self_tail_call(expr[[4]], self_name)) return(TRUE)
        return(FALSE)
      }
      # (begin e1 ... en) -> recurse into last expression
      if (identical(op_name, "begin")) {
        if (length(expr) <= 1) return(FALSE)
        return(private$expr_has_self_tail_call(expr[[length(expr)]], self_name))
      }
      # Don't descend into lambda, quote, quasiquote
      if (op_name %in% c("lambda", "quote", "quasiquote")) return(FALSE)
      FALSE
    },

    # Compile an expression in tail position (for TCO)
    compile_tail_position = function(expr, self_name, param_names,
                                     rest_param_name = NULL) {
      if (is.call(expr) && length(expr) > 0 && is.symbol(expr[[1]])) {
        op_name <- as.character(expr[[1]])
        if (identical(op_name, self_name)) {
          return(private$compile_self_tail_call(expr, param_names,
                                                rest_param_name = rest_param_name))
        }
        if (identical(op_name, "if")) {
          return(private$compile_tail_if(expr, self_name, param_names,
                                         rest_param_name = rest_param_name))
        }
        if (identical(op_name, "begin")) {
          return(private$compile_tail_begin(expr, self_name, param_names,
                                            rest_param_name = rest_param_name))
        }
      } else if (is.call(expr) && length(expr) > 0 && private$is_iife(expr)) {
        result <- private$compile_tail_iife(expr, self_name, param_names,
                                            rest_param_name = rest_param_name)
        if (!is.null(result)) return(result)
        # Fall through to normal compilation if compile_tail_iife returns NULL
      }
      # Non-tail-call: compile normally and wrap in return()
      compiled <- private$compile_impl(expr)
      if (is.null(compiled)) return(NULL)
      private$src_inherit(as.call(list(quote(return), compiled)), expr)
    },

    # Compile a self-tail-call: (name arg1 arg2 ...) -> param reassignment
    compile_self_tail_call = function(expr, param_names,
                                      rest_param_name = NULL) {
      bail <- function() {
        compiled <- private$compile_impl(expr)
        if (is.null(compiled)) return(NULL)
        private$src_inherit(as.call(list(quote(return), compiled)), expr)
      }
      n_params <- length(param_names)
      # Unified arg collection: walk arg list collecting positional and keyword args
      positional <- list()
      keywords <- list()  # named list: keyword_name -> raw_expr
      i <- 2L
      while (i <= length(expr)) {
        arg_expr <- expr[[i]]
        if (inherits(arg_expr, "arl_keyword")) {
          if (i + 1L > length(expr)) return(bail())
          kw_name <- as.character(arg_expr)
          if (!(kw_name %in% param_names)) return(bail())  # Unknown keyword
          keywords[[kw_name]] <- expr[[i + 1L]]
          i <- i + 2L
        } else {
          positional <- c(positional, list(arg_expr))
          i <- i + 1L
        }
      }
      # Build compiled_args vector indexed by named param position
      compiled_args <- vector("list", n_params)
      filled <- logical(n_params)
      # First, assign keyword args to their param slots
      for (kw_name in names(keywords)) {
        slot <- match(kw_name, param_names)
        if (is.na(slot) || filled[slot]) return(bail())  # Duplicate or unknown
        compiled_args[[slot]] <- private$compile_impl(keywords[[kw_name]])
        if (is.null(compiled_args[[slot]])) return(NULL)
        filled[slot] <- TRUE
      }
      # Then fill positional args into unfilled slots left-to-right
      pos_idx <- 1L
      for (j in seq_len(n_params)) {
        if (!filled[j]) {
          if (pos_idx > length(positional)) {
            # Not enough positional args and no rest param to absorb extras
            if (is.null(rest_param_name)) return(bail())
            # With rest param, unfilled named slots mean not enough args
            return(bail())
          }
          compiled_args[[j]] <- private$compile_impl(positional[[pos_idx]])
          if (is.null(compiled_args[[j]])) return(NULL)
          filled[j] <- TRUE
          pos_idx <- pos_idx + 1L
        }
      }
      # Handle remaining positional args
      remaining_positional <- if (pos_idx <= length(positional)) positional[pos_idx:length(positional)] else list()
      if (is.null(rest_param_name)) {
        # No rest param: no extra args allowed
        if (length(remaining_positional) > 0L) return(bail())
      }
      # Compile rest args if applicable
      compiled_rest <- NULL
      if (!is.null(rest_param_name)) {
        if (length(remaining_positional) > 0L) {
          rest_parts <- vector("list", length(remaining_positional))
          for (ri in seq_along(remaining_positional)) {
            rest_parts[[ri]] <- private$compile_impl(remaining_positional[[ri]])
            if (is.null(rest_parts[[ri]])) return(NULL)
          }
          compiled_rest <- as.call(c(list(quote(list)), rest_parts))
        } else {
          compiled_rest <- quote(list())
        }
      }
      # Identify which named params change
      changed <- integer(0)
      for (j in seq_len(n_params)) {
        ca <- compiled_args[[j]]
        if (is.symbol(ca) && identical(as.character(ca), param_names[j])) {
          next  # Unchanged
        }
        changed <- c(changed, j)
      }
      # Check if rest param changes
      rest_changed <- FALSE
      if (!is.null(rest_param_name) && !is.null(compiled_rest)) {
        if (!(is.symbol(compiled_rest) && identical(as.character(compiled_rest), rest_param_name))) {
          rest_changed <- TRUE
        }
      }
      if (length(changed) == 0L && !rest_changed) {
        # No params change: just continue the loop
        return(quote(next))
      }
      if (length(changed) == 1L && !rest_changed) {
        # Single param change: direct assignment, no temp needed
        j <- changed[1]
        return(private$src_inherit(
          as.call(list(quote(`<-`), as.symbol(param_names[j]), compiled_args[[j]])), expr))
      }
      if (length(changed) == 0L && rest_changed) {
        # Only rest param changes
        return(private$src_inherit(
          as.call(list(quote(`<-`), as.symbol(rest_param_name), compiled_rest)), expr))
      }
      # Multiple params change (or named + rest): use temps to avoid order-dependent bugs
      all_names <- param_names[changed]
      all_compiled <- compiled_args[changed]
      if (rest_changed) {
        all_names <- c(all_names, rest_param_name)
        all_compiled <- c(all_compiled, list(compiled_rest))
      }
      if (length(all_names) == 1L) {
        return(private$src_inherit(
          as.call(list(quote(`<-`), as.symbol(all_names[1]), all_compiled[[1]])), expr))
      }
      stmts <- vector("list", 2L * length(all_names))
      idx <- 1L
      for (k in seq_along(all_names)) {
        tmp_name <- paste0(".__tco_", all_names[k])
        stmts[[idx]] <- as.call(list(quote(`<-`), as.symbol(tmp_name), all_compiled[[k]]))
        idx <- idx + 1L
      }
      for (k in seq_along(all_names)) {
        tmp_name <- paste0(".__tco_", all_names[k])
        stmts[[idx]] <- as.call(list(quote(`<-`), as.symbol(all_names[k]), as.symbol(tmp_name)))
        idx <- idx + 1L
      }
      private$src_inherit(as.call(c(list(quote(`{`)), stmts)), expr)
    },

    # Compile if in tail position: both branches get tail-position treatment
    compile_tail_if = function(expr, self_name, param_names,
                               rest_param_name = NULL) {
      if (length(expr) < 3 || length(expr) > 4) {
        return(private$fail("if requires 2 or 3 arguments: (if test then [else])"))
      }
      test <- private$compile_impl(expr[[2]])
      if (is.null(test)) {
        return(private$fail("if test could not be compiled"))
      }
      # Dead code elimination: if test is a compile-time constant
      if (isTRUE(self$enable_dead_code_elim)) {
        constant_test <- private$eval_constant_test(test)
        if (!is.null(constant_test)) {
          if (isTRUE(constant_test)) {
            return(private$compile_tail_position(expr[[3]], self_name, param_names,
                                                 rest_param_name = rest_param_name))
          } else {
            if (length(expr) == 4) {
              return(private$compile_tail_position(expr[[4]], self_name, param_names,
                                                   rest_param_name = rest_param_name))
            } else {
              return(as.call(list(quote(return), private$compiled_nil())))
            }
          }
        }
      }
      then_expr <- private$compile_tail_position(expr[[3]], self_name, param_names,
                                                  rest_param_name = rest_param_name)
      if (is.null(then_expr)) {
        return(private$fail("if then-branch could not be compiled"))
      }
      then_expr <- private$wrap_branch_coverage(then_expr, expr[[3]])
      test_pred <- if (isTRUE(self$enable_truthiness_opt) && private$returns_r_logical(test)) {
        test
      } else {
        as.call(list(as.symbol(".__true_p"), test))
      }
      if (length(expr) == 4) {
        else_expr <- private$compile_tail_position(expr[[4]], self_name, param_names,
                                                    rest_param_name = rest_param_name)
        if (is.null(else_expr)) {
          return(private$fail("if else-branch could not be compiled"))
        }
        else_expr <- private$wrap_branch_coverage(else_expr, expr[[4]])
        private$src_inherit(as.call(list(quote(`if`), test_pred, then_expr, else_expr)), expr)
      } else {
        private$src_inherit(
          as.call(list(quote(`if`), test_pred, then_expr, as.call(list(quote(return), private$compiled_nil())))),
          expr)
      }
    },

    # Compile begin in tail position: last expression gets tail-position treatment
    compile_tail_begin = function(expr, self_name, param_names,
                                  rest_param_name = NULL) {
      if (length(expr) <= 1) {
        return(as.call(list(quote(return), quote(invisible(NULL)))))
      }
      parts <- as.list(expr)[-1]
      if (length(parts) == 1) {
        return(private$compile_tail_position(parts[[1]], self_name, param_names,
                                             rest_param_name = rest_param_name))
      }
      compiled <- vector("list", length(parts))
      for (i in seq_len(length(parts) - 1L)) {
        compiled[[i]] <- private$compile_impl(parts[[i]])
        if (is.null(compiled[[i]])) return(NULL)
      }
      compiled[[length(parts)]] <- private$compile_tail_position(
        parts[[length(parts)]], self_name, param_names,
        rest_param_name = rest_param_name
      )
      if (is.null(compiled[[length(parts)]])) return(NULL)
      private$src_inherit(as.call(c(list(quote(`{`)), compiled)), expr)
    },

    # Compile an IIFE in tail position: inline the lambda body
    # Returns NULL if the IIFE has complex params (bail to normal compilation)
    compile_tail_iife = function(expr, self_name, param_names,
                                 rest_param_name = NULL) {
      lambda_expr <- expr[[1]]
      iife_params <- private$lambda_params(lambda_expr[[2]])
      if (is.null(iife_params)) return(NULL)  # bail

      # Only handle simple params: no rest, no destructuring, no defaults
      if (iife_params$has_rest || length(iife_params$param_bindings) > 0) return(NULL)
      iife_param_names <- names(iife_params$formals_list)
      for (nm in iife_param_names) {
        if (!identical(iife_params$formals_list[[nm]], quote(expr = ))) return(NULL)  # has default
      }

      # Collect args — bail on keywords
      iife_args <- list()
      i <- 2L
      while (i <= length(expr)) {
        if (inherits(expr[[i]], "arl_keyword")) return(NULL)
        iife_args <- c(iife_args, list(expr[[i]]))
        i <- i + 1L
      }
      if (length(iife_args) != length(iife_param_names)) return(NULL)  # wrong count

      # Build block: param assignments + body with tail-position treatment
      stmts <- list()
      for (j in seq_along(iife_args)) {
        compiled_arg <- private$compile_impl(iife_args[[j]])
        if (is.null(compiled_arg)) return(NULL)
        stmts[[length(stmts) + 1L]] <- as.call(list(quote(`<-`), as.symbol(iife_param_names[j]), compiled_arg))
      }

      iife_body <- as.list(lambda_expr)[-(1:2)]
      if (length(iife_body) == 0) {
        return(as.call(list(quote(return), private$compiled_nil())))
      }

      # Non-last body exprs: compile normally
      for (i in seq_len(max(0, length(iife_body) - 1))) {
        compiled <- private$compile_impl(iife_body[[i]])
        if (is.null(compiled)) return(NULL)
        stmts[[length(stmts) + 1L]] <- compiled
      }

      # Last body expr: compile in tail position (recurses for nested IIFEs)
      last <- private$compile_tail_position(
        iife_body[[length(iife_body)]], self_name, param_names,
        rest_param_name = rest_param_name
      )
      if (is.null(last)) return(NULL)
      stmts[[length(stmts) + 1L]] <- last

      private$src_inherit(as.call(c(list(quote(`{`)), stmts)), expr)
    },

    # Check if an expression is an IIFE: ((lambda ...) args...)
    is_iife = function(expr) {
      is.call(expr) && length(expr) > 0 &&
        is.call(expr[[1]]) && length(expr[[1]]) >= 3 &&
        is.symbol(expr[[1]][[1]]) &&
        identical(as.character(expr[[1]][[1]]), "lambda")
    },

    # Wrap a compiled branch expression with a coverage tracking call.
    # Used by compile_if / compile_tail_if to track which branch was taken.
    wrap_branch_coverage = function(compiled_expr, source_expr) {
      if (is.null(self$context$coverage_tracker)) return(compiled_expr)
      cov_call <- private$make_coverage_call(source_expr)
      if (is.null(cov_call)) return(compiled_expr)
      as.call(list(quote(`{`), cov_call, compiled_expr))
    },

    # Build a statement-level coverage call. For `if` forms, narrow to just the
    # test line since branches are tracked separately by wrap_branch_coverage.
    make_coverage_call_for_stmt = function(src_expr) {
      if (is.null(self$context$coverage_tracker)) return(NULL)
      if (should_narrow_coverage(src_expr)) {
        src <- private$src_get(src_expr)
        if (is.null(src) || is.null(src$file) || is.null(src$start_line)) return(NULL)
        self$context$coverage_tracker$register_coverable(src$file, src$start_line, src$start_line)
        return(as.call(list(
          as.symbol(".__coverage_track"),
          src$file,
          src$start_line,
          src$start_line
        )))
      }
      private$make_coverage_call(src_expr)
    },

    # Build a .__coverage_track(file, start, end) call for the given source expression.
    # Returns NULL if coverage is disabled or no source info.
    make_coverage_call = function(src_expr) {
      if (is.null(self$context$coverage_tracker)) return(NULL)
      src <- private$src_get(src_expr)
      if (is.null(src) || is.null(src$file) || is.null(src$start_line) || is.null(src$end_line)) {
        return(NULL)
      }
      self$context$coverage_tracker$register_coverable(src$file, src$start_line, src$end_line)
      as.call(list(
        as.symbol(".__coverage_track"),
        src$file,
        src$start_line,
        src$end_line
      ))
    },

    # Interleave coverage calls before each compiled body statement.
    # body_exprs: original Arl AST expressions (for source info)
    # compiled_body: compiled R expressions (same length as body_exprs)
    # Returns new list with coverage calls interleaved.
    interleave_coverage = function(body_exprs, compiled_body) {
      if (is.null(self$context$coverage_tracker)) return(compiled_body)
      result <- vector("list", length(compiled_body) * 2L)
      idx <- 1L
      for (i in seq_along(compiled_body)) {
        cov_call <- private$make_coverage_call_for_stmt(body_exprs[[i]])
        if (!is.null(cov_call)) {
          result[[idx]] <- cov_call
          idx <- idx + 1L
        }
        result[[idx]] <- compiled_body[[i]]
        idx <- idx + 1L
      }
      result[seq_len(idx - 1L)]
    },

    # Check if a compiled expression is a "simple value" that doesn't need a temp
    # Simple values: literals (numeric, logical, string, NULL) and symbols
    # Complex values: function calls, blocks, etc.
    is_simple_value = function(expr) {
      # Literals: numeric, logical, character, NULL
      if (is.numeric(expr) || is.logical(expr) || is.character(expr) || is.null(expr)) {
        return(TRUE)
      }
      # Symbols (variable references) - safe because R uses lazy evaluation
      if (is.symbol(expr)) {
        return(TRUE)
      }
      # Everything else (calls, etc.) needs a temp to avoid double evaluation
      FALSE
    },
    # Extract a name as a string from a symbol or length-1 character.
    # Returns NULL if the value is neither.
    as_name_string = function(x) {
      if (is.symbol(x)) return(as.character(x))
      if (is.character(x) && length(x) == 1L) return(x)
      NULL
    },
    # Shared validation + compilation for define and set!.
    # Returns the compiled value expression, or NULL on failure.
    compile_assignment_value = function(expr, mode) {
      name <- expr[[2]]
      if (is.symbol(name) && startsWith(as.character(name), ".__")) {
        return(private$fail(sprintf(
          "%s cannot bind reserved name '%s' (names starting with '.__' are internal)",
          mode, as.character(name)
        )))
      }
      val_expr <- expr[[3]]
      self_name <- NULL
      if (is.symbol(name) && is.call(val_expr) && length(val_expr) >= 3 &&
          is.symbol(val_expr[[1]]) && identical(as.character(val_expr[[1]]), "lambda")) {
        self_name <- as.character(name)
      }
      self$nesting_depth <- self$nesting_depth + 1L
      on.exit(self$nesting_depth <- self$nesting_depth - 1L, add = TRUE)
      val <- if (!is.null(self_name)) {
        private$compile_lambda(val_expr, self_name = self_name)
      } else {
        private$compile_impl(expr[[3]])
      }
      if (is.null(val)) {
        return(private$fail(paste0(mode, " value could not be compiled")))
      }
      val
    },
    # Build a doc_list from an annotation record, or return NULL if empty.
    build_doc_list = function(ann) {
      doc_list <- list()
      for (field in c("description", "signature", "examples", "assert", "seealso", "note")) {
        val <- ann[[field]]
        if (!is.null(val) && nchar(val) > 0) doc_list[[field]] <- val
      }
      if (length(ann$params) > 0) {
        doc_list$arguments <- paste(
          vapply(ann$params, function(p) paste0(p$name, " \u2014 ", p$description), character(1)),
          collapse = "\n"
        )
      }
      if (isTRUE(ann$internal)) doc_list$internal <- TRUE
      if (isTRUE(ann$noeval)) doc_list$noeval <- TRUE
      if (length(doc_list) == 0) NULL else doc_list
    }
  )
)
