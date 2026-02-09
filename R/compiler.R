# Compiler: Compiles macro-expanded Rye AST to R expressions for single eval().
# Used for the optional compiled evaluation path; falls back to interpreter on failure.
#
# Reserved name: .rye_env is used for the current environment in compiled code.
# (define .rye_env x) and (set! .rye_env x) are not compiled (return NULL).

#' @keywords internal
#' @noRd
Compiler <- R6::R6Class(
  "Compiler",
  public = list(
    context = NULL,
    # Reserved name for current env in compiled code. User code that defines/sets this is not compiled.
    env_var_name = ".rye_env",
    # Env used as parent for compiled closures (so they see .rye_true_p etc.). Set by engine before compile.
    current_env = NULL,
    # When TRUE, invalid forms raise errors instead of returning NULL.
    strict = FALSE,
    # When TRUE, compile quasiquote via macro-expander helper (for macro evaluation).
    macro_eval = FALSE,
    # When FALSE, disable constant folding optimization (for preserving expression structure).
    enable_constant_folding = TRUE,
    # Last compilation error (message) when compile() returns NULL.
    last_error = NULL,
    # @param context EvalContext (for source_tracker).
    initialize = function(context) {
      if (!r6_isinstance(context, "EvalContext")) {
        stop("Compiler requires an EvalContext")
      }
      self$context <- context
    },
    # @description Compile one expression. Returns R expression or NULL (cannot compile).
    # @param expr Macro-expanded Rye expression.
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
    # @param exprs List of Rye expressions.
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
      # Atoms (self-evaluating)
      if (!is.call(expr) && !is.symbol(expr)) {
        return(private$strip_src(expr))
      }
      if (inherits(expr, "rye_keyword")) {
        return(expr)
      }
      # Symbol: .rye_nil is the parser sentinel for #nil; compile to NULL
      if (is.symbol(expr)) {
        if (identical(expr, as.symbol(".rye_nil"))) {
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
          quasiquote = if (isTRUE(self$macro_eval)) private$compile_macro_quasiquote(expr) else private$compile_quasiquote(expr),
          `if` = private$compile_if(expr),
          begin = private$compile_begin(expr),
          define = private$compile_define(expr),
          `set!` = private$compile_set(expr),
          lambda = private$compile_lambda(expr),
          load = private$compile_load(expr),
          run = private$compile_run(expr),
          import = private$compile_import(expr),
          help = private$compile_help(expr),
          `while` = private$compile_while(expr),
          and = private$compile_and(expr),
          or = private$compile_or(expr),
          delay = private$compile_delay(expr),
          defmacro = private$compile_defmacro(expr),
          module = private$compile_module(expr),
          `~` = private$compile_formula(expr),
          `::` = private$compile_package_access(expr),
          `:::` = private$compile_package_access(expr),
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
        as.symbol(".rye_macro_quasiquote"),
        as.call(list(quote(quote), private$strip_src(expr[[2]]))),
        as.symbol(self$env_var_name)
      ))
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
          # Nested quasiquote increases depth
          if (op_char == "quasiquote") {
            return(has_unquotes(tmpl[[2]], d + 1L))
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
        return(as.call(list(quote(quote), as.call(list(as.symbol("unquote"), inner)))))
      }
      if (op_char == "unquote-splicing") {
        if (depth == 1L) {
          return(private$fail("unquote-splicing can only appear in list context"))
        }
        if (length(template) != 2) {
          return(private$fail("unquote-splicing requires exactly 1 argument"))
        }
        inner <- private$compile_quasiquote_impl(template[[2]], depth - 1L)
        return(as.call(list(quote(quote), as.call(list(as.symbol("unquote-splicing"), inner)))))
      }
      if (op_char == "quasiquote") {
        if (length(template) != 2) return(private$fail("quasiquote requires exactly 1 argument"))
        inner <- private$compile_quasiquote_impl(template[[2]], depth + 1L)
        return(as.call(list(quote(quote), as.call(list(as.symbol("quasiquote"), inner)))))
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
      test <- private$compile_impl(expr[[2]])
      if (is.null(test)) {
        return(private$fail("if test could not be compiled"))
      }

      # Dead code elimination: if test is a compile-time constant, return only the taken branch
      constant_test <- private$eval_constant_test(test)
      if (!is.null(constant_test)) {
        if (isTRUE(constant_test)) {
          # Test is TRUE - return then-branch
          then_expr <- private$compile_impl(expr[[3]])
          if (is.null(then_expr)) {
            return(private$fail("if then-branch could not be compiled"))
          }
          return(then_expr)
        } else {
          # Test is FALSE or NULL - return else-branch (or NULL if no else)
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

      # Non-constant test - compile normally
      then_expr <- private$compile_impl(expr[[3]])
      if (is.null(then_expr)) {
        return(private$fail("if then-branch could not be compiled"))
      }
      # Rye: only #f and #nil are false
      # Skip .rye_true_p wrapper if test is known to return proper R logical
      test_pred <- if (private$returns_r_logical(test)) {
        test
      } else {
        as.call(list(as.symbol(".rye_true_p"), test))
      }

      if (length(expr) == 4) {
        else_expr <- private$compile_impl(expr[[4]])
        if (is.null(else_expr)) {
          return(private$fail("if else-branch could not be compiled"))
        }
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
      if (length(parts) == 1) {
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
      if (is.symbol(name) && identical(as.character(name), self$env_var_name)) {
        return(private$fail("define cannot bind reserved name .rye_env"))
      }
      # Detect (define name (lambda ...)) for self-tail-call optimization
      val_expr <- expr[[3]]
      self_name <- NULL
      if (is.symbol(name) && is.call(val_expr) && length(val_expr) >= 3 &&
          is.symbol(val_expr[[1]]) && identical(as.character(val_expr[[1]]), "lambda")) {
        self_name <- as.character(name)
      }
      val <- if (!is.null(self_name)) {
        private$compile_lambda(val_expr, self_name = self_name)
      } else {
        private$compile_impl(expr[[3]])
      }
      if (is.null(val)) {
        return(private$fail("define value could not be compiled"))
      }
      # Pass pattern unevaluated for destructuring: symbol -> string; list/call -> .rye_quote(pattern)
      pattern_arg <- if (is.symbol(name)) as.character(name) else as.call(list(as.symbol(".rye_quote"), name))
      tmp_sym <- if (!is.null(self$context) && !is.null(self$context$macro_expander)) {
        self$context$macro_expander$gensym(".__rye_define_value")
      } else {
        as.symbol(paste0(".__rye_define_value", as.integer(stats::runif(1, 1, 1e9))))
      }
      assign_tmp <- as.call(list(quote(`<-`), tmp_sym, val))
      assign_call <- as.call(list(
        as.symbol(".rye_assign_pattern"),
        as.symbol(self$env_var_name),
        pattern_arg,
        tmp_sym,
        "define"
      ))
      # (define x v) returns invisible(v) like the interpreter; evaluate v once.
      as.call(list(quote(`{`), assign_tmp, assign_call, as.call(list(quote(invisible), tmp_sym))))
    },
    compile_set = function(expr) {
      if (length(expr) != 3) {
        return(private$fail("set! requires exactly 2 arguments: (set! name value)"))
      }
      name <- expr[[2]]
      if (is.symbol(name) && identical(as.character(name), self$env_var_name)) {
        return(private$fail("set! cannot bind reserved name .rye_env"))
      }
      val <- private$compile_impl(expr[[3]])
      if (is.null(val)) {
        return(private$fail("set! value could not be compiled"))
      }
      # Pass pattern unevaluated for destructuring: symbol -> string; list/call -> .rye_quote(pattern)
      pattern_arg <- if (is.symbol(name)) as.character(name) else as.call(list(as.symbol(".rye_quote"), name))
      as.call(list(
        as.symbol(".rye_assign_pattern"),
        as.symbol(self$env_var_name),
        pattern_arg,
        val,
        "set"
      ))
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
      body_exprs <- if (length(expr) >= 3) {
        as.list(expr)[-(1:2)]
      } else {
        list()
      }
      docstring <- NULL
      if (length(body_exprs) > 0) {
        first_body <- private$strip_src(body_exprs[[1]])
        if (is.character(first_body) && length(first_body) == 1L) {
          docstring <- first_body
          body_exprs <- body_exprs[-1]
        }
      }
      if (length(body_exprs) == 0) {
        body_exprs <- list(private$compiled_nil())
      }
      # Check for self-tail-call optimization opportunity
      use_tco <- FALSE
      param_names <- NULL
      rest_param_name <- NULL
      if (!is.null(self_name)) {
        param_names <- setdiff(names(params$formals_list), "...")
        rest_param_name <- params$rest_param  # NULL if no rest param
        # For pattern rest params, generate a temp name for collection
        if (is.null(rest_param_name) && !is.null(params$rest_param_spec) &&
            identical(params$rest_param_spec$type, "pattern")) {
          rest_param_name <- if (!is.null(self$context) && !is.null(self$context$macro_expander)) {
            as.character(self$context$macro_expander$gensym(".__rye_rest"))
          } else {
            paste0(".__rye_rest", as.integer(stats::runif(1, 1, 1e9)))
          }
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
            pattern_arg <- as.call(list(as.symbol(".rye_quote"), binding$pattern))
            pattern_stmts[[pb_i]] <- as.call(list(
              as.symbol(".rye_assign_pattern"),
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
          pattern_arg <- as.call(list(as.symbol(".rye_quote"), params$rest_param_spec$pattern))
          rest_pattern_stmt <- as.call(list(
            as.symbol(".rye_assign_pattern"),
            as.symbol(self$env_var_name),
            pattern_arg,
            as.symbol(rest_param_name),
            "define"
          ))
          compiled_body <- c(list(rest_pattern_stmt), compiled_body)
        }
        # Wrap compiled body in while(TRUE) { ... }
        # Use while(TRUE) instead of repeat because Rye stdlib defines a `repeat` function
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
      # Prepend .rye_env <- environment() so closure body sees correct current env
      env_bind <- as.call(list(quote(`<-`), as.symbol(self$env_var_name), quote(environment())))
      # When use_tco is TRUE, rest binding and param_bindings are already handled
      # inside/around the TCO loop, so skip them here.
      n_rest <- if (!use_tco && params$has_rest && !is.null(params$rest_param)) 1L else 0L
      n_bindings <- if (!use_tco) length(params$param_bindings) else 0L
      n_rest_spec <- if (!use_tco && !is.null(params$rest_param_spec) && identical(params$rest_param_spec$type, "pattern")) 1L else 0L
      size <- 1L + n_rest + n_bindings + n_rest_spec + length(compiled_body)
      body_parts <- vector("list", size)
      idx <- 1
      body_parts[[idx]] <- env_bind
      idx <- idx + 1
      if (n_rest > 0L) {
        body_parts[[idx]] <- as.call(list(quote(`<-`), as.symbol(params$rest_param), quote(list(...))))
        idx <- idx + 1
      }
      if (n_bindings > 0L) {
        for (binding in params$param_bindings) {
          pattern_arg <- as.call(list(as.symbol(".rye_quote"), binding$pattern))
          body_parts[[idx]] <- as.call(list(
            as.symbol(".rye_assign_pattern"),
            as.symbol(self$env_var_name),
            pattern_arg,
            as.symbol(binding$name),
            "define"
          ))
          idx <- idx + 1
        }
      }
      if (n_rest_spec > 0L) {
        pattern_arg <- as.call(list(as.symbol(".rye_quote"), params$rest_param_spec$pattern))
        rest_val <- as.call(list(quote(list), quote(...)))
        body_parts[[idx]] <- as.call(list(
          as.symbol(".rye_assign_pattern"),
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
      if (is.null(docstring)) {
        return(fn_expr)
      }
      fn_sym <- as.symbol(".__rye_compiled_fn")
      as.call(list(
        quote(`{`),
        as.call(list(quote(`<-`), fn_sym, fn_expr)),
        as.call(list(
          quote(`<-`),
          as.call(list(quote(attr), fn_sym, "rye_doc")),
          list(description = docstring)
        )),
        fn_sym
      ))
    },
    lambda_params = function(args_expr) {
      arg_items <- if (is.null(args_expr)) {
        list()
      } else if (r6_isinstance(args_expr, "RyeCons")) {
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
            tmp_sym <- if (!is.null(self$context) && !is.null(self$context$macro_expander)) {
              self$context$macro_expander$gensym(".__rye_arg")
            } else {
              as.symbol(paste0(".__rye_arg", as.integer(stats::runif(1, 1, 1e9))))
            }
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
    compile_load = function(expr) {
      if (length(expr) != 2) {
        return(private$fail("load requires exactly 1 argument: (load \"path\")"))
      }
      path <- private$compile_impl(expr[[2]])
      if (is.null(path)) {
        return(private$fail("load path could not be compiled"))
      }
      as.call(list(
        as.symbol(".rye_load"),
        path,
        as.symbol(self$env_var_name),
        FALSE
      ))
    },
    compile_run = function(expr) {
      if (length(expr) != 2) {
        return(private$fail("run requires exactly 1 argument: (run \"path\")"))
      }
      path <- private$compile_impl(expr[[2]])
      if (is.null(path)) {
        return(private$fail("run path could not be compiled"))
      }
      as.call(list(
        as.symbol(".rye_load"),
        path,
        as.symbol(self$env_var_name),
        TRUE
      ))
    },
    compile_import = function(expr) {
      if (length(expr) != 2) {
        return(private$fail("import requires exactly 1 argument: (import name)"))
      }
      # Pass argument unevaluated (module name is a symbol/string, not a variable lookup)
      arg_quoted <- as.call(list(quote(quote), expr[[2]]))
      as.call(list(
        as.symbol(".rye_import"),
        arg_quoted,
        as.symbol(self$env_var_name)
      ))
    },
    compile_help = function(expr) {
      if (length(expr) != 2) {
        return(private$fail("help requires exactly 1 argument: (help topic)"))
      }
      topic_quoted <- as.call(list(quote(quote), expr[[2]]))
      as.call(list(
        as.symbol(".rye_help"),
        topic_quoted,
        as.symbol(self$env_var_name)
      ))
    },
    compile_while = function(expr) {
      if (length(expr) < 3) {
        return(private$fail("while requires at least 2 arguments: (while test body...)"))
      }
      test <- private$compile_impl(expr[[2]])
      if (is.null(test)) {
        return(private$fail("while test could not be compiled"))
      }
      body_expr <- if (length(expr) == 3) expr[[3]] else as.call(c(list(quote(begin)), as.list(expr)[-(1:2)]))
      body_compiled <- private$compile_impl(body_expr)
      if (is.null(body_compiled)) {
        return(private$fail("while body could not be compiled"))
      }
      test_pred <- as.call(list(as.symbol(".rye_true_p"), test))
      as.call(list(quote(`while`), test_pred, body_compiled))
    },
    compile_delay = function(expr) {
      if (length(expr) != 2) {
        return(private$fail("delay requires exactly 1 argument"))
      }
      # Disable constant folding to preserve expression structure for promise-expr
      old_folding <- self$enable_constant_folding
      self$enable_constant_folding <- FALSE
      compiled <- private$compile_impl(expr[[2]])
      self$enable_constant_folding <- old_folding

      if (is.null(compiled)) {
        return(private$fail("delay expression could not be compiled"))
      }
      # Compiled delay uses .rye_delay; promise memoization may differ from interpreter.
      as.call(list(
        as.symbol(".rye_delay"),
        as.call(list(quote(quote), compiled)),
        as.symbol(self$env_var_name)
      ))
    },
    compile_and = function(expr) {
      if (length(expr) < 2) {
        return(private$fail("and requires at least 1 argument"))
      }

      # Flatten nested ANDs recursively: (and (and a b) c) → (and a b c)
      # This avoids creating temps for intermediate AND results
      flatten_and <- function(e) {
        result <- list()
        for (i in 2:length(e)) {
          arg <- e[[i]]
          if (is.call(arg) && length(arg) >= 1 && identical(arg[[1]], quote(and))) {
            # Recursively flatten nested AND
            result <- c(result, flatten_and(arg))
          } else {
            result <- c(result, list(arg))
          }
        }
        result
      }
      flat_args <- flatten_and(expr)

      # Compile all flattened arguments
      args <- vector("list", length(flat_args))
      for (i in seq_along(flat_args)) {
        compiled <- private$compile_impl(flat_args[[i]])
        if (is.null(compiled)) {
          return(private$fail("and argument could not be compiled"))
        }
        args[[i]] <- compiled
      }
      gensym_tmp <- function() {
        if (!is.null(self$context) && !is.null(self$context$macro_expander)) {
          return(self$context$macro_expander$gensym(".__rye_tmp"))
        }
        as.symbol(paste0(".__rye_tmp", as.integer(stats::runif(1, 1, 1e9))))
      }
      build <- function(idx) {
        if (idx == length(args)) {
          return(args[[idx]])
        }

        arg <- args[[idx]]

        # Simple values don't need temps - use directly
        if (private$is_simple_value(arg)) {
          return(as.call(list(
            quote(`if`),
            as.call(list(as.symbol(".rye_true_p"), arg)),
            build(idx + 1L),
            arg
          )))
        }

        # Complex expressions need temps to avoid double evaluation
        tmp_sym <- gensym_tmp()
        as.call(c(list(quote(`{`)),
          list(as.call(list(quote(`<-`), tmp_sym, arg))),
          list(as.call(list(
            quote(`if`),
            as.call(list(as.symbol(".rye_true_p"), tmp_sym)),
            build(idx + 1L),
            tmp_sym
          )))
        ))
      }
      build(1L)
    },
    compile_or = function(expr) {
      if (length(expr) < 2) {
        return(private$fail("or requires at least 1 argument"))
      }

      # Flatten nested ORs recursively: (or (or a b) c) → (or a b c)
      # This avoids creating temps for intermediate OR results
      flatten_or <- function(e) {
        result <- list()
        for (i in 2:length(e)) {
          arg <- e[[i]]
          if (is.call(arg) && length(arg) >= 1 && identical(arg[[1]], quote(or))) {
            # Recursively flatten nested OR
            result <- c(result, flatten_or(arg))
          } else {
            result <- c(result, list(arg))
          }
        }
        result
      }
      flat_args <- flatten_or(expr)

      # Compile all flattened arguments
      args <- vector("list", length(flat_args))
      for (i in seq_along(flat_args)) {
        compiled <- private$compile_impl(flat_args[[i]])
        if (is.null(compiled)) {
          return(private$fail("or argument could not be compiled"))
        }
        args[[i]] <- compiled
      }
      gensym_tmp <- function() {
        if (!is.null(self$context) && !is.null(self$context$macro_expander)) {
          return(self$context$macro_expander$gensym(".__rye_tmp"))
        }
        as.symbol(paste0(".__rye_tmp", as.integer(stats::runif(1, 1, 1e9))))
      }
      build <- function(idx) {
        if (idx == length(args)) {
          return(args[[idx]])
        }

        arg <- args[[idx]]

        # Simple values don't need temps - use directly
        if (private$is_simple_value(arg)) {
          return(as.call(list(
            quote(`if`),
            as.call(list(as.symbol(".rye_true_p"), arg)),
            arg,
            build(idx + 1L)
          )))
        }

        # Complex expressions need temps to avoid double evaluation
        tmp_sym <- gensym_tmp()
        as.call(c(list(quote(`{`)),
          list(as.call(list(quote(`<-`), tmp_sym, arg))),
          list(as.call(list(
            quote(`if`),
            as.call(list(as.symbol(".rye_true_p"), tmp_sym)),
            tmp_sym,
            build(idx + 1L)
          )))
        ))
      }
      build(1L)
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
      if (r6_isinstance(params_expr, "RyeCons")) {
        parts <- params_expr$parts()
        params_expr <- as.call(c(parts$prefix, list(as.symbol(".")), list(parts$tail)))
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
      docstring <- NULL
      if (length(body_exprs) > 0) {
        first <- private$strip_src(body_exprs[[1]])
        if (is.character(first) && length(first) == 1L) {
          docstring <- first
          body_exprs <- body_exprs[-1]
        }
      }
      body_quoted <- as.call(list(quote(quote), as.call(c(list(quote(begin)), body_exprs))))
      docstring_arg <- if (is.null(docstring)) private$compiled_nil() else docstring
      as.call(list(
        as.symbol(".rye_defmacro"),
        as.call(list(quote(quote), name)),
        as.call(list(quote(quote), params_expr)),
        body_quoted,
        docstring_arg,
        as.symbol(self$env_var_name)
      ))
    },
    compile_module = function(expr) {
      if (length(expr) < 3) {
        return(private$fail("module requires at least 2 arguments: (module name (export ...) body...)"))
      }
      module_name <- expr[[2]]
      name_str <- if (is.symbol(module_name)) as.character(module_name) else if (is.character(module_name) && length(module_name) == 1L) module_name else NULL
      if (is.null(name_str)) {
        return(private$fail("module name must be a symbol or string"))
      }
      exports_expr <- expr[[3]]
      if (!is.call(exports_expr) || length(exports_expr) < 1 || !is.symbol(exports_expr[[1]])) {
        return(private$fail("module requires an export list: (module name (export ...) body...)"))
      }
      export_tag <- as.character(exports_expr[[1]])
      export_all <- FALSE
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
          return(private$fail("export-all does not take any arguments"))
        }
        export_all <- TRUE
      } else {
        return(private$fail("module requires an export list: (module name (export ...) body...)"))
      }
      body_exprs <- as.list(expr)[-(1:3)]
      body_exprs <- if (length(body_exprs) == 0) list() else body_exprs
      src <- private$src_get(expr)
      src_file <- NULL
      if (!is.null(src) && !is.null(src$file) && is.character(src$file) && nzchar(src$file) && grepl("[/\\\\]", src$file)) {
        src_file <- src$file
      }
      compiled_body_quoted <- as.call(list(quote(quote), body_exprs))
      as.call(list(
        as.symbol(".rye_module"),
        name_str,
        exports,
        export_all,
        compiled_body_quoted,
        src_file,
        as.symbol(self$env_var_name)
      ))
    },
    compile_formula = function(expr) {
      formula_parts <- list(as.symbol("~"))
      if (length(expr) > 1) {
        for (i in 2:length(expr)) {
          formula_parts <- c(formula_parts, list(expr[[i]]))
        }
      }
      as.call(formula_parts)
    },
    compile_package_access = function(expr) {
      if (length(expr) != 3) {
        return(private$fail(sprintf("%s requires exactly 2 arguments: (%s pkg name)", as.character(expr[[1]]), as.character(expr[[1]]))))
      }
      # Pass package and name as strings so lookup works without namespace in env
      pkg_str <- if (is.symbol(expr[[2]])) as.character(expr[[2]]) else if (is.character(expr[[2]]) && length(expr[[2]]) == 1L) expr[[2]] else NULL
      name_str <- if (is.symbol(expr[[3]])) as.character(expr[[3]]) else if (is.character(expr[[3]]) && length(expr[[3]]) == 1L) expr[[3]] else NULL
      if (is.null(pkg_str)) {
        return(private$fail("Package name must be a symbol or string"))
      }
      if (is.null(name_str)) {
        return(private$fail("Function/object name must be a symbol or string"))
      }
      as.call(list(
        as.symbol(".rye_pkg_access"),
        as.character(expr[[1]]),
        pkg_str,
        name_str,
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
          if (inherits(arg_expr, "rye_keyword")) {
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
            as.symbol(".rye_subscript_call"),
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
      strength_reduced <- private$try_strength_reduction(op, args)
      if (!is.null(strength_reduced)) {
        return(strength_reduced)
      }

      # Attempt identity elimination: detect ((lambda (x) x) value) and inline
      # Check the original expression to see if operator is a lambda
      if (is.call(expr[[1]]) && length(expr[[1]]) >= 3) {
        if (is.symbol(expr[[1]][[1]]) && as.character(expr[[1]][[1]]) == "lambda") {
          identity_result <- private$try_identity_elimination(expr[[1]], args)
          if (!is.null(identity_result)) {
            return(identity_result)
          } else {
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

      # Whitelist of pure functions safe to evaluate at compile time
      # These must be deterministic and have no side effects
      pure_functions <- c(
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
      )

      if (!(op_name %in% pure_functions)) {
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
    # These expressions don't need .rye_true_p() wrapper
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
      # It's a language object that needs .rye_true_p() wrapper

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
    # Returns NULL if not constant, TRUE if truthy, FALSE if falsy (according to Rye semantics)
    eval_constant_test = function(test) {
      # Literal TRUE/FALSE
      if (is.logical(test) && length(test) == 1 && !is.na(test)) {
        return(test)  # TRUE or FALSE
      }

      # Literal NULL (compiled as quote(NULL)) - falsy in Rye
      if (is.call(test) && length(test) == 2) {
        if (identical(test[[1]], quote(quote)) && is.null(test[[2]])) {
          return(FALSE)  # NULL is falsy in Rye
        }
      }

      # Symbol TRUE/FALSE/NULL
      if (is.symbol(test)) {
        test_str <- as.character(test)
        if (test_str == "TRUE") return(TRUE)
        if (test_str == "FALSE") return(FALSE)
        if (test_str == "NULL") return(FALSE)  # NULL is falsy in Rye
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
      if (op_name == "*" && length(args) == 2) {
        # Check if second arg is literal 2
        if (is.numeric(args[[2]]) && args[[2]] == 2) {
          # Replace with addition: x + x
          return(as.call(list(quote(`+`), args[[1]], args[[1]])))
        }
        # Also handle (* 2 x)
        if (is.numeric(args[[1]]) && args[[1]] == 2) {
          return(as.call(list(quote(`+`), args[[2]], args[[2]])))
        }
      }

      # Power of 2: (^ x 2) → (* x x)
      if (op_name == "^" && length(args) == 2) {
        # Check if second arg is literal 2
        if (is.numeric(args[[2]]) && args[[2]] == 2) {
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

    # Self-tail-call optimization: check if body has self-tail-calls (Rye AST level)
    has_self_tail_calls = function(body_exprs, self_name) {
      if (length(body_exprs) == 0) return(FALSE)
      # Only the last body expression is in tail position
      private$expr_has_self_tail_call(body_exprs[[length(body_exprs)]], self_name)
    },

    # Recursive walk of tail positions in Rye AST to find self-calls
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
        if (inherits(arg_expr, "rye_keyword")) {
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
      remaining_positional <- positional[seq_len(length(positional))[seq_len(length(positional)) >= pos_idx]]
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
        tmp_name <- paste0(".tco_", all_names[k])
        stmts[[idx]] <- as.call(list(quote(`<-`), as.symbol(tmp_name), all_compiled[[k]]))
        idx <- idx + 1L
      }
      for (k in seq_along(all_names)) {
        tmp_name <- paste0(".tco_", all_names[k])
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
      then_expr <- private$compile_tail_position(expr[[3]], self_name, param_names,
                                                  rest_param_name = rest_param_name)
      if (is.null(then_expr)) {
        return(private$fail("if then-branch could not be compiled"))
      }
      test_pred <- if (private$returns_r_logical(test)) {
        test
      } else {
        as.call(list(as.symbol(".rye_true_p"), test))
      }
      if (length(expr) == 4) {
        else_expr <- private$compile_tail_position(expr[[4]], self_name, param_names,
                                                    rest_param_name = rest_param_name)
        if (is.null(else_expr)) {
          return(private$fail("if else-branch could not be compiled"))
        }
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
        if (inherits(expr[[i]], "rye_keyword")) return(NULL)
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
      # Skip docstring
      if (length(iife_body) > 0) {
        first <- private$strip_src(iife_body[[1]])
        if (is.character(first) && length(first) == 1L) iife_body <- iife_body[-1]
      }
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

    # Build a .rye_coverage_track(file, start, end) call for the given source expression.
    # Returns NULL if coverage is disabled or no source info.
    make_coverage_call = function(src_expr) {
      if (is.null(self$context$coverage_tracker)) return(NULL)
      src <- private$src_get(src_expr)
      if (is.null(src) || is.null(src$file) || is.null(src$start_line) || is.null(src$end_line)) {
        return(NULL)
      }
      as.call(list(
        as.symbol(".rye_coverage_track"),
        src$file,
        src$start_line,
        src$end_line
      ))
    },

    # Interleave coverage calls before each compiled body statement.
    # body_exprs: original Rye AST expressions (for source info)
    # compiled_body: compiled R expressions (same length as body_exprs)
    # Returns new list with coverage calls interleaved.
    interleave_coverage = function(body_exprs, compiled_body) {
      if (is.null(self$context$coverage_tracker)) return(compiled_body)
      result <- vector("list", length(compiled_body) * 2L)
      idx <- 1L
      for (i in seq_along(compiled_body)) {
        cov_call <- private$make_coverage_call(body_exprs[[i]])
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
    }
  )
)
