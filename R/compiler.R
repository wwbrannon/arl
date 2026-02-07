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
      if (!is.call(template)) {
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
      for (i in seq_len(length(template))) {
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
      then_expr <- private$compile_impl(expr[[3]])
      if (is.null(then_expr)) {
        return(private$fail("if then-branch could not be compiled"))
      }
      # Rye: only #f and #nil are false
      test_pred <- as.call(list(as.symbol(".rye_true_p"), test))
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
      val <- private$compile_impl(expr[[3]])
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
    compile_lambda = function(expr) {
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
      # Use early-exit loop with preallocation
      compiled_body <- vector("list", length(body_exprs))
      for (i in seq_along(body_exprs)) {
        compiled_body[[i]] <- private$compile_impl(body_exprs[[i]])
        if (is.null(compiled_body[[i]])) {
          return(private$fail("lambda body could not be compiled"))  # Early exit on failure
        }
      }
      # Prepend .rye_env <- environment() so closure body sees correct current env
      env_bind <- as.call(list(quote(`<-`), as.symbol(self$env_var_name), quote(environment())))
      # Preallocate body_parts: 1 (env_bind) + has_rest + param_bindings + rest_param_spec + compiled_body
      size <- 1 +
              as.integer(params$has_rest && !is.null(params$rest_param)) +
              length(params$param_bindings) +
              as.integer(!is.null(params$rest_param_spec) && identical(params$rest_param_spec$type, "pattern")) +
              length(compiled_body)
      body_parts <- vector("list", size)
      idx <- 1
      body_parts[[idx]] <- env_bind
      idx <- idx + 1
      if (params$has_rest && !is.null(params$rest_param)) {
        body_parts[[idx]] <- as.call(list(quote(`<-`), as.symbol(params$rest_param), quote(list(...))))
        idx <- idx + 1
      }
      if (length(params$param_bindings) > 0) {
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
      if (!is.null(params$rest_param_spec) && identical(params$rest_param_spec$type, "pattern")) {
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
      # Preallocate args list
      args <- vector("list", length(expr) - 1)
      for (i in 2:length(expr)) {
        compiled <- private$compile_impl(expr[[i]])
        if (is.null(compiled)) {
          return(private$fail("and argument could not be compiled"))
        }
        args[[i - 1]] <- compiled
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
        tmp_sym <- gensym_tmp()
        as.call(c(list(quote(`{`)),
          list(as.call(list(quote(`<-`), tmp_sym, args[[idx]]))),
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
      # Preallocate args list
      args <- vector("list", length(expr) - 1)
      for (i in 2:length(expr)) {
        compiled <- private$compile_impl(expr[[i]])
        if (is.null(compiled)) {
          return(private$fail("or argument could not be compiled"))
        }
        args[[i - 1]] <- compiled
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
        tmp_sym <- gensym_tmp()
        as.call(c(list(quote(`{`)),
          list(as.call(list(quote(`<-`), tmp_sym, args[[idx]]))),
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
    }
  )
)
