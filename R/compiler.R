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
    compile = function(expr, env = NULL) {
      if (!is.null(env)) {
        self$current_env <- env
      }
      tryCatch(
        private$compile_impl(expr),
        error = function(e) NULL
      )
    },
    # @description Compile a sequence of expressions to a single R expression (block).
    # @param exprs List of Rye expressions.
    # @return R expression or NULL.
    compile_seq = function(exprs, env = NULL) {
      if (length(exprs) == 0) {
        return(NULL)
      }
      if (!is.null(env)) {
        self$current_env <- env
      }
      if (length(exprs) == 1) {
        return(self$compile(exprs[[1]], env))
      }
      compiled <- lapply(exprs, function(e) private$compile_impl(e))
      if (any(vapply(compiled, is.null, logical(1)))) {
        return(NULL)
      }
      private$src_inherit(as.call(c(list(quote(`{`)), compiled)), exprs[[1]])
    }
  ),
  private = list(
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
        return(quote(NULL))
      }
      # Atoms (self-evaluating)
      if (!is.call(expr) && !is.symbol(expr)) {
        return(expr)
      }
      if (inherits(expr, "rye_keyword")) {
        return(expr)
      }
      # Symbol: .rye_nil is the parser sentinel for #nil; compile to NULL
      if (is.symbol(expr)) {
        if (identical(expr, as.symbol(".rye_nil"))) {
          return(quote(NULL))
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
          quasiquote = private$compile_quasiquote(expr),
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
          and = NULL,   # macro in control.rye; expand before compile
          or = NULL,    # macro in control.rye; expand before compile
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
        return(NULL)
      }
      # Return R (quote x) so that eval(compiled, env) yields the quoted value, not a lookup of x.
      as.call(list(quote(quote), private$strip_src(expr[[2]])))
    },
    compile_quasiquote = function(expr) {
      if (length(expr) != 2) {
        return(NULL)
      }
      private$compile_quasiquote_impl(expr[[2]], 1L)
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
          return(NULL)
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
          return(NULL)
        }
        if (length(template) != 2) {
          return(NULL)
        }
        inner <- private$compile_quasiquote_impl(template[[2]], depth - 1L)
        return(as.call(list(quote(quote), as.call(list(as.symbol("unquote-splicing"), inner)))))
      }
      if (op_char == "quasiquote") {
        if (length(template) != 2) return(NULL)
        inner <- private$compile_quasiquote_impl(template[[2]], depth + 1L)
        return(as.call(list(quote(quote), as.call(list(as.symbol("quasiquote"), inner)))))
      }
      segments <- list()
      for (i in seq_len(length(template))) {
        elem <- template[[i]]
        if (is.call(elem) && length(elem) >= 1L && is.symbol(elem[[1]])) {
          elem_op <- as.character(elem[[1]])
          if (elem_op == "unquote-splicing" && depth == 1L) {
            if (length(elem) != 2) return(NULL)
            compiled <- private$compile_impl(elem[[2]])
            if (is.null(compiled)) return(NULL)
            segments <- c(segments, list(as.call(list(quote(as.list), eval_in_env(compiled)))))
            next
          }
          if (elem_op == "unquote" && depth == 1L) {
            if (length(elem) != 2) return(NULL)
            compiled <- private$compile_impl(elem[[2]])
            if (is.null(compiled)) return(NULL)
            segments <- c(segments, list(as.call(list(quote(list), eval_in_env(compiled)))))
            next
          }
        }
        part <- private$compile_quasiquote_impl(elem, depth)
        segments <- c(segments, list(as.call(list(quote(list), part))))
      }
      if (length(segments) == 0) {
        return(as.call(list(quote(quote), template)))
      }
      as.call(c(list(quote(as.call), as.call(c(list(quote(c)), segments)))))
    },
    compile_if = function(expr) {
      if (length(expr) < 3 || length(expr) > 4) {
        return(NULL)
      }
      test <- private$compile_impl(expr[[2]])
      if (is.null(test)) {
        return(NULL)
      }
      then_expr <- private$compile_impl(expr[[3]])
      if (is.null(then_expr)) {
        return(NULL)
      }
      # Rye: only #f and #nil are false
      test_pred <- as.call(list(as.symbol(".rye_true_p"), test))
      if (length(expr) == 4) {
        else_expr <- private$compile_impl(expr[[4]])
        if (is.null(else_expr)) {
          return(NULL)
        }
        as.call(list(quote(`if`), test_pred, then_expr, else_expr))
      } else {
        as.call(list(quote(`if`), test_pred, then_expr, quote(NULL)))
      }
    },
    compile_begin = function(expr) {
      if (length(expr) <= 1) {
        return(quote(invisible(NULL)))
      }
      parts <- as.list(expr)[-1]
      compiled <- lapply(parts, function(e) private$compile_impl(e))
      if (any(vapply(compiled, is.null, logical(1)))) {
        return(NULL)
      }
      as.call(c(list(quote(`{`)), compiled))
    },
    compile_define = function(expr) {
      if (length(expr) != 3) {
        return(NULL)
      }
      name <- expr[[2]]
      if (is.symbol(name) && identical(as.character(name), self$env_var_name)) {
        return(NULL)
      }
      val <- private$compile_impl(expr[[3]])
      if (is.null(val)) {
        return(NULL)
      }
      # Pass pattern unevaluated for destructuring: symbol -> string; list/call -> .rye_quote(pattern)
      pattern_arg <- if (is.symbol(name)) as.character(name) else as.call(list(as.symbol(".rye_quote"), name))
      assign_call <- as.call(list(
        as.symbol(".rye_assign_pattern"),
        as.symbol(self$env_var_name),
        pattern_arg,
        val,
        "define"
      ))
      # (define x v) returns invisible(v) like the interpreter
      as.call(list(quote(`{`), assign_call, as.call(list(quote(invisible), val))))
    },
    compile_set = function(expr) {
      if (length(expr) != 3) {
        return(NULL)
      }
      name <- expr[[2]]
      if (is.symbol(name) && identical(as.character(name), self$env_var_name)) {
        return(NULL)
      }
      val <- private$compile_impl(expr[[3]])
      if (is.null(val)) {
        return(NULL)
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
        return(NULL)
      }
      args_expr <- expr[[2]]
      params <- private$lambda_params(args_expr)
      if (is.null(params)) {
        return(NULL)
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
        body_exprs <- list(quote(NULL))
      }
      compiled_body <- lapply(body_exprs, function(e) private$compile_impl(e))
      if (any(vapply(compiled_body, is.null, logical(1)))) {
        return(NULL)
      }
      # Prepend .rye_env <- environment() so closure body sees correct current env
      env_bind <- as.call(list(quote(`<-`), as.symbol(self$env_var_name), quote(environment())))
      body_parts <- list(env_bind)
      if (params$has_rest && !is.null(params$rest_param)) {
        body_parts <- c(body_parts, list(as.call(list(quote(`<-`), as.symbol(params$rest_param), quote(list(...))))))
      }
      if (length(params$param_bindings) > 0) {
        for (binding in params$param_bindings) {
          pattern_arg <- as.call(list(as.symbol(".rye_quote"), binding$pattern))
          body_parts <- c(body_parts, list(as.call(list(
            as.symbol(".rye_assign_pattern"),
            as.symbol(self$env_var_name),
            pattern_arg,
            as.symbol(binding$name),
            "define"
          ))))
        }
      }
      if (!is.null(params$rest_param_spec) && identical(params$rest_param_spec$type, "pattern")) {
        pattern_arg <- as.call(list(as.symbol(".rye_quote"), params$rest_param_spec$pattern))
        rest_val <- as.call(list(quote(list), quote(...)))
        body_parts <- c(body_parts, list(as.call(list(
          as.symbol(".rye_assign_pattern"),
          as.symbol(self$env_var_name),
          pattern_arg,
          rest_val,
          "define"
        ))))
      }
      body_parts <- c(body_parts, compiled_body)
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
        return(NULL)
      }
      rest_param <- NULL
      rest_param_spec <- NULL
      if (length(arg_items) > 0) {
        dot_idx <- which(vapply(arg_items, function(a) {
          is.symbol(a) && identical(as.character(a), ".")
        }, logical(1)))
        if (length(dot_idx) == 1) {
          if (dot_idx != length(arg_items) - 1) {
            return(NULL)
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
              return(NULL)
            }
            if (length(rest_list) != 2) {
              return(NULL)
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
          formals_list[[name]] <- quote(expr = )
        } else if (is.call(arg) && length(arg) >= 2 && is.symbol(arg[[1]])) {
          op_char <- as.character(arg[[1]])
          if (op_char %in% c("pattern", "destructure")) {
            if (length(arg) != 2 && length(arg) != 3) {
              return(NULL)
            }
            pattern <- arg[[2]]
            tmp_sym <- if (!is.null(self$context) && !is.null(self$context$macro_expander)) {
              self$context$macro_expander$gensym(".__rye_arg")
            } else {
              as.symbol(paste0(".__rye_arg", as.integer(stats::runif(1, 1, 1e9))))
            }
            tmp_name <- as.character(tmp_sym)
            default <- quote(expr = )
            if (length(arg) == 3) {
              default <- private$compile_impl(arg[[3]])
              if (is.null(default)) {
                return(NULL)
              }
            }
            formals_list[[tmp_name]] <- default
            display <- paste(deparse(pattern, width.cutoff = 500), collapse = " ")
            param_bindings[[length(param_bindings) + 1]] <- list(
              type = "pattern",
              name = tmp_name,
              pattern = pattern,
              display = display
            )
          } else if (length(arg) == 2) {
            name <- op_char
            default <- private$compile_impl(arg[[2]])
            if (is.null(default)) {
              return(NULL)
            }
            formals_list[[name]] <- default
          } else {
            return(NULL)
          }
        } else {
          return(NULL)
        }
      }
      if (!is.null(rest_param) || !is.null(rest_param_spec)) {
        formals_list[["..."]] <- quote(expr = )
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
        return(NULL)
      }
      path <- private$compile_impl(expr[[2]])
      if (is.null(path)) {
        return(NULL)
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
        return(NULL)
      }
      path <- private$compile_impl(expr[[2]])
      if (is.null(path)) {
        return(NULL)
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
        return(NULL)
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
        return(NULL)
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
        return(NULL)
      }
      test <- private$compile_impl(expr[[2]])
      if (is.null(test)) {
        return(NULL)
      }
      body_expr <- if (length(expr) == 3) expr[[3]] else as.call(c(list(quote(begin)), as.list(expr)[-(1:2)]))
      body_compiled <- private$compile_impl(body_expr)
      if (is.null(body_compiled)) {
        return(NULL)
      }
      test_pred <- as.call(list(as.symbol(".rye_true_p"), test))
      as.call(list(quote(`while`), test_pred, body_compiled))
    },
    compile_delay = function(expr) {
      if (length(expr) != 2) {
        return(NULL)
      }
      compiled <- private$compile_impl(expr[[2]])
      if (is.null(compiled)) {
        return(NULL)
      }
      # Compiled delay uses .rye_delay; promise memoization may differ from interpreter.
      as.call(list(
        as.symbol(".rye_delay"),
        as.call(list(quote(quote), compiled)),
        as.symbol(self$env_var_name)
      ))
    },
    compile_defmacro = function(expr) {
      if (length(expr) < 4) {
        return(NULL)
      }
      name <- expr[[2]]
      if (!is.symbol(name)) {
        return(NULL)
      }
      params_expr <- expr[[3]]
      if (r6_isinstance(params_expr, "RyeCons")) {
        parts <- params_expr$parts()
        params_expr <- as.call(c(parts$prefix, list(as.symbol(".")), list(parts$tail)))
      }
      if (is.call(params_expr) && length(params_expr) > 0) {
        for (i in seq_along(params_expr)) {
          item <- params_expr[[i]]
          if (is.symbol(item)) {
            next
          }
          if (is.call(item)) {
            if (length(item) < 1) {
              return(NULL)
            }
            op_char <- if (is.symbol(item[[1]])) as.character(item[[1]]) else NULL
            if (!is.null(op_char) && op_char %in% c("pattern", "destructure")) {
              if (length(item) != 2 && length(item) != 3) {
                return(NULL)
              }
              next
            }
            if (length(item) == 2 && is.symbol(item[[1]])) {
              next
            }
          } else if (is.list(item)) {
            item_list <- item
            if (length(item_list) == 0) {
              return(NULL)
            }
            op_char <- if (is.symbol(item_list[[1]])) as.character(item_list[[1]]) else NULL
            if (!is.null(op_char) && op_char %in% c("pattern", "destructure")) {
              if (length(item_list) != 2 && length(item_list) != 3) {
                return(NULL)
              }
              next
            }
            if (length(item_list) == 2 && is.symbol(item_list[[1]])) {
              next
            }
          }
          return(NULL)
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
      docstring_arg <- if (is.null(docstring)) quote(NULL) else docstring
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
        return(NULL)
      }
      module_name <- expr[[2]]
      name_str <- if (is.symbol(module_name)) as.character(module_name) else if (is.character(module_name) && length(module_name) == 1L) module_name else NULL
      if (is.null(name_str)) {
        return(NULL)
      }
      exports_expr <- expr[[3]]
      if (!is.call(exports_expr) || length(exports_expr) < 1 || !is.symbol(exports_expr[[1]])) {
        return(NULL)
      }
      export_tag <- as.character(exports_expr[[1]])
      export_all <- FALSE
      exports <- character(0)
      if (identical(export_tag, "export")) {
        if (length(exports_expr) > 1) {
          for (i in 2:length(exports_expr)) {
            item <- exports_expr[[i]]
            if (!is.symbol(item)) {
              return(NULL)
            }
            exports <- c(exports, as.character(item))
          }
        }
      } else if (identical(export_tag, "export-all")) {
        if (length(exports_expr) > 1) {
          return(NULL)
        }
        export_all <- TRUE
      } else {
        return(NULL)
      }
      body_exprs <- as.list(expr)[-(1:3)]
      if (length(body_exprs) == 0) {
        compiled_body <- quote(invisible(NULL))
      } else {
        compiled_body <- self$compile_seq(body_exprs)
        if (is.null(compiled_body)) {
          return(NULL)
        }
      }
      src <- private$src_get(expr)
      src_file <- NULL
      if (!is.null(src) && !is.null(src$file) && is.character(src$file) && nzchar(src$file) && grepl("[/\\\\]", src$file)) {
        src_file <- src$file
      }
      as.call(list(
        as.symbol(".rye_module"),
        name_str,
        exports,
        export_all,
        compiled_body,
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
        return(NULL)
      }
      # Pass package and name as strings so lookup works without namespace in env
      pkg_str <- if (is.symbol(expr[[2]])) as.character(expr[[2]]) else if (is.character(expr[[2]]) && length(expr[[2]]) == 1L) expr[[2]] else NULL
      name_str <- if (is.symbol(expr[[3]])) as.character(expr[[3]]) else if (is.character(expr[[3]]) && length(expr[[3]]) == 1L) expr[[3]] else NULL
      if (is.null(pkg_str) || is.null(name_str)) {
        return(NULL)
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
        return(NULL)
      }
      if (is.symbol(op)) {
        op_char <- as.character(op)
        if (op_char %in% c("$", "[", "[[")) {
          args <- list()
          i <- 2
          while (i <= length(expr)) {
            arg_expr <- expr[[i]]
            if (inherits(arg_expr, "rye_keyword")) {
              if (i + 1 > length(expr)) {
                return(NULL)
              }
              keyword_name <- as.character(arg_expr)
              val <- private$compile_impl(expr[[i + 1]])
              if (is.null(val)) {
                return(NULL)
              }
              args <- c(args, list(val))
              names(args)[length(args)] <- keyword_name
              i <- i + 2
            } else {
              val <- private$compile_impl(arg_expr)
              if (is.null(val)) {
                return(NULL)
              }
              args <- c(args, list(val))
              names(args)[length(args)] <- ""
              i <- i + 1
            }
          }
          return(as.call(list(
            as.symbol(".rye_subscript_call"),
            op_char,
            as.call(c(list(quote(list)), args)),
            as.symbol(self$env_var_name)
          )))
        }
      }
      args <- list()
      i <- 2
      while (i <= length(expr)) {
        arg_expr <- expr[[i]]
        if (inherits(arg_expr, "rye_keyword")) {
          if (i + 1 > length(expr)) {
            return(NULL)
          }
          keyword_name <- as.character(arg_expr)
          val <- private$compile_impl(expr[[i + 1]])
          if (is.null(val)) {
            return(NULL)
          }
          args <- c(args, list(val))
          names(args)[length(args)] <- keyword_name
          i <- i + 2
        } else {
          val <- private$compile_impl(arg_expr)
          if (is.null(val)) {
            return(NULL)
          }
          args <- c(args, list(val))
          names(args)[length(args)] <- ""  # positional; avoid NA so R does not treat as named
          i <- i + 1
        }
      }
      as.call(c(list(op), args))
    }
  )
)
