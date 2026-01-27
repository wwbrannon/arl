# Help utilities for Rye
rye_help_topics <- list(
  quote = list(
    usage = "(quote expr)",
    description = "Return expr without evaluation."
  ),
  quasiquote = list(
    usage = "(quasiquote expr)",
    description = "Template with selective evaluation via unquote."
  ),
  unquote = list(
    usage = "(unquote expr)",
    description = "Evaluate expr inside quasiquote."
  ),
  `unquote-splicing` = list(
    usage = "(unquote-splicing expr)",
    description = "Splice a list into a quasiquoted list."
  ),
  `if` = list(
    usage = "(if test then [else])",
    description = "Evaluate then or else depending on test."
  ),
  lambda = list(
    usage = "(lambda (args...) body...)",
    description = "Create an anonymous function."
  ),
  define = list(
    usage = "(define name value)",
    description = "Bind name to value in the current environment."
  ),
  `set!` = list(
    usage = "(set! name value)",
    description = "Update an existing binding."
  ),
  begin = list(
    usage = "(begin expr...)",
    description = "Evaluate expressions in sequence, returning the last."
  ),
  load = list(
    usage = "(load \"path\")",
    description = "Load and evaluate a Rye source file."
  ),
  defmacro = list(
    usage = "(defmacro name (params...) body...)",
    description = "Define a macro."
  ),
  `::` = list(
    usage = "(:: pkg name)",
    description = "Access an exported object from an R package."
  ),
  `:::` = list(
    usage = "(::: pkg name)",
    description = "Access a non-exported object from an R package."
  ),
  `~` = list(
    usage = "(~ lhs rhs)",
    description = "Build an R formula without evaluating arguments."
  )
)

rye_help_macro_topics <- list(
  `when` = list(
    usage = "(when test body)",
    description = "Evaluate body when test is truthy."
  ),
  `unless` = list(
    usage = "(unless test body)",
    description = "Evaluate body when test is falsy."
  ),
  `and` = list(
    usage = "(and expr ...)",
    description = "Short-circuit boolean AND."
  ),
  `or` = list(
    usage = "(or expr ...)",
    description = "Short-circuit boolean OR."
  ),
  `cond` = list(
    usage = "(cond (test expr...) ...)",
    description = "Multi-branch conditional."
  ),
  `case` = list(
    usage = "(case key (datum expr...) ...)",
    description = "Branch on key equality."
  ),
  `let` = list(
    usage = "(let ((name value) ...) body...)",
    description = "Bind names to values within body."
  ),
  `let*` = list(
    usage = "(let* ((name value) ...) body...)",
    description = "Sequential let bindings."
  ),
  `letrec` = list(
    usage = "(letrec ((name value) ...) body...)",
    description = "Recursive bindings."
  ),
  `defstruct` = list(
    usage = "(defstruct Name (field ...))",
    description = "Define a struct constructor, predicate, and accessors."
  ),
  `while` = list(
    usage = "(while test body...)",
    description = "Repeat body while test is truthy."
  ),
  `for` = list(
    usage = "(for (var seq) body...)",
    description = "Map body over seq, binding var."
  ),
  `->` = list(
    usage = "(-> value form...)",
    description = "Thread value through forms (first argument)."
  ),
  `->>` = list(
    usage = "(->> value form...)",
    description = "Thread value through forms (last argument)."
  ),
  `try` = list(
    usage = "(try body [catch err body...] [finally body...])",
    description = "Macro wrapper around try* with catch/finally."
  )
)

rye_help_print <- function(topic, doc) {
  lines <- c(paste0("Topic: ", topic))
  if (!is.null(doc$usage) && doc$usage != "") {
    lines <- c(lines, paste0("Usage: ", doc$usage))
  }
  if (!is.null(doc$description) && doc$description != "") {
    lines <- c(lines, paste0("Description: ", doc$description))
  }
  cat(paste(lines, collapse = "\n"), "\n")
}

rye_help <- function(topic, env = parent.frame()) {
  if (!is.character(topic) || length(topic) != 1) {
    stop("help requires a symbol or string")
  }

  doc <- rye_help_topics[[topic]]
  if (!is.null(doc)) {
    rye_help_print(topic, doc)
    return(invisible(NULL))
  }

  macro_doc <- rye_help_macro_topics[[topic]]
  if (!is.null(macro_doc)) {
    rye_help_print(topic, macro_doc)
    return(invisible(NULL))
  }

  if (exists(topic, envir = env, inherits = TRUE)) {
    obj <- get(topic, envir = env, inherits = TRUE)
    obj_doc <- attr(obj, "rye_doc", exact = TRUE)
    if (!is.null(obj_doc)) {
      rye_help_print(topic, obj_doc)
      return(invisible(NULL))
    }
  }

  utils::help(topic, help_type = "text")
  invisible(NULL)
}
