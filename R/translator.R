rye_translate_env <- local({
  env <- NULL
  function() {
    if (is.null(env)) {
      env <- rye_load_stdlib()
      rye_eval(rye_read("(import translator)")[[1]], env)
    }
    env
  }
})

rye_expr_to_r <- function(expr, indent = 0) {
  env <- rye_translate_env()
  translator <- env$`translate-expr`
  translator(expr, indent)
}

rye_translate_str <- function(source) {
  env <- rye_translate_env()
  translator <- env$`translate-str`
  translator(source)
}

rye_translate_file <- function(path) {
  env <- rye_translate_env()
  translator <- env$`translate-file`
  translator(path)
}

#' Translate Rye code to R code
#'
#' Translates Rye source into equivalent R code without evaluating it.
#'
#' @param source Either a file path to a .rye file or a string containing Rye code
#' @param is_file Logical indicating whether source is a file path (default: TRUE if source ends with .rye)
#' @return A character string containing the translated R code
#' @examples
#' rye_translate("(+ 1 2)", is_file = FALSE)
#' #> [1] "1 + 2"
#' @export
rye_translate <- function(source, is_file = NULL) {
  env <- rye_translate_env()
  translator <- env$`translate-source`
  translator(source, is_file)
}
