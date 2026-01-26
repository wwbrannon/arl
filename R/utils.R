trimws_compat <- function(x, which = c("both", "left", "right"), whitespace = "[ \t\r\n]") {
  which <- match.arg(which)
  left_pattern <- paste0("^", whitespace, "+")
  right_pattern <- paste0(whitespace, "+$")
  if (which == "left") {
    return(sub(left_pattern, "", x))
  }
  if (which == "right") {
    return(sub(right_pattern, "", x))
  }
  sub(left_pattern, "", sub(right_pattern, "", x))
}

trimws_shim <- function(x, which = c("both", "left", "right"), whitespace = "[ \t\r\n]") {
  if (exists("trimws", mode = "function", inherits = TRUE)) {
    return(trimws(x, which = which, whitespace = whitespace))
  }
  trimws_compat(x, which = which, whitespace = whitespace)
}
