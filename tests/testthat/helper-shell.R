## The text shQuote() quoted: sh wraps it in single quotes; cmd (the default on Windows) wraps it
## in double quotes, with \" for each double quote inside.
.unquoteShell <- function(x, type = if (.Platform$OS.type == "windows") "cmd" else "sh") {
  if (type == "cmd") {
    gsub('\\"', '"', sub('^"(.*)"$', "\\1", x), fixed = TRUE)
  } else {
    sub("^'(.*)'$", "\\1", x)
  }
}
