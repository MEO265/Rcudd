#' @useDynLib Rcudd, .registration=TRUE
NULL

.rcudd_call <- function(symbol, ...) {
  .Call(symbol, ..., PACKAGE = "Rcudd")
}
