#' @useDynLib Rcudd, .registration=TRUE
NULL

.rcudd_symbols <- new.env(parent = emptyenv())

.rcudd_symbol <- function(name) {
  if (exists(name, envir = .rcudd_symbols, inherits = FALSE)) {
    return(get(name, envir = .rcudd_symbols, inherits = FALSE))
  }
  addr <- getNativeSymbolInfo(name, PACKAGE = "Rcudd")$address
  assign(name, addr, envir = .rcudd_symbols)
  return(addr)
}

.rcudd_call <- function(symbol, ...) {
  if (is.character(symbol)) {
    symbol <- .rcudd_symbol(symbol)
  }
  .Call(symbol, ...)
}

.cudd_check_same_manager <- function(lhs, rhs, op) {
  same_manager <- identical(lhs@manager_ptr, rhs@manager_ptr)
  if (!same_manager) {
    warning(
      sprintf("Objects for %s come from different CuddManager instances.", op),
      call. = FALSE
    )
  }
  return(same_manager)
}
