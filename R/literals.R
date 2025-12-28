#' Reduce literals of boolean expression
#'
#' Builds a reduced ordered binary decision diagram (ROBDD) via CUDD from the
#' provided expression and reports which literals remain in the diagram after
#' reduction.
#'
#' Allowed operators are `!` (negation), `&` (and), `|` (or) and `^` (XOR).
#' Parentheses can be used for grouping. Variable names are any non-empty
#' strings that are neither operators nor whitespace, which makes the parser
#' reusable from other functions.
#'
#' @param expr A character vector of length 1 containing a boolean expression in C++ syntax.
#'
#' @return A list with three elements:
#' * `constant`: logical scalar. `TRUE` or `FALSE` when the expression reduces to
#'   a constant, otherwise `NA`.
#' * `variables`: character vector with the remaining literal names (empty when
#'   `constant` is set).
#' * `initial_variables`: all literal names that appeared in the original
#'   expression.
#'
#' @examples
#' bdd_literals("x & !x")
#' bdd_literals("(y & x) | (y & !x)")
#'
#' @export
bdd_literals <- function(expr) {
  return(.rcudd_call("c_bdd_remaining_literals", expr))
}

#' Check expression for redundant variables
#'
#' Evaluates the given expression via `bdd_literals()` and returns `TRUE` when
#' at least one variable from the original expression is not required in the
#' reduced ROBDD (including the case where the expression is a constant).
#'
#' @inheritParams bdd_literals
#'
#' @return Logical scalar indicating whether any variable is unnecessary.
#'
#' @examples
#' bdd_has_redundant_variables("x & !x")
#' bdd_has_redundant_variables("(y & x) | (y & !x)")
#'
#' @export
bdd_has_redundant_variables <- function(expr) {
  result <- bdd_literals(expr)

  if (!is.na(result$constant)) {
    return(TRUE)
  }

  initial <- result$initial_variables
  remaining <- result$variables

  return(!all(initial %in% remaining))
}
