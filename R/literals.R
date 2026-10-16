#' Simplify a Boolean expression with a BDD
#'
#' Builds a reduced ordered binary decision diagram (ROBDD) via CUDD from the
#' provided expression and reports which variables remain in the diagram after
#' reduction, together with a factored expression for the reduced diagram.
#'
#' Allowed operators are `!` (negation), `&` (and), `|` (or) and `^` (XOR).
#' Parentheses can be used for grouping. Variable names are any non-empty
#' tokens without whitespace or operator characters. `true` and `false`
#' (case insensitive) are reserved constants. Precedence is `!`, `&`, `^`, `|`.
#'
#' @param expr A character vector of length 1 containing a boolean expression in C++ syntax.
#'
#' @details
#' The reduced expression uses the original variable names and the same C++
#' syntax as the input. It represents the same boolean function, but is not
#' guaranteed to be the shortest expression. Rendering can expand a compact BDD
#' into a larger string.
#'
#' @return A list with four elements:
#' * `constant`: logical scalar. `TRUE` or `FALSE` when the expression reduces to
#'   a constant, otherwise `NA`.
#' * `variables`: sorted remaining variable names (empty when
#'   `constant` is set).
#' * `initial_variables`: sorted variable names from the original expression.
#' * `reduced_expression`: character scalar representing the reduced BDD using
#'   `!`, `&` and `|`, or `true`/`false` for constants.
#'
#' @examples
#' bdd_literals("x & !x")
#' bdd_literals("(y & x) | (y & !x)")
#' bdd_literals("(x & y) | (x & !y)")$reduced_expression
#'
#' @export
bdd_literals <- function(expr) {
  return(.Call(c_bdd_remaining_literals, expr))
}

#' Check expression for redundant variables
#'
#' Evaluates the given expression via `bdd_literals()` and returns `TRUE` when
#' at least one input variable is absent from the reduced BDD. Constant inputs
#' without variables return `FALSE`.
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

  any(!result$initial_variables %in% result$variables)
}
