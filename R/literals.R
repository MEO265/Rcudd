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
#' @param use_rwbo Numeric variable limit for optional MaxSAT optimization.
#'   `0` disables it, a positive whole number limits the native variable pool,
#'   and `Inf` removes this filter. Logical values are rejected. Independent
#'   resource limits, including the native cap of 20 variables, still apply.
#' @param rwbo_control Named list overriding `max_operators = 8L`,
#'   `max_clauses = 2000000L` or `time_limit = 5L` (whole seconds shared across
#'   the call). The budget is checked between jobs; an active export and model
#'   validation finish. Solver timeouts round up to whole seconds. Operator
#'   bounds may be zero; clause and time limits must be positive whole numbers.
#'   The optional Rwbo package is required only for solver calls.
#'
#' @details
#' The reduced expression uses the original variable names and the same C++
#' syntax as the input. It represents the same boolean function, but is not
#' guaranteed to be the shortest expression. Rendering can expand a compact BDD
#' into a larger string.
#'
#' Optional synthesis uses one unit per binary `&`, `|`, `^` or unary `!` in
#' a formula tree, including repeated subexpressions. It searches for strictly
#' cheaper formulas within the operator bound. Proven minima are skipped;
#' without a context, other formulas without repeated variables are also
#' skipped as a heuristic. Resource limits, missing Rwbo and solver failures
#' retain the BDD expression. Heuristic and resource skips do not claim a minimum.
#'
#' @return A list with these elements:
#' * `constant`: logical scalar. `TRUE` or `FALSE` when the expression reduces to
#'   a constant, otherwise `NA`.
#' * `variables`: sorted remaining variable names (empty when
#'   `constant` is set).
#' * `initial_variables`: sorted variable names from the original expression.
#' * `reduced_expression`: character scalar representing the reduced BDD using
#'   `!`, `&` and `|`, or `true`/`false` for constants.
#'
#' With `use_rwbo > 0`, a verified improvement may also contain `^`. An
#' `optimization` list is added with:
#'
#' * `status`: `improved`, `minimal`, `not_candidate`, `variable_limit`,
#'   `resource_limit`, `package_unavailable`, `time_limit`, `no_solution`,
#'   `solver_error`, `invalid_output` or `not_improved`. The R-expression
#'   interface also returns this list with status `disabled` when RWBO is off.
#' * `applied`: whether a solver formula replaced the BDD result.
#' * `operator_count_before`, `operator_count_after`: Boolean operator costs
#'   of the BDD expression and the returned formula.
#' * `verified`, `optimal`: validity and minimality established by a checked
#'   model or a direct BDD proof. A verified candidate need not be optimal.
#' * `solver_status`, `message`: solver status and a brief diagnostic, or `NA`.
#'
#' @examples
#' bdd_literals("x & !x")
#' bdd_literals("(y & x) | (y & !x)")
#' bdd_literals("(x & y) | (x & !y)")$reduced_expression
#'
#' @export
bdd_literals <- function(expr, use_rwbo = 0L, rwbo_control = list()) {
  state <- .rwbo_setup(use_rwbo, rwbo_control)
  on.exit(if (!is.null(state$directory)) unlink(state$directory, recursive = TRUE))
  result <- .Call(c_bdd_remaining_literals, expr, state$config)
  .rwbo_finish(result, state)
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
