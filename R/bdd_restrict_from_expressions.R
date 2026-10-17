#' Simplify R conditions and if/else chains
#'
#' Shared entry point for callers such as lintr. Converts R conditions to BDDs,
#' applies ordering constraints and earlier false conditions, and returns
#' readable R replacements. Optional Rwbo synthesis searches for cheaper
#' formulas with AND, OR, XOR and NOT.
#'
#' @param exprs A quoted R condition, or a nonempty list, expression vector or
#'   character vector of conditions in source order. Strings are parsed with
#'   [str2lang()]; expressions are never evaluated. Append `TRUE` for a final
#'   `else` branch.
#' @inheritParams bdd_literals
#'
#' @details
#' A single condition uses only the ordering constraints. For branch `i`, the
#' context also requires all preceding original conditions to be false. A
#' replacement is equivalent only within its context. Solver replacements do
#' not alter the contexts used for later branches.
#'
#' Comparisons assume a total order and conditions use classical Boolean logic.
#' The caller decides whether this model applies to the R code, including
#' missing values, scalar conditions, and side effects. No code eligibility
#' analysis is performed here.
#'
#' @return A list with elements:
#' * `expressions`: logical expressions translated to C++ syntax.
#' * `placeholders`: mapping of placeholder names to original expression strings.
#' * `order_variables`: mapping of comparison variable tokens to their names.
#' * `chain`: result of [bdd_restrict_chain()] with ordering constraints applied.
#'   Each `reduced_expression` is translated back to readable R syntax with
#'   [cpp_logic_to_r()], including the original operands and comparison
#'   operators such as `>`, `>=` and `<=`. The corresponding
#'   `reduced_cpp_expression` retains the raw string with C++ placeholders.
#'   The reduced expressions are equivalent to their inputs within the ordering
#'   constraints and the negations of all earlier expressions in the chain.
#'   `context_reachable` reports whether the context is satisfiable;
#'   `reachable` reports whether context AND condition is satisfiable. Thus a
#'   reachable context with a false condition differs from an unreachable
#'   context. Each entry has an `optimization` diagnostic, including when RWBO
#'   is disabled; see [bdd_literals()] for its fields. Operator counts and costs
#'   refer to the Boolean formula before conversion back to R comparisons.
#'
#' @examples
#' bdd_restrict_from_expressions(quote((x && y) || (x && !y)))$chain
#' bdd_restrict_from_expressions(list(quote(a > b), quote(a >= b), TRUE))$chain
#' # Optional synthesis (requires Rwbo):
#' # bdd_restrict_from_expressions("(a && !b) || (!a && b)", use_rwbo = 6L)
#'
#' @export
bdd_restrict_from_expressions <- function(exprs, use_rwbo = 0L, rwbo_control = list()) {
  if (is.language(exprs) && !is.expression(exprs) ||
      is.logical(exprs) && length(exprs) == 1L) {
    exprs <- list(exprs)
  } else if (is.expression(exprs) || is.character(exprs)) {
    exprs <- as.list(exprs)
  }
  if (!is.list(exprs) || !length(exprs)) {
    stop("'exprs' must contain at least one quoted condition or expression string.", call. = FALSE)
  }
  converted <- cpp_logic_from_expressions(exprs)
  constraints <- order_constraints(names(converted$order_variables))

  chain <- bdd_restrict_chain(
    converted$expressions,
    additional_constraints = constraints,
    use_rwbo = use_rwbo,
    rwbo_control = rwbo_control
  )

  reduced_cpp <- vapply(chain, function(entry) entry$reduced_expression, character(1L))
  reduced_r <- cpp_logic_to_r(
    reduced_cpp, converted$placeholders, converted$order_variables
  )
  for (i in seq_along(chain)) {
    if (is.null(chain[[i]]$optimization)) {
      chain[[i]]$optimization <- .rwbo_diagnostic(
        "disabled", .rwbo_operator_count(reduced_cpp[[i]]),
        cost_before = .rwbo_operator_cost(
          reduced_cpp[[i]], prefer_negations = !isFALSE(rwbo_control$prefer_negations)
        )
      )
    }
    chain[[i]]$reduced_cpp_expression <- reduced_cpp[[i]]
    chain[[i]]$reduced_expression <- reduced_r[[i]]
  }

  return(list(
    expressions = converted$expressions,
    placeholders = converted$placeholders,
    order_variables = converted$order_variables,
    chain = chain
  ))
}
