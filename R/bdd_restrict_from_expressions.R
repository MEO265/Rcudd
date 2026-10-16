#' Build a BDD restriction chain from R expressions with ordering constraints
#'
#' Converts one or more R expressions to C++ style logical strings using
#' [expressions_to_cpp_logic()] and evaluates them with [bdd_restrict_chain()],
#' automatically supplying ordering constraints derived from placeholders for
#' comparison operators.
#'
#' @param exprs List of quoted R expressions or character scalars that can be
#'   parsed with [str2lang()].
#'
#' @return A list with elements:
#' * `expressions`: logical expressions translated to C++ syntax.
#' * `placeholders`: mapping of placeholder names to original expression strings.
#' * `order_variables`: mapping of comparison variable tokens to their names.
#' * `chain`: result of [bdd_restrict_chain()] with ordering constraints applied.
#'   Each `reduced_expression` is translated back to readable R syntax with
#'   [cpp_logic_to_expressions()], including the original operands and comparison
#'   operators such as `>`, `>=` and `<=`. The corresponding
#'   `reduced_cpp_expression` retains the raw string with C++ placeholders.
#'   The reduced expressions are equivalent to their inputs within the ordering
#'   constraints and the negations of all earlier expressions in the chain.
#'
#' @examples
#' bdd_restrict_from_expressions(list(quote(x || y < z || y >= z)))
#' bdd_restrict_from_expressions(list(quote(b < c && a < b && a >= c)))
#' bdd_restrict_from_expressions(list(quote(a > b), quote(a >= b)))$chain
#'
#' @export
bdd_restrict_from_expressions <- function(exprs) {
  converted <- cpp_logic_from_expressions(exprs)
  constraints <- order_constraints(names(converted$order_variables))

  chain <- bdd_restrict_chain(
    converted$expressions,
    additional_constraints = constraints
  )

  reduced_cpp <- vapply(chain, function(entry) entry$reduced_expression, character(1L))
  reduced_r <- cpp_logic_to_expressions(
    reduced_cpp, converted$placeholders, converted$order_variables
  )
  for (i in seq_along(chain)) {
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
