#' Build a BDD restriction chain from R expressions with ordering constraints
#'
#' Converts one or more R expressions to C++ style logical strings using
#' [cpp_logic_from_expressions()] and evaluates them with [bdd_restrict_chain()],
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
#'
#' @examples
#' bdd_restrict_from_expressions(list(quote(x || y < z || y >= z)))
#'
#' @export
bdd_restrict_from_expressions <- function(exprs) {
  converted <- cpp_logic_from_expressions(exprs)
  constraints <- order_constraints(names(converted$order_variables))

  chain <- bdd_restrict_chain(
    converted$expressions,
    additional_constraints = constraints
  )

  return(list(
    expressions = converted$expressions,
    placeholders = converted$placeholders,
    order_variables = converted$order_variables,
    chain = chain
  ))
}
