#' ROBDD restriction chain
#'
#' Builds reduced ordered binary decision diagrams (ROBDDs) for a sequence of
#' logical expressions. Each diagram is simplified under a shared context formed
#' by the negation of all previous expressions and any additional constraints.
#' Use this to simplify later conditions in an if ... else ... sequence under
#' the assumption that the earlier conditions were false.
#'
#' @param exprs Character vector with at least one logical expression written in
#'   C++ syntax (operators `!`, `&`, `|`, `^`).
#' @param additional_constraints Optional character vector of logical
#'   expressions in C++ syntax. All additional constraints are conjoined once.
#'   For each expression, this conjunction is combined with the negation of all
#'   previous expressions and applied in a single restriction. This preserves
#'   relationships through variables absent from the expression being simplified.
#'
#' @details
#' The reduced diagram agrees with the input wherever the combined context is
#' true. Its behavior outside that context is unspecified; this is a reduction
#' operation, not a check of whether the context itself is satisfiable.
#'
#' When the combined context is contradictory, CUDD returns a constant FALSE,
#' including when the input expression is TRUE. A FALSE result can therefore
#' indicate either an expression that is false throughout its context or an
#' unreachable context.
#'
#' @return One [bdd_literals()] result per input, in input order.
#'   `initial_variables` refers to that branch's input. `reduced_expression`
#'   agrees with it only within the branch's context and need not be shortest.
#'
#' @examples
#' bdd_restrict_chain("x", c("!x | z", "!z"))
#' bdd_restrict_chain("true", c("x", "!x"))
#' bdd_restrict_chain(c("y | x", "(x | y | z) & w"))[[2L]]$reduced_expression
#'
#' @export
bdd_restrict_chain <- function(exprs, additional_constraints = character()) {
  .Call(c_bdd_restrict_chain, exprs, additional_constraints)
}
