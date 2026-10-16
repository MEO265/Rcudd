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
#'   expressions in C++ syntax. They are combined with the negated previous
#'   expressions before applying `Restrict`, preserving relationships through
#'   variables absent from the expression being simplified.
#'
#' @details
#' The reduced diagram agrees with the input wherever the combined context is
#' true. Its behavior outside that context is unspecified; this is a reduction
#' operation, not a check of whether the context itself is satisfiable.
#'
#' When the combined context is contradictory, there is no unique reduction.
#' In that case the original sequential behavior is preserved: negated previous
#' diagrams are applied in reverse order, followed by the additional constraints
#' in input order. In particular, a constant TRUE can remain TRUE under
#' contradictory additional constraints.
#'
#' @return List with one entry per input expression. Each entry mirrors the
#'   structure of [bdd_literals()]:
#' * `constant`: logical scalar indicating whether the restricted BDD  collapses
#'   to a constant (`TRUE`/`FALSE`) or `NA` when it is non-constant.
#' * `variables`: remaining variable names in the restricted diagram (sorted).
#' * `initial_variables`: variables that appeared in the input expressions (sorted).
#'
#' @examples
#' bdd_restrict_chain("x", c("!x | z", "!z"))
#' bdd_restrict_chain("true", c("x", "!x"))
#'
#' @export
bdd_restrict_chain <- function(exprs, additional_constraints = character()) {
  .Call(c_bdd_restrict_chain, exprs, additional_constraints)
}
