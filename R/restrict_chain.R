#' ROBDD restriction chain
#'
#' Builds reduced ordered binary decision diagrams (ROBDDs) for a sequence of
#' logical expressions and progressively restricts the final diagram by the
#' negation of all previous diagrams (e.g. `BDD3.Restrict(!BDD2).Restrict(!BDD1)`).
#' Use this when later expressions must exclude solutions already covered by
#' earlier ones (e.g. if ... else ... ), while optionally adding shared constraints
#' that apply to every diagram (e.g. transitive relations).
#'
#' @param exprs Character vector with at least one logical expression written in
#'   C++ syntax (operators `!`, `&`, `|`, `^`).
#' @param additional_constraints Optional character vector of logical
#'   expressions in C++ syntax. After the usual negation-based restrictions, the
#'   constraints are applied to every diagram via `Restrict` so each result also
#'   satisfies them.
#'
#' @return List with one entry per input expression. Each entry mirrors the
#'   structure of [bdd_literals()]:
#' * `constant`: logical scalar indicating whether the restricted BDD  collapses
#'   to a constant (`TRUE`/`FALSE`) or `NA` when it is non-constant.
#' * `variables`: remaining variable names in the restricted diagram (sorted).
#' * `initial_variables`: variables that appeared in the input expressions (sorted).
#'
#' @export
bdd_restrict_chain <- function(exprs, additional_constraints = character()) {
  return(.Call(c_bdd_restrict_chain, exprs, additional_constraints))
}
