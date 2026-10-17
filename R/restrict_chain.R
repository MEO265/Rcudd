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
#' @inheritParams bdd_literals
#'
#' @details
#' The reduced diagram agrees with the input wherever the combined context is
#' true. Its behavior outside that context is unspecified. `context_reachable`
#' reports whether the context is satisfiable; `reachable` reports whether the
#' context and original condition can both be true. Contexts always use the
#' original conditions, including when RWBO supplies a cheaper replacement.
#'
#' @return One [bdd_literals()] result per input, in input order.
#'   `initial_variables` refers to that branch's input. `reduced_expression`
#'   agrees with it only within the branch's context and need not be shortest.
#'   Each entry also contains `context_reachable` and `reachable`. Positive
#'   `use_rwbo` adds an `optimization` diagnostic. All branches share the
#'   `rwbo_control$time_limit` budget; solver failures retain the BDD result.
#'   `variables` describes the returned formula and can include variables from
#'   the context that were absent from this branch's original input.
#'
#' @examples
#' bdd_restrict_chain("x", c("!x | z", "!z"))
#' bdd_restrict_chain("true", c("x", "!x"))
#' bdd_restrict_chain(c("y | x", "(x | y | z) & w"))[[2L]]$reduced_expression
#'
#' @export
bdd_restrict_chain <- function(exprs, additional_constraints = character(),
                               use_rwbo = 0L, rwbo_control = list()) {
  state <- .rwbo_setup(use_rwbo, rwbo_control)
  on.exit(if (!is.null(state$directory)) unlink(state$directory, recursive = TRUE))
  result <- .Call(c_bdd_restrict_chain, exprs, additional_constraints, state$config)
  lapply(result, .rwbo_finish, state = state)
}
