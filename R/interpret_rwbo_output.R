#' Interpret and verify Rwbo solver output
#'
#' Decodes an operator formula from a complete Open-WBO stdout string and a
#' problem from [bdd_to_wcnf()]. Uses the shared native C++ interpreter.
#'
#' @param problem An `rwbo_formula_problem` returned by [bdd_to_wcnf()]. Its
#'   WCNF file must still be available at `problem$wcnf_file`.
#' @param output Solver stdout as a character scalar or vector of lines.
#'
#' @details
#' The interpreter checks problem metadata, every signed model assignment,
#' every hard WCNF clause, the objective cost, formula-tree structure and
#' operator semantics on all target rows. This R wrapper additionally checks
#' BDD equivalence with the original expression. Solver output must preserve
#' complete physical lines, as returned by the corrected Rwbo wrapper.
#'
#' `OPTIMUM FOUND` becomes `optimal = TRUE` only with a validated model and
#' equivalent formula. `SATISFIABLE` can return a verified, unproven candidate.
#' `UNSATISFIABLE`, `UNKNOWN` and missing models return no expression and do
#' not claim verification or optimality. Invalid metadata, incomplete or
#' conflicting models, bad objectives and violated hard clauses raise errors.
#' Valid zero-cost BDD shortcuts are proven directly without solver output.
#'
#' Native callers use `rcudd::interpret_rwbo_output(problem, output)` in
#' `rcudd/bdd_wcnf.hpp`; native verification uses the BDD-derived target vector
#' and does not require an expression or another BDD.
#'
#' @return A list with `expression`, `operator_count`, `optimal`, `verified`,
#'   `status`, problem metadata and `solver_output`. Missing expressions and
#'   operator counts are `NA`.
#'
#' @examples
#' path <- tempfile(fileext = ".wcnf")
#' problem <- bdd_to_wcnf("a", path, max_operators = 0L)
#' interpret_rwbo_output(problem, "")$expression
#' unlink(path)
#'
#' @export
interpret_rwbo_output <- function(problem, output) {
  if (!inherits(problem, "rwbo_formula_problem")) {
    stop("Use a problem returned by bdd_to_wcnf().", call. = FALSE)
  }
  if (!is.character(output) || anyNA(output)) {
    stop("'output' must contain non-missing character data.", call. = FALSE)
  }
  if (!is.character(problem$expression) || length(problem$expression) != 1L ||
      is.na(problem$expression) || !nzchar(problem$expression)) {
    stop("Invalid original expression metadata.", call. = FALSE)
  }
  result <- .Call(c_interpret_rwbo_output, problem, paste(output, collapse = "\n"))
  if (result$verified) {
    difference <- paste0("(", problem$expression, ") ^ (", result$expression, ")")
    if (!identical(bdd_literals(difference)$constant, FALSE)) {
      stop("Decoded formula failed the BDD equivalence check.", call. = FALSE)
    }
  }
  result$bdd_expression <- problem$bdd_expression
  result$variables <- problem$variables
  result$max_operators <- problem$max_operators
  result$truth_table_rows <- length(problem$target)
  result$solver_output <- output
  result
}
