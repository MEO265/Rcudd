#' @useDynLib Rcudd, .registration=TRUE
#' @importFrom methods show
NULL

.cudd_check_same_manager <- function(lhs, rhs, op) {
  same_manager <- identical(lhs@manager_ptr, rhs@manager_ptr)
  if (!same_manager) {
    warning(
      sprintf("Objects for %s come from different CuddManager instances.", op),
      call. = FALSE
    )
  }
  return(same_manager)
}
