#' NXOR
#' @param x Integer (0 or 1)
#' @param y Integer (0 or 1)
#' @returns Integer (0 or 1)
test <- function (x, y) .Call(c_run_cudd_example, x, y)