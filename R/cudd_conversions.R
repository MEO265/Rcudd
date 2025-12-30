#' Convert a BDD to an ADD
#'
#' @param bdd A [`CuddBDD`] instance.
#' @return A [`CuddADD`] instance.
#' @export
cudd_bdd_to_add <- function(bdd) {
  ptr <- .Call(.cudd_native$c_cudd_bdd_to_add, .cudd_bdd_ptr(bdd))
  return(methods::new("CuddADD", ptr = ptr, manager_ptr = .cudd_bdd_manager_ptr(bdd)))
}

#' Convert an ADD to a BDD using the non-zero pattern
#'
#' @param add A [`CuddADD`] instance.
#' @return A [`CuddBDD`] instance.
#' @export
cudd_add_to_bdd <- function(add) {
  ptr <- .Call(.cudd_native$c_cudd_add_to_bdd, .cudd_add_ptr(add))
  return(methods::new("CuddBDD", ptr = ptr, manager_ptr = .cudd_add_manager_ptr(add)))
}

#' Convert a BDD to a ZDD
#'
#' @param bdd A [`CuddBDD`] instance.
#' @return A [`CuddZDD`] instance.
#' @export
cudd_bdd_to_zdd <- function(bdd) {
  ptr <- .Call(.cudd_native$c_cudd_bdd_to_zdd, .cudd_bdd_ptr(bdd))
  return(methods::new("CuddZDD", ptr = ptr, manager_ptr = .cudd_bdd_manager_ptr(bdd)))
}

#' Convert a ZDD to a BDD
#'
#' @param zdd A [`CuddZDD`] instance.
#' @return A [`CuddBDD`] instance.
#' @export
cudd_zdd_to_bdd <- function(zdd) {
  ptr <- .Call(.cudd_native$c_cudd_zdd_to_bdd, .cudd_zdd_ptr(zdd))
  return(methods::new("CuddBDD", ptr = ptr, manager_ptr = .cudd_zdd_manager_ptr(zdd)))
}
