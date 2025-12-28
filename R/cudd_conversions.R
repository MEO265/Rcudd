#' Convert a BDD to an ADD
#'
#' @param bdd A [`CuddBDD`] instance.
#' @return A [`CuddADD`] instance.
#' @export
cudd_bdd_to_add <- function(bdd) {
  ptr <- .rcudd_call("c_cudd_bdd_to_add", .cudd_bdd_ptr(bdd))
  methods::new("CuddADD", ptr = ptr)
}

#' Convert an ADD to a BDD using the non-zero pattern
#'
#' @param add A [`CuddADD`] instance.
#' @return A [`CuddBDD`] instance.
#' @export
cudd_add_to_bdd <- function(add) {
  ptr <- .rcudd_call("c_cudd_add_to_bdd", .cudd_add_ptr(add))
  methods::new("CuddBDD", ptr = ptr)
}

#' Convert a BDD to a ZDD
#'
#' @param bdd A [`CuddBDD`] instance.
#' @return A [`CuddZDD`] instance.
#' @export
cudd_bdd_to_zdd <- function(bdd) {
  ptr <- .rcudd_call("c_cudd_bdd_to_zdd", .cudd_bdd_ptr(bdd))
  methods::new("CuddZDD", ptr = ptr)
}

#' Convert a ZDD to a BDD
#'
#' @param zdd A [`CuddZDD`] instance.
#' @return A [`CuddBDD`] instance.
#' @export
cudd_zdd_to_bdd <- function(zdd) {
  ptr <- .rcudd_call("c_cudd_zdd_to_bdd", .cudd_zdd_ptr(zdd))
  methods::new("CuddBDD", ptr = ptr)
}
