#' S4 wrapper for a CUDD BDD
#'
#' The BDD wrapper encapsulates the underlying C++ `BDD` instance and is
#' represented in R as an external pointer managed by a finalizer.
#'
#' @slot ptr External pointer to the underlying BDD object.
#' @keywords internal
methods::setClass(
  "CuddBDD",
  slots = list(
    ptr = "externalptr"
  ),
  validity = function(object) {
    if (!methods::is(object@ptr, "externalptr")) {
      return("`ptr` must be an external pointer.")
    }
    if (is.null(object@ptr)) {
      return("`ptr` must not be NULL.")
    }
    TRUE
  }
)

#' @describeIn CuddBDD-class Show a brief summary of the BDD.
#' @param object A `CuddBDD` instance.
#' @keywords internal
methods::setMethod("show", "CuddBDD", function(object) {
  cat("<CuddBDD>\n")
})

.cudd_bdd_ptr <- function(bdd) {
  if (!methods::is(bdd, "CuddBDD")) {
    stop("`bdd` must be a CuddBDD object.", call. = FALSE)
  }
  bdd@ptr
}

#' Create a BDD node that represents logical TRUE
#'
#' @param manager A [`CuddManager`] instance.
#' @return A [`CuddBDD`] instance representing a constant TRUE BDD.
#' @export
cudd_bdd_one <- function(manager) {
  ptr <- .rcudd_call("c_cudd_bdd_one", .cudd_manager_ptr(manager))
  methods::new("CuddBDD", ptr = ptr)
}

#' Create a BDD node that represents logical FALSE
#'
#' @param manager A [`CuddManager`] instance.
#' @return A [`CuddBDD`] instance representing a constant FALSE BDD.
#' @export
cudd_bdd_zero <- function(manager) {
  ptr <- .rcudd_call("c_cudd_bdd_zero", .cudd_manager_ptr(manager))
  methods::new("CuddBDD", ptr = ptr)
}

#' Create or access a BDD variable
#'
#' When `index` is `NULL`, a new BDD variable is created in the manager and
#' returned. Otherwise, the BDD variable at the specified index is returned.
#'
#' @param manager A [`CuddManager`] instance.
#' @param index Optional non-negative integer index of the BDD variable.
#' @return A [`CuddBDD`] instance representing the requested variable.
#' @export
cudd_bdd_var <- function(manager, index = NULL) {
  ptr <- .rcudd_call("c_cudd_bdd_var", .cudd_manager_ptr(manager), index)
  methods::new("CuddBDD", ptr = ptr)
}
