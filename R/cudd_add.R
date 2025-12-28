#' S4 wrapper for a CUDD ADD
#'
#' The ADD wrapper encapsulates the underlying C++ `ADD` instance and is
#' represented in R as an external pointer managed by a finalizer.
#'
#' @slot ptr External pointer to the underlying ADD object.
#' @keywords internal
methods::setClass(
  "CuddADD",
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

#' @describeIn CuddADD-class Show a brief summary of the ADD.
#' @param object A `CuddADD` instance.
#' @keywords internal
methods::setMethod("show", "CuddADD", function(object) {
  cat("<CuddADD>\n")
})

.cudd_add_ptr <- function(add) {
  if (!methods::is(add, "CuddADD")) {
    stop("`add` must be a CuddADD object.", call. = FALSE)
  }
  add@ptr
}

#' Create an ADD node that represents logical TRUE
#'
#' @param manager A [`CuddManager`] instance.
#' @return A [`CuddADD`] instance representing a constant TRUE ADD.
#' @export
cudd_add_one <- function(manager) {
  ptr <- .rcudd_call("c_cudd_add_one", .cudd_manager_ptr(manager))
  methods::new("CuddADD", ptr = ptr)
}

#' Create an ADD node that represents logical FALSE
#'
#' @param manager A [`CuddManager`] instance.
#' @return A [`CuddADD`] instance representing a constant FALSE ADD.
#' @export
cudd_add_zero <- function(manager) {
  ptr <- .rcudd_call("c_cudd_add_zero", .cudd_manager_ptr(manager))
  methods::new("CuddADD", ptr = ptr)
}

#' Create or access an ADD variable
#'
#' When `index` is `NULL`, a new ADD variable is created in the manager and
#' returned. Otherwise, the ADD variable at the specified index is returned.
#'
#' @param manager A [`CuddManager`] instance.
#' @param index Optional non-negative integer index of the ADD variable.
#' @return A [`CuddADD`] instance representing the requested variable.
#' @export
cudd_add_var <- function(manager, index = NULL) {
  ptr <- .rcudd_call("c_cudd_add_var", .cudd_manager_ptr(manager), index)
  methods::new("CuddADD", ptr = ptr)
}
