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
    return(TRUE)
  }
)

#' @describeIn CuddADD-class Show a brief summary of the ADD.
#' @param object A `CuddADD` instance.
#' @keywords internal
methods::setMethod("show", "CuddADD", function(object) {
  cat("<CuddADD>\n")
  return(invisible(object))
})

.cudd_add_ptr <- function(add) {
  if (!methods::is(add, "CuddADD")) {
    stop("`add` must be a CuddADD object.", call. = FALSE)
  }
  return(add@ptr)
}

#' Create an ADD node that represents logical TRUE
#'
#' @param manager A [`CuddManager`] instance.
#' @return A [`CuddADD`] instance representing a constant TRUE ADD.
#' @export
cudd_add_one <- function(manager) {
  ptr <- .rcudd_call("c_cudd_add_one", .cudd_manager_ptr(manager))
  return(methods::new("CuddADD", ptr = ptr))
}

#' Create an ADD node that represents logical FALSE
#'
#' @param manager A [`CuddManager`] instance.
#' @return A [`CuddADD`] instance representing a constant FALSE ADD.
#' @export
cudd_add_zero <- function(manager) {
  ptr <- .rcudd_call("c_cudd_add_zero", .cudd_manager_ptr(manager))
  return(methods::new("CuddADD", ptr = ptr))
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
  return(methods::new("CuddADD", ptr = ptr))
}

#' @describeIn CuddADD-class Combine ADDs with multiplication
#' @param e1 A `CuddADD` instance.
#' @param e2 A `CuddADD` instance.
#' @return A `CuddADD` instance.
setMethod("*", signature(e1 = "CuddADD", e2 = "CuddADD"), function(e1, e2) {
  ptr <- .rcudd_call("c_cudd_add_times", .cudd_add_ptr(e1), .cudd_add_ptr(e2))
  return(methods::new("CuddADD", ptr = ptr))
})

#' @describeIn CuddADD-class Combine ADDs with addition
#' @param e1 A `CuddADD` instance.
#' @param e2 A `CuddADD` instance.
#' @return A `CuddADD` instance.
setMethod("+", signature(e1 = "CuddADD", e2 = "CuddADD"), function(e1, e2) {
  ptr <- .rcudd_call("c_cudd_add_plus", .cudd_add_ptr(e1), .cudd_add_ptr(e2))
  return(methods::new("CuddADD", ptr = ptr))
})

#' @describeIn CuddADD-class XOR is not defined for ADDs
#' @param e1 A `CuddADD` instance.
#' @param e2 A `CuddADD` instance.
#' @return An error indicating the operator is unsupported.
setMethod("^", signature(e1 = "CuddADD", e2 = "CuddADD"), function(e1, e2) {
  stop("XOR (^) is not defined for CuddADD.", call. = FALSE)
})

#' @describeIn CuddADD-class Negation is not defined for ADDs
#' @param x A `CuddADD` instance.
#' @return An error indicating the operator is unsupported.
setMethod("!", "CuddADD", function(x) {
  stop("Negation (!) is not defined for CuddADD.", call. = FALSE)
})
