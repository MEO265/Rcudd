#' S4 wrapper for a CUDD ZDD
#'
#' The ZDD wrapper encapsulates the underlying C++ `ZDD` instance and is
#' represented in R as an external pointer managed by a finalizer.
#'
#' @slot ptr External pointer to the underlying ZDD object.
#' @keywords internal
methods::setClass(
  "CuddZDD",
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

#' @describeIn CuddZDD-class Show a brief summary of the ZDD.
#' @param object A `CuddZDD` instance.
#' @keywords internal
methods::setMethod("show", "CuddZDD", function(object) {
  cat("<CuddZDD>\n")
})

.cudd_zdd_ptr <- function(zdd) {
  if (!methods::is(zdd, "CuddZDD")) {
    stop("`zdd` must be a CuddZDD object.", call. = FALSE)
  }
  zdd@ptr
}

#' Create a ZDD node that represents the constant one
#'
#' @param manager A [`CuddManager`] instance.
#' @param index Non-negative integer index for the ZDD one node.
#' @return A [`CuddZDD`] instance representing the ZDD one node.
#' @export
cudd_zdd_one <- function(manager, index = 0L) {
  ptr <- .rcudd_call("c_cudd_zdd_one", .cudd_manager_ptr(manager), index)
  methods::new("CuddZDD", ptr = ptr)
}

#' Create a ZDD node that represents the constant zero
#'
#' @param manager A [`CuddManager`] instance.
#' @return A [`CuddZDD`] instance representing the ZDD zero node.
#' @export
cudd_zdd_zero <- function(manager) {
  ptr <- .rcudd_call("c_cudd_zdd_zero", .cudd_manager_ptr(manager))
  methods::new("CuddZDD", ptr = ptr)
}

#' Create or access a ZDD variable
#'
#' When `index` is `NULL`, the next available ZDD variable index is used.
#'
#' @param manager A [`CuddManager`] instance.
#' @param index Optional non-negative integer index of the ZDD variable.
#' @return A [`CuddZDD`] instance representing the requested variable.
#' @export
cudd_zdd_var <- function(manager, index = NULL) {
  ptr <- .rcudd_call("c_cudd_zdd_var", .cudd_manager_ptr(manager), index)
  methods::new("CuddZDD", ptr = ptr)
}

#' @describeIn CuddZDD-class Combine ZDDs with intersection
#' @param e1 A `CuddZDD` instance.
#' @param e2 A `CuddZDD` instance.
#' @return A `CuddZDD` instance.
#' @export
setMethod("*", signature(e1 = "CuddZDD", e2 = "CuddZDD"), function(e1, e2) {
  ptr <- .rcudd_call("c_cudd_zdd_intersect", .cudd_zdd_ptr(e1), .cudd_zdd_ptr(e2))
  methods::new("CuddZDD", ptr = ptr)
})

#' @describeIn CuddZDD-class Combine ZDDs with union
#' @param e1 A `CuddZDD` instance.
#' @param e2 A `CuddZDD` instance.
#' @return A `CuddZDD` instance.
#' @export
setMethod("+", signature(e1 = "CuddZDD", e2 = "CuddZDD"), function(e1, e2) {
  ptr <- .rcudd_call("c_cudd_zdd_union", .cudd_zdd_ptr(e1), .cudd_zdd_ptr(e2))
  methods::new("CuddZDD", ptr = ptr)
})

#' @describeIn CuddZDD-class XOR is not defined for ZDDs
#' @param e1 A `CuddZDD` instance.
#' @param e2 A `CuddZDD` instance.
#' @return An error indicating the operator is unsupported.
#' @export
setMethod("^", signature(e1 = "CuddZDD", e2 = "CuddZDD"), function(e1, e2) {
  stop("XOR (^) is not defined for CuddZDD.", call. = FALSE)
})

#' @describeIn CuddZDD-class Negation is not defined for ZDDs
#' @param x A `CuddZDD` instance.
#' @return An error indicating the operator is unsupported.
#' @export
setMethod("!", "CuddZDD", function(x) {
  stop("Negation (!) is not defined for CuddZDD.", call. = FALSE)
})
