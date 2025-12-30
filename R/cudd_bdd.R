#' S4 wrapper for a CUDD BDD
#'
#' The BDD wrapper encapsulates the underlying C++ `BDD` instance and is
#' represented in R as an external pointer managed by a finalizer.
#'
#' @slot ptr External pointer to the underlying BDD object.
#' @slot manager_ptr External pointer to the owning CUDD manager.
#' @keywords internal
methods::setClass(
  "CuddBDD",
  slots = list(
    ptr = "externalptr",
    manager_ptr = "externalptr"
  ),
  validity = function(object) {
    if (!methods::is(object@ptr, "externalptr")) {
      return("`ptr` must be an external pointer.")
    }
    if (is.null(object@ptr)) {
      return("`ptr` must not be NULL.")
    }
    if (!methods::is(object@manager_ptr, "externalptr")) {
      return("`manager_ptr` must be an external pointer.")
    }
    if (is.null(object@manager_ptr)) {
      return("`manager_ptr` must not be NULL.")
    }
    return(TRUE)
  }
)

#' @describeIn CuddBDD-class Show a brief summary of the BDD.
#' @param object A `CuddBDD` instance.
#' @keywords internal
methods::setMethod("show", "CuddBDD", function(object) {
  cat("<CuddBDD>\n")
  print(object)
  return(invisible(object))
})

.cudd_bdd_ptr <- function(bdd) {
  if (!methods::is(bdd, "CuddBDD")) {
    stop("`bdd` must be a CuddBDD object.", call. = FALSE)
  }
  return(bdd@ptr)
}

.cudd_bdd_manager_ptr <- function(bdd) {
  if (!methods::is(bdd, "CuddBDD")) {
    stop("`bdd` must be a CuddBDD object.", call. = FALSE)
  }
  return(bdd@manager_ptr)
}

#' Create a BDD node that represents logical TRUE
#'
#' @param manager A [`CuddManager`] instance.
#' @return A [`CuddBDD`] instance representing a constant TRUE BDD.
#' @export
cudd_bdd_one <- function(manager) {
  ptr <- .Call(c_cudd_bdd_one, .cudd_manager_ptr(manager))
  return(methods::new("CuddBDD", ptr = ptr, manager_ptr = .cudd_manager_ptr(manager)))
}

#' Create a BDD node that represents logical FALSE
#'
#' @param manager A [`CuddManager`] instance.
#' @return A [`CuddBDD`] instance representing a constant FALSE BDD.
#' @export
cudd_bdd_zero <- function(manager) {
  ptr <- .Call(c_cudd_bdd_zero, .cudd_manager_ptr(manager))
  return(methods::new("CuddBDD", ptr = ptr, manager_ptr = .cudd_manager_ptr(manager)))
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
  ptr <- .Call(c_cudd_bdd_var, .cudd_manager_ptr(manager), index)
  return(methods::new("CuddBDD", ptr = ptr, manager_ptr = .cudd_manager_ptr(manager)))
}

#' @describeIn CuddBDD-class Negate a BDD
#' @param x A `CuddBDD` instance.
#' @return A `CuddBDD` instance.
setMethod("!", "CuddBDD", function(x) {
  ptr <- .Call(c_cudd_bdd_not, .cudd_bdd_ptr(x))
  return(methods::new("CuddBDD", ptr = ptr, manager_ptr = .cudd_bdd_manager_ptr(x)))
})

#' @describeIn CuddBDD-class Combine BDDs with addition (logical OR)
#' @param e1 A `CuddBDD` instance.
#' @param e2 A `CuddBDD` instance.
#' @return A `CuddBDD` instance.
setMethod("+", signature(e1 = "CuddBDD", e2 = "CuddBDD"), function(e1, e2) {
  if (!.cudd_check_same_manager(e1, e2, "+")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_or, .cudd_bdd_ptr(e1), .cudd_bdd_ptr(e2))
  return(methods::new("CuddBDD", ptr = ptr, manager_ptr = .cudd_bdd_manager_ptr(e1)))
})

#' @describeIn CuddBDD-class Combine BDDs with multiplication (logical AND)
#' @param e1 A `CuddBDD` instance.
#' @param e2 A `CuddBDD` instance.
#' @return A `CuddBDD` instance.
setMethod("*", signature(e1 = "CuddBDD", e2 = "CuddBDD"), function(e1, e2) {
  if (!.cudd_check_same_manager(e1, e2, "*")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_and, .cudd_bdd_ptr(e1), .cudd_bdd_ptr(e2))
  return(methods::new("CuddBDD", ptr = ptr, manager_ptr = .cudd_bdd_manager_ptr(e1)))
})

#' @describeIn CuddBDD-class Combine BDDs with XOR
#' @param e1 A `CuddBDD` instance.
#' @param e2 A `CuddBDD` instance.
#' @return A `CuddBDD` instance.
setMethod("^", signature(e1 = "CuddBDD", e2 = "CuddBDD"), function(e1, e2) {
  if (!.cudd_check_same_manager(e1, e2, "^")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_xor, .cudd_bdd_ptr(e1), .cudd_bdd_ptr(e2))
  return(methods::new("CuddBDD", ptr = ptr, manager_ptr = .cudd_bdd_manager_ptr(e1)))
})

#' Print an EPD minterm count for a BDD
#'
#' This uses the CUDD `EpdPrintMinterm` implementation, which writes to R's
#' output stream.
#'
#' @param bdd A [`CuddBDD`] instance.
#' @param nvars Non-negative integer indicating how many variables to consider.
#' @return `NULL`, invisibly.
#' @export
cudd_bdd_epd_print_minterm <- function(bdd, nvars) {
  .Call(c_cudd_bdd_epd_print_minterm, .cudd_bdd_ptr(bdd), nvars)
  return(invisible(NULL))
}

#' Print the minterm representation for a BDD
#'
#' This uses the CUDD `PrintMinterm` implementation, which writes to R's
#' output stream.
#'
#' @param x A [`CuddBDD`] instance.
#' @param ... Unused.
#' @return The input `x`, invisibly.
#' @export
setMethod("print", "CuddBDD", function(x, ...) {
  .Call(c_cudd_bdd_print_minterm, .cudd_bdd_ptr(x))
  return(invisible(x))
})

#' Print a debug representation for a BDD
#'
#' This uses the CUDD `PrintDebug` implementation, which writes to R's output
#' stream.
#'
#' @param bdd A [`CuddBDD`] instance.
#' @param nvars Optional non-negative integer indicating how many variables to
#'   include. Defaults to the manager size.
#' @param verbosity Optional non-negative integer debug verbosity.
#' @return `NULL`, invisibly.
#' @export
cudd_bdd_print_debug <- function(bdd, nvars = NULL, verbosity = NULL) {
  .Call(c_cudd_bdd_print_debug, .cudd_bdd_ptr(bdd), nvars, verbosity)
  return(invisible(NULL))
}

#' Dump a BDD as Graphviz DOT
#'
#' Produces a DOT representation that can be rendered by Graphviz to visualize
#' the full decision diagram structure.
#'
#' @param bdd A [`CuddBDD`] instance.
#' @return Character scalar containing DOT text.
#' @export
cudd_bdd_dump_dot <- function(bdd) {
  return(.Call(c_cudd_bdd_dump_dot, .cudd_bdd_ptr(bdd)))
}
