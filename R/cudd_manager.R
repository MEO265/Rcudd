#' S4 wrapper for a CUDD manager
#'
#' The CUDD manager encapsulates the underlying C++ `Cudd` instance and is
#' represented in R as an external pointer managed by a finalizer.
#'
#' @slot ptr External pointer to the underlying CUDD manager.
#' @keywords internal
methods::setClass(
  "CuddManager",
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

#' Create a new CUDD manager
#'
#' @return A [`CuddManager`] instance.
#' @export
CuddManager <- function() { # nolint: object_name_linter.
  ptr <- .rcudd_call("c_cudd_new")
  return(methods::new("CuddManager", ptr = ptr))
}

#' @describeIn CuddManager-class Show a brief summary of the manager.
#' @param object A `CuddManager` instance.
#' @keywords internal
methods::setMethod("show", "CuddManager", function(object) {
  cat("<CuddManager>\n")
  return(invisible(object))
})

.cudd_manager_ptr <- function(manager) {
  if (!methods::is(manager, "CuddManager")) {
    stop("`manager` must be a CuddManager object.", call. = FALSE)
  }
  return(manager@ptr)
}

#' Read the number of BDD variables in the manager
#'
#' @param manager A [`CuddManager`] instance.
#' @return Integer scalar with the number of BDD variables.
#' @export
cudd_read_size <- function(manager) {
  return(.rcudd_call("c_cudd_read_size", .cudd_manager_ptr(manager)))
}

#' Read the cache slot count
#'
#' @param manager A [`CuddManager`] instance.
#' @return Integer scalar with the cache slot count.
#' @export
cudd_read_cache_slots <- function(manager) {
  return(.rcudd_call("c_cudd_read_cache_slots", .cudd_manager_ptr(manager)))
}

#' Read the cache used slots
#'
#' @param manager A [`CuddManager`] instance.
#' @return Double scalar with the cache used slot count.
#' @export
cudd_read_cache_used_slots <- function(manager) {
  return(.rcudd_call("c_cudd_read_cache_used_slots", .cudd_manager_ptr(manager)))
}

#' Read the cache lookup count
#'
#' @param manager A [`CuddManager`] instance.
#' @return Double scalar with the cache lookup count.
#' @export
cudd_read_cache_lookups <- function(manager) {
  return(.rcudd_call("c_cudd_read_cache_lookups", .cudd_manager_ptr(manager)))
}

#' Read the cache hit count
#'
#' @param manager A [`CuddManager`] instance.
#' @return Double scalar with the cache hit count.
#' @export
cudd_read_cache_hits <- function(manager) {
  return(.rcudd_call("c_cudd_read_cache_hits", .cudd_manager_ptr(manager)))
}

#' Read the minimum cache hit ratio
#'
#' @param manager A [`CuddManager`] instance.
#' @return Integer scalar with the minimum cache hit ratio.
#' @export
cudd_read_min_hit <- function(manager) {
  return(.rcudd_call("c_cudd_read_min_hit", .cudd_manager_ptr(manager)))
}

#' Read the cache loose up-to threshold
#'
#' @param manager A [`CuddManager`] instance.
#' @return Integer scalar with the cache loose up-to threshold.
#' @export
cudd_read_loose_up_to <- function(manager) {
  return(.rcudd_call("c_cudd_read_loose_up_to", .cudd_manager_ptr(manager)))
}

#' Read the live node count
#'
#' @param manager A [`CuddManager`] instance.
#' @return Double scalar with the live node count.
#' @export
cudd_read_node_count <- function(manager) {
  return(.rcudd_call("c_cudd_read_node_count", .cudd_manager_ptr(manager)))
}

#' Read the number of ZDD variables in the manager
#'
#' @param manager A [`CuddManager`] instance.
#' @return Integer scalar with the number of ZDD variables.
#' @export
cudd_read_zdd_size <- function(manager) {
  return(.rcudd_call("c_cudd_read_zdd_size", .cudd_manager_ptr(manager)))
}

#' Read the number of reorderings performed
#'
#' @param manager A [`CuddManager`] instance.
#' @return Integer scalar with the number of reorderings.
#' @export
cudd_read_reorderings <- function(manager) {
  return(.rcudd_call("c_cudd_read_reorderings", .cudd_manager_ptr(manager)))
}
