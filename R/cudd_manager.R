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
    TRUE
  }
)

#' Create a new CUDD manager
#'
#' @return A [`CuddManager`] instance.
#' @export
CuddManager <- function() {
  ptr <- .Call("c_cudd_new")
  methods::new("CuddManager", ptr = ptr)
}

#' @describeIn CuddManager-class Show a brief summary of the manager.
#' @param object A `CuddManager` instance.
#' @keywords internal
methods::setMethod("show", "CuddManager", function(object) {
  cat("<CuddManager>\n")
})
