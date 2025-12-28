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

#' @rdname cudd_manager_controls
#' @export
cudd_set_min_hit <- function(manager, hr) {
  return(.rcudd_call("c_cudd_set_min_hit", .cudd_manager_ptr(manager), hr))
}

#' Read the cache loose up-to threshold
#'
#' @param manager A [`CuddManager`] instance.
#' @return Integer scalar with the cache loose up-to threshold.
#' @export
cudd_read_loose_up_to <- function(manager) {
  return(.rcudd_call("c_cudd_read_loose_up_to", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_set_loose_up_to <- function(manager, lut) {
  return(.rcudd_call("c_cudd_set_loose_up_to", .cudd_manager_ptr(manager), lut))
}

#' @rdname cudd_manager_controls
#' @export
cudd_make_verbose <- function(manager) {
  return(.rcudd_call("c_cudd_make_verbose", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_make_terse <- function(manager) {
  return(.rcudd_call("c_cudd_make_terse", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_is_verbose <- function(manager) {
  return(.rcudd_call("c_cudd_is_verbose", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_info <- function(manager) {
  return(.rcudd_call("c_cudd_info", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_push_variable_name <- function(manager, name) {
  return(.rcudd_call("c_cudd_push_variable_name", .cudd_manager_ptr(manager), name))
}

#' @rdname cudd_manager_controls
#' @export
cudd_clear_variable_names <- function(manager) {
  return(.rcudd_call("c_cudd_clear_variable_names", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_get_variable_name <- function(manager, index) {
  return(.rcudd_call("c_cudd_get_variable_name", .cudd_manager_ptr(manager), index))
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

#' CUDD manager controls
#'
#' Convenience wrappers around the CUDD manager API for tuning reordering,
#' time limits, memory limits, and reading manager statistics.
#'
#' @param manager A [`CuddManager`] instance.
#' @param st,tl,inc Numeric scalars used for time limit operations.
#' @param method Optional integer reordering method (CUDD enum value).
#' @param mc,mr,smv,sms,max_live Integers used for cache/reordering limits.
#' @param hr,lut Integers used for cache hit/loose-up-to thresholds.
#' @param mg,ep Numeric values for growth/epsilon settings.
#' @param i,index Integer indices for variable-related queries.
#' @param name Single string for variable names.
#' @param gc Integer CUDD aggregation type.
#' @param recomb,symm,arc,pop,xovers Integers for genetic algorithm settings.
#' @param factor Integer randomization factor.
#' @param nr Integer next reordering value.
#' @param max_mem Numeric memory limit.
#' @param add A [`CuddADD`] instance.
#' @return A scalar value or `NULL`, depending on the call.
#' @name cudd_manager_controls
NULL

#' @rdname cudd_manager_controls
#' @export
cudd_read_start_time <- function(manager) {
  return(.rcudd_call("c_cudd_read_start_time", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_elapsed_time <- function(manager) {
  return(.rcudd_call("c_cudd_read_elapsed_time", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_set_start_time <- function(manager, st) {
  return(.rcudd_call("c_cudd_set_start_time", .cudd_manager_ptr(manager), st))
}

#' @rdname cudd_manager_controls
#' @export
cudd_reset_start_time <- function(manager) {
  return(.rcudd_call("c_cudd_reset_start_time", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_time_limit <- function(manager) {
  return(.rcudd_call("c_cudd_read_time_limit", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_set_time_limit <- function(manager, tl) {
  return(.rcudd_call("c_cudd_set_time_limit", .cudd_manager_ptr(manager), tl))
}

#' @rdname cudd_manager_controls
#' @export
cudd_update_time_limit <- function(manager) {
  return(.rcudd_call("c_cudd_update_time_limit", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_increase_time_limit <- function(manager, inc) {
  return(.rcudd_call("c_cudd_increase_time_limit", .cudd_manager_ptr(manager), inc))
}

#' @rdname cudd_manager_controls
#' @export
cudd_unset_time_limit <- function(manager) {
  return(.rcudd_call("c_cudd_unset_time_limit", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_time_limited <- function(manager) {
  return(.rcudd_call("c_cudd_time_limited", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_autodyn_enable <- function(manager, method = NULL) {
  return(.rcudd_call("c_cudd_autodyn_enable", .cudd_manager_ptr(manager), method))
}

#' @rdname cudd_manager_controls
#' @export
cudd_autodyn_disable <- function(manager) {
  return(.rcudd_call("c_cudd_autodyn_disable", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_reordering_status <- function(manager) {
  return(.rcudd_call("c_cudd_reordering_status", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_autodyn_enable_zdd <- function(manager, method = NULL) {
  return(.rcudd_call("c_cudd_autodyn_enable_zdd", .cudd_manager_ptr(manager), method))
}

#' @rdname cudd_manager_controls
#' @export
cudd_autodyn_disable_zdd <- function(manager) {
  return(.rcudd_call("c_cudd_autodyn_disable_zdd", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_reordering_status_zdd <- function(manager) {
  return(.rcudd_call("c_cudd_reordering_status_zdd", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_zdd_realignment_enabled <- function(manager) {
  return(.rcudd_call("c_cudd_zdd_realignment_enabled", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_zdd_realign_enable <- function(manager) {
  return(.rcudd_call("c_cudd_zdd_realign_enable", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_zdd_realign_disable <- function(manager) {
  return(.rcudd_call("c_cudd_zdd_realign_disable", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_bdd_realignment_enabled <- function(manager) {
  return(.rcudd_call("c_cudd_bdd_realignment_enabled", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_bdd_realign_enable <- function(manager) {
  return(.rcudd_call("c_cudd_bdd_realign_enable", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_bdd_realign_disable <- function(manager) {
  return(.rcudd_call("c_cudd_bdd_realign_disable", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_background <- function(manager) {
  ptr <- .rcudd_call("c_cudd_background", .cudd_manager_ptr(manager))
  return(methods::new("CuddADD", ptr = ptr))
}

#' @rdname cudd_manager_controls
#' @export
cudd_set_background <- function(manager, add) {
  return(.rcudd_call("c_cudd_set_background", .cudd_manager_ptr(manager), .cudd_add_ptr(add)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_max_cache <- function(manager) {
  return(.rcudd_call("c_cudd_read_max_cache", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_max_cache_hard <- function(manager) {
  return(.rcudd_call("c_cudd_read_max_cache_hard", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_set_max_cache_hard <- function(manager, mc) {
  return(.rcudd_call("c_cudd_set_max_cache_hard", .cudd_manager_ptr(manager), mc))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_slots <- function(manager) {
  return(.rcudd_call("c_cudd_read_slots", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_keys <- function(manager) {
  return(.rcudd_call("c_cudd_read_keys", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_dead <- function(manager) {
  return(.rcudd_call("c_cudd_read_dead", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_min_dead <- function(manager) {
  return(.rcudd_call("c_cudd_read_min_dead", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_max_reorderings <- function(manager) {
  return(.rcudd_call("c_cudd_read_max_reorderings", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_set_max_reorderings <- function(manager, mr) {
  return(.rcudd_call("c_cudd_set_max_reorderings", .cudd_manager_ptr(manager), mr))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_reordering_time <- function(manager) {
  return(.rcudd_call("c_cudd_read_reordering_time", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_garbage_collections <- function(manager) {
  return(.rcudd_call("c_cudd_read_garbage_collections", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_garbage_collection_time <- function(manager) {
  return(.rcudd_call("c_cudd_read_garbage_collection_time", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_sift_max_var <- function(manager) {
  return(.rcudd_call("c_cudd_read_sift_max_var", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_set_sift_max_var <- function(manager, smv) {
  return(.rcudd_call("c_cudd_set_sift_max_var", .cudd_manager_ptr(manager), smv))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_sift_max_swap <- function(manager) {
  return(.rcudd_call("c_cudd_read_sift_max_swap", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_set_sift_max_swap <- function(manager, sms) {
  return(.rcudd_call("c_cudd_set_sift_max_swap", .cudd_manager_ptr(manager), sms))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_max_growth <- function(manager) {
  return(.rcudd_call("c_cudd_read_max_growth", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_set_max_growth <- function(manager, mg) {
  return(.rcudd_call("c_cudd_set_max_growth", .cudd_manager_ptr(manager), mg))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_perm <- function(manager, i) {
  return(.rcudd_call("c_cudd_read_perm", .cudd_manager_ptr(manager), i))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_perm_zdd <- function(manager, i) {
  return(.rcudd_call("c_cudd_read_perm_zdd", .cudd_manager_ptr(manager), i))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_inv_perm <- function(manager, i) {
  return(.rcudd_call("c_cudd_read_inv_perm", .cudd_manager_ptr(manager), i))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_inv_perm_zdd <- function(manager, i) {
  return(.rcudd_call("c_cudd_read_inv_perm_zdd", .cudd_manager_ptr(manager), i))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_vars <- function(manager, i) {
  ptr <- .rcudd_call("c_cudd_read_vars", .cudd_manager_ptr(manager), i)
  return(methods::new("CuddBDD", ptr = ptr))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_epsilon <- function(manager) {
  return(.rcudd_call("c_cudd_read_epsilon", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_set_epsilon <- function(manager, ep) {
  return(.rcudd_call("c_cudd_set_epsilon", .cudd_manager_ptr(manager), ep))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_groupcheck <- function(manager) {
  return(.rcudd_call("c_cudd_read_groupcheck", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_set_groupcheck <- function(manager, gc) {
  return(.rcudd_call("c_cudd_set_groupcheck", .cudd_manager_ptr(manager), gc))
}

#' @rdname cudd_manager_controls
#' @export
cudd_garbage_collection_enabled <- function(manager) {
  return(.rcudd_call("c_cudd_garbage_collection_enabled", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_enable_garbage_collection <- function(manager) {
  return(.rcudd_call("c_cudd_enable_garbage_collection", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_disable_garbage_collection <- function(manager) {
  return(.rcudd_call("c_cudd_disable_garbage_collection", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_dead_are_counted <- function(manager) {
  return(.rcudd_call("c_cudd_dead_are_counted", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_turn_on_count_dead <- function(manager) {
  return(.rcudd_call("c_cudd_turn_on_count_dead", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_turn_off_count_dead <- function(manager) {
  return(.rcudd_call("c_cudd_turn_off_count_dead", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_recomb <- function(manager) {
  return(.rcudd_call("c_cudd_read_recomb", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_set_recomb <- function(manager, recomb) {
  return(.rcudd_call("c_cudd_set_recomb", .cudd_manager_ptr(manager), recomb))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_symmviolation <- function(manager) {
  return(.rcudd_call("c_cudd_read_symmviolation", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_set_symmviolation <- function(manager, symm) {
  return(.rcudd_call("c_cudd_set_symmviolation", .cudd_manager_ptr(manager), symm))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_arcviolation <- function(manager) {
  return(.rcudd_call("c_cudd_read_arcviolation", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_set_arcviolation <- function(manager, arc) {
  return(.rcudd_call("c_cudd_set_arcviolation", .cudd_manager_ptr(manager), arc))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_population_size <- function(manager) {
  return(.rcudd_call("c_cudd_read_population_size", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_set_population_size <- function(manager, pop) {
  return(.rcudd_call("c_cudd_set_population_size", .cudd_manager_ptr(manager), pop))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_number_xovers <- function(manager) {
  return(.rcudd_call("c_cudd_read_number_xovers", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_set_number_xovers <- function(manager, xovers) {
  return(.rcudd_call("c_cudd_set_number_xovers", .cudd_manager_ptr(manager), xovers))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_order_randomization <- function(manager) {
  return(.rcudd_call("c_cudd_read_order_randomization", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_set_order_randomization <- function(manager, factor) {
  return(.rcudd_call("c_cudd_set_order_randomization", .cudd_manager_ptr(manager), factor))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_memory_in_use <- function(manager) {
  return(.rcudd_call("c_cudd_read_memory_in_use", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_peak_node_count <- function(manager) {
  return(.rcudd_call("c_cudd_read_peak_node_count", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_node_count_current <- function(manager) {
  return(.rcudd_call("c_cudd_read_node_count_current", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_node_count_zdd <- function(manager) {
  return(.rcudd_call("c_cudd_read_node_count_zdd", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_enable_reordering_reporting <- function(manager) {
  return(.rcudd_call("c_cudd_enable_reordering_reporting", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_disable_reordering_reporting <- function(manager) {
  return(.rcudd_call("c_cudd_disable_reordering_reporting", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_reordering_reporting <- function(manager) {
  return(.rcudd_call("c_cudd_reordering_reporting", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_error_code <- function(manager) {
  return(.rcudd_call("c_cudd_read_error_code", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_clear_error_code <- function(manager) {
  return(.rcudd_call("c_cudd_clear_error_code", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_next_reordering <- function(manager) {
  return(.rcudd_call("c_cudd_read_next_reordering", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_set_next_reordering <- function(manager, nr) {
  return(.rcudd_call("c_cudd_set_next_reordering", .cudd_manager_ptr(manager), nr))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_swap_steps <- function(manager) {
  return(.rcudd_call("c_cudd_read_swap_steps", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_max_live <- function(manager) {
  return(.rcudd_call("c_cudd_read_max_live", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_set_max_live <- function(manager, max_live) {
  return(.rcudd_call("c_cudd_set_max_live", .cudd_manager_ptr(manager), max_live))
}

#' @rdname cudd_manager_controls
#' @export
cudd_read_max_memory <- function(manager) {
  return(.rcudd_call("c_cudd_read_max_memory", .cudd_manager_ptr(manager)))
}

#' @rdname cudd_manager_controls
#' @export
cudd_set_max_memory <- function(manager, max_mem) {
  return(.rcudd_call("c_cudd_set_max_memory", .cudd_manager_ptr(manager), max_mem))
}

#' @rdname cudd_manager_controls
#' @export
cudd_bdd_bind_var <- function(manager, index) {
  return(.rcudd_call("c_cudd_bdd_bind_var", .cudd_manager_ptr(manager), index))
}

#' @rdname cudd_manager_controls
#' @export
cudd_bdd_unbind_var <- function(manager, index) {
  return(.rcudd_call("c_cudd_bdd_unbind_var", .cudd_manager_ptr(manager), index))
}

#' @rdname cudd_manager_controls
#' @export
cudd_bdd_var_is_bound <- function(manager, index) {
  return(.rcudd_call("c_cudd_bdd_var_is_bound", .cudd_manager_ptr(manager), index))
}
