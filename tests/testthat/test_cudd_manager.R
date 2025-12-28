test_that("CuddManager exposes manager stats", {
  manager <- CuddManager()

  expect_length(cudd_read_cache_used_slots(manager), 1L)
  expect_type(cudd_read_cache_used_slots(manager), "double")

  expect_length(cudd_read_min_hit(manager), 1L)
  expect_type(cudd_read_min_hit(manager), "integer")

  expect_length(cudd_read_loose_up_to(manager), 1L)
  expect_type(cudd_read_loose_up_to(manager), "integer")

  expect_length(cudd_read_zdd_size(manager), 1L)
  expect_type(cudd_read_zdd_size(manager), "integer")

  expect_length(cudd_read_reorderings(manager), 1L)
  expect_type(cudd_read_reorderings(manager), "integer")
})

test_that("CuddManager exposes time limit and verbosity controls", {
  manager <- CuddManager()

  expect_length(cudd_read_start_time(manager), 1L)
  expect_type(cudd_read_start_time(manager), "double")
  expect_length(cudd_read_elapsed_time(manager), 1L)
  expect_type(cudd_read_elapsed_time(manager), "double")

  expect_null(cudd_set_start_time(manager, 1.0))
  expect_null(cudd_reset_start_time(manager))

  expect_length(cudd_read_time_limit(manager), 1L)
  expect_type(cudd_read_time_limit(manager), "double")
  expect_length(cudd_set_time_limit(manager, 1.0), 1L)
  expect_type(cudd_set_time_limit(manager, 1.0), "double")
  expect_null(cudd_update_time_limit(manager))
  expect_null(cudd_increase_time_limit(manager, 1.0))
  expect_null(cudd_unset_time_limit(manager))
  expect_type(cudd_time_limited(manager), "logical")

  expect_null(cudd_make_verbose(manager))
  expect_type(cudd_is_verbose(manager), "logical")
  expect_null(cudd_info(manager))
  expect_null(cudd_make_terse(manager))
})

test_that("CuddManager exposes reordering and realignment controls", {
  manager <- CuddManager()

  expect_null(cudd_autodyn_enable(manager))
  status <- cudd_reordering_status(manager)
  expect_true(all(c("enabled", "method") %in% names(status)))
  expect_type(status$enabled, "logical")
  expect_type(status$method, "integer")
  expect_null(cudd_autodyn_disable(manager))

  expect_null(cudd_autodyn_enable_zdd(manager))
  status_zdd <- cudd_reordering_status_zdd(manager)
  expect_true(all(c("enabled", "method") %in% names(status_zdd)))
  expect_type(status_zdd$enabled, "logical")
  expect_type(status_zdd$method, "integer")
  expect_null(cudd_autodyn_disable_zdd(manager))

  expect_type(cudd_bdd_realignment_enabled(manager), "logical")
  expect_null(cudd_bdd_realign_enable(manager))
  expect_true(cudd_bdd_realignment_enabled(manager))
  expect_null(cudd_bdd_realign_disable(manager))

  expect_type(cudd_zdd_realignment_enabled(manager), "logical")
  expect_null(cudd_zdd_realign_enable(manager))
  expect_true(cudd_zdd_realignment_enabled(manager))
  expect_null(cudd_zdd_realign_disable(manager))
})

test_that("CuddManager exposes cache and memory controls", {
  manager <- CuddManager()

  expect_null(cudd_set_min_hit(manager, 1L))
  expect_length(cudd_read_min_hit(manager), 1L)

  expect_null(cudd_set_loose_up_to(manager, 1L))
  expect_length(cudd_read_loose_up_to(manager), 1L)

  expect_length(cudd_read_max_cache(manager), 1L)
  expect_type(cudd_read_max_cache(manager), "integer")
  expect_length(cudd_read_max_cache_hard(manager), 1L)
  expect_type(cudd_read_max_cache_hard(manager), "integer")
  expect_null(cudd_set_max_cache_hard(manager, 10L))

  expect_length(cudd_read_slots(manager), 1L)
  expect_type(cudd_read_slots(manager), "integer")
  expect_length(cudd_read_keys(manager), 1L)
  expect_type(cudd_read_keys(manager), "integer")
  expect_length(cudd_read_dead(manager), 1L)
  expect_type(cudd_read_dead(manager), "integer")
  expect_length(cudd_read_min_dead(manager), 1L)
  expect_type(cudd_read_min_dead(manager), "integer")

  expect_length(cudd_read_memory_in_use(manager), 1L)
  expect_type(cudd_read_memory_in_use(manager), "double")
  expect_length(cudd_read_peak_node_count(manager), 1L)
  expect_type(cudd_read_peak_node_count(manager), "double")
  expect_length(cudd_read_node_count_current(manager), 1L)
  expect_type(cudd_read_node_count_current(manager), "double")
  expect_length(cudd_read_node_count_zdd(manager), 1L)
  expect_type(cudd_read_node_count_zdd(manager), "double")

  expect_length(cudd_read_max_memory(manager), 1L)
  expect_type(cudd_read_max_memory(manager), "double")
  expect_length(cudd_set_max_memory(manager, 1024.0), 1L)
  expect_type(cudd_set_max_memory(manager, 1024.0), "double")
})

test_that("CuddManager exposes variable and reordering parameters", {
  manager <- CuddManager()

  expect_null(cudd_set_max_reorderings(manager, 1L))
  expect_length(cudd_read_max_reorderings(manager), 1L)
  expect_type(cudd_read_reordering_time(manager), "double")

  expect_null(cudd_set_sift_max_var(manager, 1L))
  expect_length(cudd_read_sift_max_var(manager), 1L)
  expect_type(cudd_read_sift_max_var(manager), "integer")
  expect_null(cudd_set_sift_max_swap(manager, 1L))
  expect_length(cudd_read_sift_max_swap(manager), 1L)
  expect_type(cudd_read_sift_max_swap(manager), "integer")

  expect_null(cudd_set_max_growth(manager, 1.5))
  expect_length(cudd_read_max_growth(manager), 1L)
  expect_type(cudd_read_max_growth(manager), "double")

  expect_null(cudd_set_groupcheck(manager, 0L))
  expect_length(cudd_read_groupcheck(manager), 1L)
  expect_type(cudd_read_groupcheck(manager), "integer")

  expect_null(cudd_set_recomb(manager, 1L))
  expect_length(cudd_read_recomb(manager), 1L)
  expect_type(cudd_read_recomb(manager), "integer")
  expect_null(cudd_set_symmviolation(manager, 1L))
  expect_length(cudd_read_symmviolation(manager), 1L)
  expect_type(cudd_read_symmviolation(manager), "integer")
  expect_null(cudd_set_arcviolation(manager, 1L))
  expect_length(cudd_read_arcviolation(manager), 1L)
  expect_type(cudd_read_arcviolation(manager), "integer")

  expect_null(cudd_set_population_size(manager, 2L))
  expect_length(cudd_read_population_size(manager), 1L)
  expect_type(cudd_read_population_size(manager), "integer")
  expect_null(cudd_set_number_xovers(manager, 2L))
  expect_length(cudd_read_number_xovers(manager), 1L)
  expect_type(cudd_read_number_xovers(manager), "integer")
  expect_null(cudd_set_order_randomization(manager, 1L))
  expect_length(cudd_read_order_randomization(manager), 1L)
  expect_type(cudd_read_order_randomization(manager), "integer")

  expect_null(cudd_set_next_reordering(manager, 0L))
  expect_length(cudd_read_next_reordering(manager), 1L)
  expect_type(cudd_read_next_reordering(manager), "integer")
  expect_length(cudd_read_swap_steps(manager), 1L)
  expect_type(cudd_read_swap_steps(manager), "double")
})

test_that("CuddManager exposes variable name, epsilon, and GC helpers", {
  manager <- CuddManager()

  expect_null(cudd_push_variable_name(manager, "x0"))
  expect_identical(cudd_get_variable_name(manager, 0L), "x0")
  expect_null(cudd_clear_variable_names(manager))

  expect_length(cudd_read_epsilon(manager), 1L)
  expect_type(cudd_read_epsilon(manager), "double")
  expect_null(cudd_set_epsilon(manager, 0.01))

  expect_type(cudd_garbage_collection_enabled(manager), "logical")
  expect_null(cudd_enable_garbage_collection(manager))
  expect_null(cudd_disable_garbage_collection(manager))

  expect_type(cudd_dead_are_counted(manager), "logical")
  expect_null(cudd_turn_on_count_dead(manager))
  expect_null(cudd_turn_off_count_dead(manager))

  expect_null(cudd_enable_reordering_reporting(manager))
  expect_type(cudd_reordering_reporting(manager), "logical")
  expect_null(cudd_disable_reordering_reporting(manager))

  expect_length(cudd_read_error_code(manager), 1L)
  expect_type(cudd_read_error_code(manager), "integer")
  expect_null(cudd_clear_error_code(manager))
})

test_that("CuddManager exposes BDD bindings and variable accessors", {
  manager <- CuddManager()

  expect_s4_class(cudd_bdd_var(manager, 0L), "CuddBDD")
  expect_type(cudd_bdd_var_is_bound(manager, 0L), "logical")
  expect_type(cudd_bdd_bind_var(manager, 0L), "integer")
  expect_type(cudd_bdd_var_is_bound(manager, 0L), "logical")
  expect_type(cudd_bdd_unbind_var(manager, 0L), "integer")

  expect_s4_class(cudd_read_vars(manager, 0L), "CuddBDD")
  expect_type(cudd_read_perm(manager, 0L), "integer")
  expect_type(cudd_read_perm_zdd(manager, 0L), "integer")
  expect_type(cudd_read_inv_perm(manager, 0L), "integer")
  expect_type(cudd_read_inv_perm_zdd(manager, 0L), "integer")
})

test_that("CuddManager exposes background and live controls", {
  manager <- CuddManager()

  add_zero <- cudd_add_zero(manager)
  expect_s4_class(cudd_background(manager), "CuddADD")
  expect_null(cudd_set_background(manager, add_zero))

  expect_length(cudd_read_max_live(manager), 1L)
  expect_type(cudd_read_max_live(manager), "integer")
  expect_null(cudd_set_max_live(manager, 1L))
})
