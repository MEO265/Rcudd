test_that("CuddManager exposes manager stats", {
  manager <- CuddManager()

  expect_length(cudd_read_cache_used_slots(manager), 1L)
  expect_true(is.numeric(cudd_read_cache_used_slots(manager)))

  expect_length(cudd_read_min_hit(manager), 1L)
  expect_true(is.numeric(cudd_read_min_hit(manager)))

  expect_length(cudd_read_loose_up_to(manager), 1L)
  expect_true(is.numeric(cudd_read_loose_up_to(manager)))

  expect_length(cudd_read_zdd_size(manager), 1L)
  expect_true(is.numeric(cudd_read_zdd_size(manager)))

  expect_length(cudd_read_reorderings(manager), 1L)
  expect_true(is.numeric(cudd_read_reorderings(manager)))
})
