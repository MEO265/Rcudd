test_that("additional BDD methods are exposed", {
  manager <- CuddManager()
  vars <- lapply(1:3, function(i) cudd_bdd_var(manager))

  bdd_one <- cudd_bdd_one(manager)
  bdd_zero <- cudd_bdd_zero(manager)
  bdd_or <- vars[[1L]] + vars[[2L]]
  bdd_and <- vars[[1L]] * vars[[3L]]

  expect_true(cudd_bdd_is_one(bdd_one))
  expect_true(cudd_bdd_is_zero(bdd_zero))
  expect_true(cudd_bdd_is_var(vars[[1L]]))
  expect_true(cudd_bdd_is_cube(vars[[1L]]))
  expect_false(cudd_bdd_is_cube(bdd_or))

  expect_equal(cudd_bdd_count_minterm(vars[[1L]], 3L), 4.0, tolerance = 1e-8)
  expect_identical(cudd_bdd_support_size(bdd_and), 2L)
  expect_identical(cudd_bdd_support_indices(bdd_and), c(0L, 2L))

  classified <- cudd_bdd_classify_support(bdd_and, vars[[1L]])
  expect_named(classified, c("common", "only_bdd", "only_other"))
  expect_true(methods::is(classified$common, "CuddBDD"))

  expect_snapshot_output(cudd_bdd_print(bdd_or, 3L))
  expect_silent(cudd_bdd_summary(bdd_or, 3L))

  expect_equal(cudd_bdd_correlation(vars[[1L]], vars[[1L]]), 1.0)
  expect_equal(
    cudd_bdd_correlation_weights(vars[[1L]], vars[[1L]], rep(0.5, 3L)),
    1.0
  )
  expect_identical(cudd_bdd_ite_formula(vars[[1L]] + vars[[2L]]), "or(x0, x1)")
  expect_identical(cudd_bdd_ite_formula(vars[[1L]] * vars[[2L]]), "and(x0, x1)")
  expect_identical(cudd_bdd_ite_formula(!vars[[1L]]), "not(x0)")
  expect_identical(cudd_bdd_ite_formula(vars[[1L]] ^ vars[[2L]]), "xor(x0, x1)")
  expect_identical(cudd_bdd_ite_formula(!(vars[[1L]] ^ vars[[2L]])), "not(xor(x0, x1))")
  expect_identical(
    cudd_bdd_ite_formula((!vars[[1L]]) * vars[[2L]]),
    "and(not(x0), x1)"
  )

  shortest_length <- cudd_bdd_shortest_length(vars[[1L]])
  expect_type(shortest_length, "integer")
  expect_gte(shortest_length, 0L)

  manager2 <- CuddManager()
  transferred <- cudd_bdd_transfer(vars[[1L]], manager2)
  expect_true(methods::is(transferred, "CuddBDD"))
  expect_silent(transferred + cudd_bdd_var(manager2))
})
