test_that("BDD operations and conversions return expected classes", {
  manager <- CuddManager()

  bdd_one <- cudd_bdd_one(manager)
  bdd_zero <- cudd_bdd_zero(manager)
  bdd_var <- cudd_bdd_var(manager)

  expect_s4_class(bdd_one, "CuddBDD")
  expect_s4_class(bdd_zero, "CuddBDD")
  expect_s4_class(bdd_var, "CuddBDD")

  expect_s4_class(!bdd_one, "CuddBDD")
  expect_s4_class(bdd_one + bdd_zero, "CuddBDD")
  expect_s4_class(bdd_one * bdd_zero, "CuddBDD")
  expect_s4_class(bdd_one ^ bdd_zero, "CuddBDD")

  add_from_bdd <- cudd_bdd_to_add(bdd_one)
  expect_s4_class(add_from_bdd, "CuddADD")

  zdd_from_bdd <- cudd_bdd_to_zdd(bdd_var)
  expect_s4_class(zdd_from_bdd, "CuddZDD")

  output <- capture.output(cudd_bdd_epd_print_minterm(bdd_one, 0L))
  expect_snapshot(output)

  print_output <- capture.output(print(bdd_one))
  expect_snapshot(print_output)

  debug_output <- capture.output(cudd_bdd_print_debug(bdd_one, nvars = 1L, verbosity = 2L))
  expect_snapshot(debug_output)
})

test_that("ADD operations and conversions return expected classes", {
  manager <- CuddManager()

  add_one <- cudd_add_one(manager)
  add_zero <- cudd_add_zero(manager)
  add_var <- cudd_add_var(manager)

  expect_s4_class(add_one, "CuddADD")
  expect_s4_class(add_zero, "CuddADD")
  expect_s4_class(add_var, "CuddADD")

  expect_s4_class(add_one + add_zero, "CuddADD")
  expect_s4_class(add_one * add_zero, "CuddADD")
  expect_error(add_one ^ add_zero, "XOR")
  expect_error(!add_one, "Negation")

  bdd_from_add <- cudd_add_to_bdd(add_one)
  expect_s4_class(bdd_from_add, "CuddBDD")

  output <- capture.output(cudd_add_epd_print_minterm(add_one, 0L))
  expect_snapshot(output)

  print_output <- capture.output(print(add_one))
  expect_snapshot(print_output)
})

test_that("ZDD operations and conversions return expected classes", {
  manager <- CuddManager()

  zdd_one <- cudd_zdd_one(manager, 0L)
  zdd_zero <- cudd_zdd_zero(manager)
  zdd_var <- cudd_zdd_var(manager)

  expect_s4_class(zdd_one, "CuddZDD")
  expect_s4_class(zdd_zero, "CuddZDD")
  expect_s4_class(zdd_var, "CuddZDD")

  expect_s4_class(zdd_one + zdd_zero, "CuddZDD")
  expect_s4_class(zdd_one * zdd_zero, "CuddZDD")
  expect_error(zdd_one ^ zdd_zero, "XOR")
  expect_error(!zdd_one, "Negation")

  bdd_from_zdd <- cudd_zdd_to_bdd(zdd_var)
  expect_s4_class(bdd_from_zdd, "CuddBDD")

  print_output <- capture.output(print(zdd_one))
  expect_snapshot(print_output)
})
