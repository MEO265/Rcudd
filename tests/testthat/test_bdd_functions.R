test_that("bdd_literals reports constants and remaining variables", {
  redundant <- bdd_literals("x & !x")
  expect_false(redundant$constant)
  expect_identical(redundant$variables, character(0L))
  expect_identical(redundant$initial_variables, "x")

  simplified <- bdd_literals("(y & x) | (y & !x)")
  expect_true(is.na(simplified$constant))
  expect_identical(simplified$variables, "y")
  expect_identical(simplified$initial_variables, c("x", "y"))
})

test_that("bdd_literals handles tautologies and XORs", {
  tautology <- bdd_literals("x | !x")
  expect_true(tautology$constant)
  expect_identical(tautology$variables, character(0L))
  expect_identical(tautology$initial_variables, "x")

  xor_self <- bdd_literals("x ^ x")
  expect_false(xor_self$constant)
  expect_identical(xor_self$variables, character(0L))
  expect_identical(xor_self$initial_variables, "x")

  absorbed <- bdd_literals("x & (y | !y)")
  expect_true(is.na(absorbed$constant))
  expect_identical(absorbed$variables, "x")
  expect_identical(absorbed$initial_variables, c("x", "y"))
})

test_that("bdd_has_redundant_variables identifies removable literals", {
  expect_true(bdd_has_redundant_variables("x & !x"))
  expect_true(bdd_has_redundant_variables("(y & x) | (y & !x)"))
  expect_false(bdd_has_redundant_variables("x & y"))
  expect_true(bdd_has_redundant_variables("x | !x"))
})

test_that("bdd_restrict_chain applies chained restrictions", {
  chain_constant <- bdd_restrict_chain(c("x", "x & y"))
  expect_length(chain_constant, 2L)
  expect_true(is.na(chain_constant[[1L]]$constant))
  expect_identical(chain_constant[[1L]]$variables, "x")
  expect_identical(chain_constant[[1L]]$initial_variables, "x")

  expect_false(chain_constant[[2L]]$constant)
  expect_identical(chain_constant[[2L]]$variables, character(0L))
  expect_identical(chain_constant[[2L]]$initial_variables, c("x", "y"))

  chain_remaining <- bdd_restrict_chain(c("x", "y | x"))
  expect_true(is.na(chain_remaining[[1L]]$constant))
  expect_identical(chain_remaining[[1L]]$variables, "x")
  expect_identical(chain_remaining[[1L]]$initial_variables, "x")

  expect_true(is.na(chain_remaining[[2L]]$constant))
  expect_identical(chain_remaining[[2L]]$variables, "y")
  expect_identical(chain_remaining[[2L]]$initial_variables, c("x", "y"))
})

test_that("bdd_restrict_chain applies additional constraints after reductions", {
  chain <- bdd_restrict_chain(c("x | y", "y"), additional_constraints = "!y")

  expect_identical(chain[[1L]]$constant, NA)
  expect_identical(chain[[1L]]$variables, "x")
  expect_identical(chain[[1L]]$initial_variables, c("x", "y"))

  expect_false(chain[[2L]]$constant)
  expect_identical(chain[[2L]]$variables, character(0L))
  expect_identical(chain[[2L]]$initial_variables, "y")

  constrained <- bdd_restrict_chain("x | y", additional_constraints = c("!x", "!y"))
  expect_false(constrained[[1L]]$constant)
  expect_identical(constrained[[1L]]$variables, character(0L))
})

test_that("bdd_restrict_chain simplifies variables under constraints", {
  chain <- bdd_restrict_chain("x | y", additional_constraints = "!x")

  expect_identical(chain[[1L]]$constant, NA)
  expect_identical(chain[[1L]]$variables, "y")
  expect_identical(chain[[1L]]$initial_variables, c("x", "y"))
})

test_that("bdd_restrict_chain keeps initial variables from the raw expression", {
  expr_logic <- expressions_to_cpp_logic(quote((x || y && (!z || z))))$expressions
  chain <- bdd_restrict_chain(expr_logic)

  expect_length(chain, 1L)
  expect_identical(chain[[1L]]$constant, NA)
  expect_identical(chain[[1L]]$variables, c("p1", "p2"))
  expect_identical(chain[[1L]]$initial_variables, c("p1", "p2", "p3"))
})

test_that("single-element restrict chains preserve variables", {
  chain <- bdd_restrict_chain("x | !y")

  expect_length(chain, 1L)
  expect_identical(chain[[1L]]$constant, NA)
  expect_identical(chain[[1L]]$variables, c("x", "y"))
  expect_identical(chain[[1L]]$initial_variables, c("x", "y"))
})

test_that("bdd_restrict_chain reports initial variables per expression", {
  chain <- bdd_restrict_chain(c("x & y", "z | !z"))

  expect_identical(chain[[1L]]$initial_variables, c("x", "y"))
  expect_identical(chain[[1L]]$constant, NA)
  expect_identical(chain[[1L]]$variables, c("x", "y"))

  expect_identical(chain[[2L]]$initial_variables, "z")
  expect_true(chain[[2L]]$constant)
  expect_identical(chain[[2L]]$variables, character(0L))
})

test_that("bdd_restrict_chain validates additional_constraints", {
  expect_error(
    bdd_restrict_chain("x", additional_constraints = TRUE),
    "character"
  )
})

test_that("additional constraints can force contradictions", {
  chain <- bdd_restrict_chain("x", additional_constraints = "!x")

  expect_false(chain[[1L]]$constant)
  expect_identical(chain[[1L]]$variables, character(0L))
  expect_identical(chain[[1L]]$initial_variables, "x")
})

test_that("initial variables survive simplification during parsing", {
  expr_logic <- expressions_to_cpp_logic(
    quote((a && b) || (b && !a)),
    quote((x && y) || (y && !x))
  )$expressions

  chain <- bdd_restrict_chain(expr_logic)

  expect_length(chain, 2L)
  expect_identical(chain[[1L]]$initial_variables, c("p1", "p2"))
  expect_identical(chain[[1L]]$variables, "p2")
  expect_identical(chain[[1L]]$constant, NA)

  expect_identical(chain[[2L]]$initial_variables, c("p3", "p4"))
  expect_identical(chain[[2L]]$variables, "p4")
  expect_true(is.na(chain[[2L]]$constant))
})

test_that("restricting a constant expression keeps its initial variables", {
  chain <- bdd_restrict_chain(c("x & !x", "x | y"))

  expect_false(chain[[1L]]$constant)
  expect_identical(chain[[1L]]$variables, character(0L))
  expect_identical(chain[[1L]]$initial_variables, "x")

  expect_identical(chain[[2L]]$initial_variables, c("x", "y"))
  expect_identical(chain[[2L]]$variables, c("x", "y"))
  expect_true(is.na(chain[[2L]]$constant))
})

test_that("negated comparisons remain consistent in restriction", {
  # nolint next
  expr_logic <- expressions_to_cpp_logic(quote(!(x >= y)))$expressions
  chain <- bdd_restrict_chain(expr_logic)

  expect_setequal(chain[[1L]]$initial_variables, c("G_n1_n2", "E_n1_n2"))
  expect_setequal(chain[[1L]]$variables, c("G_n1_n2", "E_n1_n2"))
  expect_identical(chain[[1L]]$constant, NA)
})
