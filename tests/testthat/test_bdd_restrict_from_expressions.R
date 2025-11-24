test_that("order constraints make composite comparisons tautological", {
  result <- bdd_restrict_from_expressions(list(quote(x || y < z || y >= z)))

  expect_identical(result$expressions, "((p1 | L_n1_n2) | (G_n1_n2 | E_n1_n2))")
  expect_identical(result$placeholders, c(p1 = "x"))
  expect_identical(result$order_variables, c(n1 = "y", n2 = "z"))

  chain <- result$chain[[1L]]
  expect_true(chain$constant)
  expect_identical(chain$variables, character(0L))
  expect_identical(chain$initial_variables, c("E_n1_n2", "G_n1_n2", "L_n1_n2", "p1"))
})

test_that("constraints are optional when no comparisons exist", {
  result <- bdd_restrict_from_expressions(list(quote(x && y)))

  expect_identical(result$order_variables, setNames(character(0L), character(0L)))
  expect_identical(result$chain[[1L]]$initial_variables, c("p1", "p2"))
  expect_identical(result$chain[[1L]]$variables, c("p1", "p2"))
})

test_that("multiple expressions share placeholders and restrictions", {
  result <- bdd_restrict_from_expressions(list(quote(x && y), quote(y && x)))

  expect_identical(result$placeholders, c(p1 = "x", p2 = "y"))
  expect_identical(result$expressions, c("(p1 & p2)", "(p2 & p1)"))

  expect_true(is.na(result$chain[[1L]]$constant))
  expect_identical(result$chain[[1L]]$variables, c("p1", "p2"))

  expect_false(result$chain[[2L]]$constant)
  expect_identical(result$chain[[2L]]$variables, character(0L))
})

test_that("single expression inputs keep restriction metadata", {
  result <- bdd_restrict_from_expressions(list(quote(!x)))

  expect_identical(result$placeholders, c(p1 = "x"))
  expect_identical(result$chain[[1L]]$initial_variables, "p1")
  expect_identical(result$chain[[1L]]$variables, "p1")
  expect_identical(result$order_variables, setNames(character(0L), character(0L)))
})

test_that("negated comparisons remain canonical", {
  # nolint next
  result <- bdd_restrict_from_expressions(list(quote(!(b > a))))

  expect_identical(result$order_variables[order(names(result$order_variables))], c(n1 = "a", n2 = "b"))
  expect_identical(result$chain[[1L]]$initial_variables, "L_n1_n2")
  expect_identical(result$chain[[1L]]$variables, "L_n1_n2")
})

test_that("comparisons contribute order variables and constraints", {
  result <- bdd_restrict_from_expressions(list(quote((c < a) && (b >= a))))

  expect_identical(result$order_variables, c(n1 = "a", n2 = "c", n3 = "b"))

  initial <- result$chain[[1L]]$initial_variables
  expect_setequal(initial, c("G_n1_n2", "L_n1_n3", "E_n1_n3"))
})
