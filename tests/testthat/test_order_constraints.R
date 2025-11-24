test_that("reflexive equality and pairwise completeness are generated", {
  result <- order_constraints(c("n2", "n1"))

  expect_setequal(
    result,
    c(
      "E_n1_n1",
      "E_n2_n2",
      "(G_n1_n2 | L_n1_n2 | E_n1_n2)",
      "!(G_n1_n2 & L_n1_n2)",
      "!(G_n1_n2 & E_n1_n2)",
      "!(L_n1_n2 & E_n1_n2)"
    )
  )
})

test_that("transitivity rules for three tokens are included", {
  result <- order_constraints(c("n1", "n2", "n3"))

  transitivity <- result[startsWith(prefix = "!(", result) & grepl("\\) \\| [GLE]_n1_n3$", result)]

  expect_setequal(
    transitivity,
    c(
      "!(L_n1_n2 & L_n2_n3) | L_n1_n3",
      "!(G_n1_n2 & G_n2_n3) | G_n1_n3",
      "!(E_n1_n2 & E_n2_n3) | E_n1_n3",
      "!(L_n1_n2 & E_n2_n3) | L_n1_n3",
      "!(E_n1_n2 & L_n2_n3) | L_n1_n3",
      "!(G_n1_n2 & E_n2_n3) | G_n1_n3",
      "!(E_n1_n2 & G_n2_n3) | G_n1_n3"
    )
  )
})

test_that("transitivity scales beyond three tokens", {
  result <- order_constraints(c("n3", "n1", "n2", "n4"))

  transitivity <- result[startsWith(prefix = "!(", result) & grepl("\\) \\| [GLE]_", result)]

  expect_length(transitivity, 28L)
  expect_true("!(L_n1_n2 & L_n2_n3) | L_n1_n3" %in% transitivity)
  expect_true("!(G_n1_n2 & G_n2_n4) | G_n1_n4" %in% transitivity)
  expect_true("!(E_n1_n3 & G_n3_n4) | G_n1_n4" %in% transitivity)
})

test_that("transitivity rules are added for non-ascending token names", {
  result <- order_constraints(c("z", "a", "m"))

  transitivity <- result[startsWith(prefix = "!(", result) & grepl("\\) \\| [GLE]_a_z$", result)]

  expect_setequal(
    transitivity,
    c(
      "!(L_a_m & L_m_z) | L_a_z",
      "!(G_a_m & G_m_z) | G_a_z",
      "!(E_a_m & E_m_z) | E_a_z",
      "!(L_a_m & E_m_z) | L_a_z",
      "!(E_a_m & L_m_z) | L_a_z",
      "!(G_a_m & E_m_z) | G_a_z",
      "!(E_a_m & G_m_z) | G_a_z"
    )
  )
})

test_that("empty or invalid inputs are handled", {
  expect_identical(order_constraints(character()), character())
  expect_identical(order_constraints(c(NA, "")), character())
})

test_that("single-token inputs return only reflexive equality", {
  expect_identical(order_constraints("n1"), "E_n1_n1")
})

test_that("redundant token names are deduplicated", {
  tokens <- c("n2", "n2", "n1", "n1")

  constraints <- order_constraints(tokens)

  expect_setequal(constraints[startsWith(prefix = "E_", constraints)], c("E_n1_n1", "E_n2_n2"))
  expect_true("(G_n1_n2 | L_n1_n2 | E_n1_n2)" %in% constraints)
})
