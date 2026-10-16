test_that("comparison placeholders follow token order after late variable discovery", {
  result <- expressions_to_cpp_logic(
    quote(b < c), quote(a < b), quote(a >= c), quote(b > a)
  )

  expect_identical(result$order_variables, c(n1 = "b", n2 = "c", n3 = "a"))
  expect_identical(
    result$expressions,
    c("L_n1_n2", "G_n1_n3", "(L_n2_n3 | E_n2_n3)", "G_n1_n3")
  )
})

test_that("comparison placeholders match ordering rules beyond nine tokens", {
  result <- expressions_to_cpp_logic(
    quote(a < b), quote(c < d), quote(e < f), quote(g < h), quote(i < j),
    quote(b < j), quote(j > b)
  )

  expect_identical(result$order_variables[["n10"]], "j")
  expect_identical(result$expressions[6:7], rep("G_n10_n2", 2L))
  expect_true(
    any(grepl("G_n10_n2", order_constraints(names(result$order_variables)), fixed = TRUE))
  )
})

test_that("joint restrictions retain relationships through auxiliary variables", {
  for (constraints in list(c("!x | z", "!z"), c("!z", "!x | z"))) {
    result <- bdd_restrict_chain("x", constraints)[[1L]]

    expect_false(result$constant)
    expect_identical(result$variables, character())
    expect_identical(result$initial_variables, "x")
  }

  implied <- bdd_restrict_chain("x", c("x | z", "!z"))[[1L]]
  expect_true(implied$constant)
})

test_that("constraint prefixes retain all earlier assumptions", {
  constraints <- c("!x | z", "!z | w", "!w")
  constants <- vapply(seq_along(constraints), function(i) {
    return(bdd_restrict_chain("x", constraints[seq_len(i)])[[1L]]$constant)
  }, logical(1L))

  expect_identical(constants, c(NA, NA, FALSE))

  constraints[[1L]] <- "x | z"
  constants <- vapply(seq_along(constraints), function(i) {
    return(bdd_restrict_chain("x", constraints[seq_len(i)])[[1L]]$constant)
  }, logical(1L))

  expect_identical(constants, c(NA, NA, TRUE))
})

test_that("all previous expressions contribute to the restriction context", {
  excluded <- bdd_restrict_chain(c("x & !z", "z", "x"))[[3L]]
  expect_false(excluded$constant)
  expect_identical(excluded$initial_variables, "x")

  implied <- bdd_restrict_chain(c("!x & !z", "z", "x"))[[3L]]
  expect_true(implied$constant)
})

test_that("previous expressions and shared constraints are considered together", {
  excluded <- bdd_restrict_chain(c("x & !z", "x"), "!z")[[2L]]
  expect_false(excluded$constant)

  implied <- bdd_restrict_chain(c("!x & !z", "x"), "!z")[[2L]]
  expect_true(implied$constant)
})

test_that("inconsistent shared constraints reduce every expression to FALSE", {
  for (constraints in list(c("x", "!x"), c("!x", "x"))) {
    for (expr in c("true", "false", "x")) {
      result <- bdd_restrict_chain(expr, constraints)[[1L]]
      expect_false(result$constant)
      expect_identical(result$variables, character())
    }
  }

  expect_identical(
    bdd_restrict_chain("x", c("x", "!x"))[[1L]]$initial_variables, "x"
  )
  expect_false(bdd_restrict_chain("true", "false")[[1L]]$constant)
})

test_that("unreachable positions in the chain reduce to FALSE", {
  expect_false(bdd_restrict_chain(c("true", "true"))[[2L]]$constant)

  unreachable <- bdd_restrict_chain(c("x", "!x", "y"))[[3L]]
  expect_false(unreachable$constant)
  expect_identical(unreachable$variables, character())
  expect_identical(unreachable$initial_variables, "y")

  expect_false(bdd_restrict_chain(c("x", "y"), "x")[[2L]]$constant)
})

test_that("transitive contradictions are reduced in every clause order", {
  clauses <- c("b < c", "a < b", "a >= c")
  permutations <- list(c(1L, 2L, 3L), c(1L, 3L, 2L), c(2L, 1L, 3L),
                       c(2L, 3L, 1L), c(3L, 1L, 2L), c(3L, 2L, 1L))

  for (permutation in permutations) {
    expr <- paste(clauses[permutation], collapse = " && ")
    result <- bdd_restrict_from_expressions(list(expr))$chain[[1L]]

    expect_false(result$constant, info = expr)
    expect_identical(result$variables, character())
  }
})

test_that("joint ordering rules recognize equality, non-strict bounds and implications", {
  contradictions <- c(
    "a == b && b < c && a >= c",
    "a <= b && b <= c && a > c",
    "c > b && b > a && c <= a"
  )
  for (expr in contradictions) {
    result <- bdd_restrict_from_expressions(list(expr))$chain[[1L]]
    expect_false(result$constant, info = expr)
  }

  tautology <- bdd_restrict_from_expressions(list("!(b < c && a < b) || a < c"))
  expect_true(tautology$chain[[1L]]$constant)

  feasible <- bdd_restrict_from_expressions(list("b < c && a < b && a < c"))
  expect_identical(feasible$chain[[1L]]$constant, NA)
})

test_that("three-variable relation combinations agree with concrete ordered values", {
  relations <- expand.grid(bc = c("<", "==", ">"),
                           ab = c("<", "==", ">"),
                           ac = c("<", "==", ">"), stringsAsFactors = FALSE)
  values <- expand.grid(a = 1:3, b = 1:3, c = 1:3)

  for (i in seq_len(nrow(relations))) {
    relation <- relations[i, ]
    feasible <- any(
      do.call(relation$bc, list(values$b, values$c)) &
        do.call(relation$ab, list(values$a, values$b)) &
        do.call(relation$ac, list(values$a, values$c))
    )
    expr <- sprintf("b %s c && a %s b && a %s c", relation$bc, relation$ab, relation$ac)
    result <- bdd_restrict_from_expressions(list(expr))$chain[[1L]]

    expect_identical(isFALSE(result$constant), !feasible, info = expr)
  }
})
