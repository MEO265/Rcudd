test_that("standalone reduction returns an expression and retains metadata", {
  result <- bdd_literals("(x & y) | (x & !y)")

  expect_named(result, c("constant", "variables", "initial_variables", "reduced_expression"))
  expect_identical(result$constant, NA)
  expect_identical(result$variables, "x")
  expect_identical(result$initial_variables, c("x", "y"))
  expect_identical(result$reduced_expression, "x")
  expect_identical(bdd_literals("x & x")$reduced_expression, "x")
  expect_identical(bdd_literals("NA")$reduced_expression, "NA")
})

test_that("constant diagrams render as parser-compatible constants", {
  for (expr in c("true", "x | !x")) {
    expect_identical(bdd_literals(expr)$reduced_expression, "true")
    expect_identical(bdd_restrict_chain(expr)[[1L]]$reduced_expression, "true")
  }
  for (expr in c("false", "x & !x")) {
    expect_identical(bdd_literals(expr)$reduced_expression, "false")
    expect_identical(bdd_restrict_chain(expr)[[1L]]$reduced_expression, "false")
  }
})

test_that("standalone expressions round-trip and agree on all boolean inputs", {
  exprs <- c(
    "z & a", "z | !a", "(z & a) | (!z & b)",
    "!(z & (a | b))", "(z ^ a) | (b & !z)"
  )
  inputs <- expand.grid(z = c(FALSE, TRUE), a = c(FALSE, TRUE), b = c(FALSE, TRUE))
  evaluate <- function(expr) {
    return(eval(str2lang(gsub("^", "!=", expr, fixed = TRUE)), inputs))
  }

  for (expr in exprs) {
    reduced <- bdd_literals(expr)$reduced_expression
    expect_type(reduced, "character")
    expect_length(reduced, 1L)
    expect_identical(evaluate(reduced), evaluate(expr), info = expr)
    expect_false(
      bdd_literals(paste0("(", expr, ") ^ (", reduced, ")"))$constant,
      info = expr
    )
  }
})

test_that("earlier branches remove covered terms from later expressions", {
  result <- bdd_restrict_chain(c("y | x", "(x | y | z) & w"))

  expect_identical(result[[2L]]$reduced_expression, "z & w")
  expect_identical(result[[2L]]$initial_variables, c("w", "x", "y", "z"))
  expect_identical(result[[2L]]$variables, c("w", "z"))

  covered <- bdd_restrict_chain(c("x", "x | y", "!y", "z"))
  expect_identical(
    vapply(covered, function(entry) entry$reduced_expression, character(1L)),
    c("x", "y", "true", "false")
  )
})

test_that("shared constraints are reflected in the rendered expression", {
  expect_identical(bdd_restrict_chain("x | y", "!x")[[1L]]$reduced_expression, "y")
  expect_identical(bdd_restrict_chain("x", c("!x | z", "!z"))[[1L]]$reduced_expression, "false")
  expect_identical(bdd_restrict_chain("true", c("x", "!x"))[[1L]]$reduced_expression, "false")
})

test_that("long names and complemented comparison diagrams render correctly", {
  # These exercise repeated growth of CUDD's factored-form string buffer.
  expr <- "!(G_n1_n2 | E_n1_n2)"
  reduced <- bdd_literals(expr)$reduced_expression
  expect_false(bdd_literals(paste0("(", expr, ") ^ (", reduced, ")"))$constant)

  name <- paste(rep("long_variable", 100L), collapse = "_")
  expect_identical(bdd_literals(name)$reduced_expression, name)
  expect_identical(bdd_literals(paste0("!", name))$reduced_expression, paste0("!", name))
})

test_that("rendered chain expressions agree with their inputs within each context", {
  exprs <- c("x | y", "z | w", "(!x | z) & (!y | w)", "!z")
  constraints <- c("!x | z", "!w | y")
  inputs <- expand.grid(x = c(FALSE, TRUE), y = c(FALSE, TRUE),
                        z = c(FALSE, TRUE), w = c(FALSE, TRUE))
  inputs$true <- TRUE
  inputs$false <- FALSE
  evaluate <- function(expr) {
    return(rep_len(eval(str2lang(expr), inputs), nrow(inputs)))
  }
  context <- Reduce(`&`, lapply(constraints, evaluate))
  result <- bdd_restrict_chain(exprs, constraints)

  for (i in seq_along(exprs)) {
    original <- evaluate(exprs[[i]])
    rendered <- evaluate(result[[i]]$reduced_expression)
    expect_identical(rendered[context], original[context], info = paste("branch", i))
    if (!any(context)) {
      expect_identical(result[[i]]$reduced_expression, "false")
    }
    context <- context & !original
  }
})

test_that("R expression chains expose readable formulas and retain C++ strings", {
  result <- bdd_restrict_from_expressions(
    list(quote(y || x), quote((x || y || z) && w))
  )
  expect_identical(result$chain[[2L]]$reduced_expression, "z && w")
  expect_identical(result$chain[[2L]]$reduced_cpp_expression, "p3 & p4")
  expect_identical(result$placeholders, c(p1 = "y", p2 = "x", p3 = "z", p4 = "w"))
})
