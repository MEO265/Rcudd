test_that("expression_to_cpp_logic converts expressions with placeholders", {
  result <- expression_to_cpp_logic(quote(((x && y) || f(z)) && w))

  expect_identical(result$expression, "(((p1 & p2) | p3) & p4)")
  expect_identical(result$placeholders, c(p1 = "x", p2 = "y", p3 = "f(z)", p4 = "w"))
})

test_that("placeholders can be disabled while keeping mapping", {
  result <- expression_to_cpp_logic("x && y", use_placeholders = FALSE)

  expect_identical(result$expression, "(x & y)")
  expect_identical(result$placeholders, c(p1 = "x", p2 = "y"))
})

test_that("expressions_to_cpp_logic shares placeholders across inputs", {
  result <- expressions_to_cpp_logic(quote(x && y), quote(!(x && y)))

  expect_identical(result$expressions, c("(p1 & p2)", "!(p1 & p2)"))
  expect_identical(result$placeholders, c(p1 = "x", p2 = "y"))
})

test_that("expressions_to_cpp_logic validates inputs", {
  expect_error(expressions_to_cpp_logic(), "At least one expression must be supplied")
})

test_that("logical constants are rewritten to lowercase", {
  result <- expression_to_cpp_logic(quote(TRUE || FALSE))

  expect_identical(result$expression, "(true | false)")
  expect_identical(result$placeholders, setNames(character(0L), character(0L)))
})

test_that("constant-only inputs keep shared placeholder mapping shape", {
  result <- expressions_to_cpp_logic(quote(TRUE), quote(FALSE))

  expect_identical(result$expressions, c("true", "false"))
  expect_identical(result$placeholders, setNames(character(0L), character(0L)))
})

test_that("logical constants are accepted without quoting", {
  multi_result <- expressions_to_cpp_logic(TRUE, FALSE)
  expect_identical(multi_result$expressions, c("true", "false"))
  expect_identical(multi_result$placeholders, setNames(character(0L), character(0L)))

  single_result <- expression_to_cpp_logic(TRUE)
  expect_identical(single_result$expression, "true")
  expect_identical(single_result$placeholders, setNames(character(0L), character(0L)))
})

test_that("placeholders remain stable for redundant subexpressions", {
  expr <- quote((x && y) || (y && !x))
  result <- expression_to_cpp_logic(expr)

  expect_identical(result$expression, "((p1 & p2) | (p2 & !p1))")
  expect_identical(result$placeholders, c(p1 = "x", p2 = "y"))
})

test_that("standalone symbols convert to single placeholders", {
  result <- expression_to_cpp_logic(quote(x))

  expect_identical(result$expression, "p1")
  expect_identical(result$placeholders, c(p1 = "x"))
})

test_that("placeholder mapping persists when placeholders are disabled", {
  expr <- quote((x && y) || (f(z) && w))
  result <- expressions_to_cpp_logic(expr, use_placeholders = FALSE)

  expect_identical(result$expressions, "((x & y) | (f(z) & w))")
  expect_identical(result$placeholders, c(p1 = "x", p2 = "y", p3 = "f(z)", p4 = "w"))
})

test_that("comparisons are converted to ordered placeholders", {
  result <- expression_to_cpp_logic(quote(x1 < x2 && x2 >= x1))

  expect_identical(result$expression, "(L_n1_n2 & (L_n1_n2 | E_n1_n2))")
  expect_identical(result$order_variables[order(names(result$order_variables))], c(n1 = "x1", n2 = "x2"))
  expect_identical(result$placeholders, setNames(character(0L), character(0L)))
})

test_that("expressions with only comparisons return empty placeholders", {
  result <- expression_to_cpp_logic("a < b")

  expect_identical(result$expression, "L_n1_n2")
  expect_identical(result$order_variables[order(names(result$order_variables))], c(n1 = "a", n2 = "b"))
  expect_identical(result$placeholders, setNames(character(0L), character(0L)))
})

test_that("expressions without comparisons have empty order variables", {
  result <- expression_to_cpp_logic("x || y")

  expect_identical(result$expression, "(p1 | p2)")
  expect_identical(result$order_variables, setNames(character(0L), character(0L)))
  expect_identical(result$placeholders, c(p1 = "x", p2 = "y"))
})

test_that("symmetric comparisons reuse canonical ordering", {
  result <- expressions_to_cpp_logic(quote(y > x), quote(x <= y))

  expect_identical(result$expressions, c("L_n1_n2", "(L_n1_n2 | E_n1_n2)"))
  expect_identical(result$order_variables[order(names(result$order_variables))], c(n1 = "x", n2 = "y"))
})

test_that("negated comparisons reuse the canonical order", {
  # nolint next
  result <- expression_to_cpp_logic(quote(!(b <= a)))

  expect_identical(result$expression, "!(G_n1_n2 | E_n1_n2)")
  expect_identical(result$order_variables[order(names(result$order_variables))], c(n1 = "a", n2 = "b"))
})

test_that("comparison placeholders stay consistent across transitive chains", {
  result <- expression_to_cpp_logic(quote((a < b) && (b < c) && (a < c)))

  expect_identical(result$expression, "((L_n1_n2 & L_n2_n3) & L_n1_n3)")
  expect_identical(result$order_variables[order(names(result$order_variables))], c(n1 = "a", n2 = "b", n3 = "c"))
})
