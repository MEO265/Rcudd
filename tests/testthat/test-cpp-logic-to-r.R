test_that("one vectorized reverse converter handles single and multiple expressions", {
  converted <- expressions_to_cpp_logic(quote(a > b), quote(a <= b))
  result <- cpp_logic_to_r(converted$expressions, converted$placeholders, converted$order_variables)
  expect_identical(result, c("a > b", "a <= b"))
  expect_identical(cpp_logic_to_r("true"), "TRUE")
  expect_identical(cpp_logic_to_r(character()), character())
})
