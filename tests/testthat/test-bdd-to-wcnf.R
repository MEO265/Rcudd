# Native exporter tests do not require Rwbo; the script integration tests run it.
test_that("native export keeps CUDD indices separate from names and truth rows", {
  file <- tempfile(fileext = ".wcnf")
  on.exit(unlink(file))
  problem <- bdd_to_wcnf("(z & !z) | (b ^ a)", file, max_operators = 3L)
  expect_s3_class(problem, "rwbo_formula_problem")
  expect_identical(problem$variables, c("a", "b"))
  expect_identical(problem$variable_indices, c(2L, 1L))
  expect_identical(problem$inputs, expand.grid(a = c(FALSE, TRUE), b = c(FALSE, TRUE),
                                             KEEP.OUT.ATTRS = FALSE))
  expect_identical(problem$target, xor(problem$inputs$a, problem$inputs$b))
  expect_null(problem$bdd_expression)
  expect_match(problem$encoding, "native CUDD", fixed = TRUE)
  lines <- readLines(file)
  expect_equal(length(lines) - 1L, problem$nclauses)
  expect_true(all(grepl(" 0$", lines[-1L])))
})

test_that("native export supports constant and UTF-8 variable shortcuts", {
  file <- tempfile()
  on.exit(unlink(file))
  for (value in c("true", "false", "x", "\u00e4")) {
    problem <- bdd_to_wcnf(value, file, max_operators = 0L)
    expect_identical(problem$shortcut, value)
    expect_length(problem$gates, 0L)
    expect_identical(problem$nclauses, 2L)
    expect_identical(readLines(file), c("p wcnf 1 2 2", "2 1 0", "1 1 0"))
  }
  problem <- bdd_to_wcnf("!x", file, max_operators = 0L)
  expect_null(problem$shortcut)
  expect_identical(readLines(file), c("p wcnf 1 3 2", "2 1 0", "2 -1 0", "1 1 0"))
})

test_that("weighted operator costs use only unit-weight soft clauses", {
  file <- tempfile(fileext = ".wcnf")
  on.exit(unlink(file))
  problem <- bdd_to_wcnf("a ^ b", file, max_operators = 1L)
  lines <- readLines(file)
  expect_identical(lines[[1L]], "p wcnf 25 132 3")
  expect_identical(problem$nclauses, 132L)
  expect_identical(problem$objective, "weighted operator cost: ! = 1; &, | and ^ = 2")
  gate <- problem$gates[[1L]]
  expect_identical(tail(lines, 2L), c(
    paste(1L, gate$kind[["unused"]], 0L),
    paste(1L, gate$kind[["unused"]], gate$kind[["not"]], 0L)
  ))
  expect_true(all(startsWith(lines[-c(1L, length(lines) - 1L, length(lines))], "3 ")))

  problem <- bdd_to_wcnf("a ^ b", file, max_operators = 3L)
  lines <- readLines(file)
  expect_identical(problem$nclauses, 499L)
  weights <- as.integer(sub(" .*", "", lines[-1L]))
  expect_true(all(weights %in% c(1L, 7L)))
  expect_identical(sum(weights == 1L), 6L)
})

test_that("resource checks precede allocations and output changes", {
  file <- tempfile()
  on.exit(unlink(file))
  writeLines("sentinel", file)
  # Exact count is 499; coarse lower bound is smaller than this requested limit.
  expect_error(bdd_to_wcnf("a ^ b", file, 3L, max_clauses = 498L), "Clause limit")
  expect_identical(readLines(file), "sentinel")
  expect_error(bdd_to_wcnf("a ^ b", file, 1000000L), "Clause limit")
  expect_identical(readLines(file), "sentinel")
  expect_error(bdd_to_wcnf("a ^ b", file, max_variables = 1L), "truth-table limit")
  expect_error(bdd_to_wcnf("a", file, max_operators = -1L), "whole number")
  expect_error(bdd_to_wcnf("a", file, max_operators = Inf), "whole number")
  expect_error(bdd_to_wcnf("a", file, max_clauses = NA_real_), "whole number")
  expect_error(bdd_to_wcnf(NA_character_, file), "non-missing string")
  expect_error(bdd_to_wcnf("a &", file), "Unexpected end")
  expect_error(bdd_to_wcnf("a", file.path(tempfile(), "missing.wcnf")), "Cannot open")
})

test_that("context variables and root variables share one bounded synthesis pool", {
  file <- tempfile(fileext = ".wcnf")
  on.exit(unlink(file))
  problem <- bdd_to_wcnf("a ^ b", file, max_operators = 1L, care = "c")
  expect_identical(problem$variables, c("a", "b", "c"))
  expect_identical(problem$target, xor(problem$inputs$a, problem$inputs$b))
  expect_identical(problem$care, problem$inputs$c)
  expect_identical(problem$care_expression, "c")
  expect_identical(problem$lower_bound, 1L)
  expect_error(bdd_to_wcnf("a ^ b", file, max_variables = 2L, care = "c"), "truth-table limit")
  expect_error(bdd_to_wcnf("a", file, care = NA_character_), "non-missing string")
  expect_error(bdd_to_wcnf("a", file, care = "b &"), "Unexpected end")
  unconditional <- bdd_to_wcnf("a ^ b", file, care = "c | !c")
  expect_identical(unconditional$care, logical())
  expect_identical(unconditional$variables, c("a", "b"))
  # Compact BDD with the maximum pool: reject by clause preflight before
  # constructing any truth table or replacing the existing output file.
  writeLines("sentinel", file)
  parity <- paste0("x", seq_len(20L), collapse = " ^ ")
  expect_error(bdd_to_wcnf(parity, file, max_operators = 1000000L,
                           max_variables = 20L, care = "x1"), "Clause limit")
  expect_identical(readLines(file), "sentinel")
})
