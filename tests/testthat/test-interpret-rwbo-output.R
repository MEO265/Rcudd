make_not_case <- function() {
  problem <- bdd_to_wcnf("!x", tempfile(fileext = ".wcnf"), max_operators = 1L)
  gate <- problem$gates[[1L]]
  model <- -seq_len(problem$nvars)
  positive <- c(gate$kind[["not"]], gate$left[[3L]], gate$left_value[[2L]], gate$out[[1L]])
  model[positive] <- positive
  output <- paste("s OPTIMUM FOUND", "o 1", paste("v", paste(model, collapse = " "), "0"), sep = "\n")
  list(problem = problem, model = model, output = output)
}

test_that("native interpretation validates and decodes a complete formula", {
  case <- make_not_case()
  on.exit(unlink(case$problem$wcnf_file))
  result <- interpret_rwbo_output(case$problem, case$output)
  expect_identical(result$expression, "!x")
  expect_identical(result$operator_count, 1L)
  expect_true(result$verified)
  expect_true(result$optimal)
  candidate <- interpret_rwbo_output(case$problem, sub("OPTIMUM FOUND", "SATISFIABLE", case$output))
  expect_true(candidate$verified)
  expect_false(candidate$optimal)
  for (output in c("s UNKNOWN", "s UNSATISFIABLE", "s OPTIMUM FOUND\no 1")) {
    missing <- interpret_rwbo_output(case$problem, output)
    expect_false(missing$verified)
    expect_false(missing$optimal)
    expect_true(is.na(missing$expression))
  }
})

test_that("malformed models, objective values and WCNF files fail", {
  case <- make_not_case()
  on.exit(unlink(case$problem$wcnf_file))
  model_output <- function(model) paste("s OPTIMUM FOUND\no 1\nv", paste(model, collapse = " "), "0")
  expect_error(interpret_rwbo_output(case$problem, model_output(case$model[-1L])), "incomplete or invalid")
  expect_error(interpret_rwbo_output(case$problem, model_output(c(case$model, case$model[[1L]]))), "duplicate/conflicting")
  expect_error(interpret_rwbo_output(case$problem, model_output(c(case$model, -case$model[[1L]]))), "duplicate/conflicting")
  case$model[[1L]] <- case$problem$nvars + 1L
  expect_error(interpret_rwbo_output(case$problem, model_output(case$model)), "out-of-range")
  expect_error(interpret_rwbo_output(case$problem, sub("o 1", "o 99", case$output)), "cost does not match")
  expect_error(interpret_rwbo_output(case$problem, sub("o 1", "o -1", case$output)), "objective")
  expect_error(interpret_rwbo_output(case$problem, sub("o 1", "o 1.5", case$output)), "Invalid integer")
  lines <- readLines(case$problem$wcnf_file)
  corrupted <- lines
  corrupted[[2L]] <- paste(2L, -case$problem$gates[[1L]]$kind[["not"]], 0L)
  writeLines(corrupted, case$problem$wcnf_file)
  expect_error(interpret_rwbo_output(case$problem, case$output), "hard clause")
  writeLines(lines[-length(lines)], case$problem$wcnf_file)
  expect_error(interpret_rwbo_output(case$problem, case$output), "clause count")
})

test_that("metadata is validated before native indexing or claims of optimality", {
  case <- make_not_case()
  on.exit(unlink(case$problem$wcnf_file))
  bad <- case$problem; bad$gates[[1L]]$left <- integer()
  expect_error(interpret_rwbo_output(bad, case$output), "metadata")
  bad <- case$problem; bad$gates[[1L]]$left[[1L]] <- NA_integer_
  expect_error(interpret_rwbo_output(bad, case$output), "metadata")
  bad <- case$problem; bad$gates[[1L]]$kind[[1L]] <- bad$gates[[1L]]$kind[[2L]]
  expect_error(interpret_rwbo_output(bad, case$output), "metadata")
  bad <- case$problem; bad$nvars <- .Machine$integer.max - 1L
  expect_error(interpret_rwbo_output(bad, case$output), "count does not match")
  bad <- case$problem; bad$target[[1L]] <- NA
  expect_error(interpret_rwbo_output(bad, case$output), "cannot contain NA")
  bad <- case$problem; bad$target <- logical()
  expect_error(interpret_rwbo_output(bad, case$output), "metadata")
  bad <- case$problem; bad$shortcut <- "true"
  expect_error(interpret_rwbo_output(bad, case$output), "metadata")
  bad <- case$problem; bad$lower_bound <- 0L
  expect_error(interpret_rwbo_output(bad, case$output), "lower-bound")
  bad <- case$problem; bad$gates[[1L]]$right <- c(bad$gates[[1L]]$right, 1L)
  expect_error(interpret_rwbo_output(bad, case$output), "metadata")
  bad <- case$problem; bad$gates[[1L]] <- NULL
  expect_error(interpret_rwbo_output(bad, case$output), "UNSAT metadata")
  shortcut <- bdd_to_wcnf("x", tempfile(), max_operators = 0L)
  on.exit(unlink(shortcut$wcnf_file), add = TRUE)
  bad <- shortcut; bad$shortcut <- "false"
  expect_error(interpret_rwbo_output(bad, ""), "Shortcut does not match")
  bad <- shortcut; bad$expression <- "!x"
  expect_error(interpret_rwbo_output(bad, ""), "BDD equivalence")
  expect_error(interpret_rwbo_output(case$problem, NA_character_), "non-missing")
})