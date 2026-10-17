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
  expect_identical(result$cost, 1L)
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
    expect_true(is.na(missing$cost))
    expect_true(is.na(missing$operator_count))
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
  corrupted[[2L]] <- paste(3L, -case$problem$gates[[1L]]$kind[["not"]], 0L)
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
  bad <- case$problem; bad$care_expression <- "false"
  expect_error(interpret_rwbo_output(bad, case$output), "Care metadata failed the BDD equivalence")
  bad <- case$problem; bad$care_expression <- "x"
  expect_error(interpret_rwbo_output(bad, case$output), "Care metadata failed the BDD equivalence")
  bad <- case$problem; bad$care_expression <- "z"
  expect_error(interpret_rwbo_output(bad, case$output), "Care expression has support outside")
  bad <- case$problem; bad$expression <- "!z"
  expect_error(interpret_rwbo_output(bad, case$output), "Original expression under care has support outside")
  expect_error(interpret_rwbo_output(case$problem, NA_character_), "non-missing")
})

test_that("contextual shortcuts are verified against original conditions and context", {
  file <- tempfile(fileext = ".wcnf")
  on.exit(unlink(file))
  problem <- bdd_to_wcnf("a & b", file, care = "!(c ^ (a & b))")
  expect_identical(problem$shortcut, "c")
  result <- interpret_rwbo_output(problem, "")
  expect_identical(result$expression, "c")
  expect_true(result$verified)
  expect_true(result$optimal)
  expect_identical(result$operator_count, 0L)
  expect_identical(result$cost, 0L)
  bad <- problem; bad$care_expression <- "true"
  expect_error(interpret_rwbo_output(bad, ""), "BDD equivalence")
  bad <- problem; bad$care[[1L]] <- NA
  expect_error(interpret_rwbo_output(bad, ""), "Care metadata cannot contain NA")
  bad <- problem; bad$care <- logical(2)
  expect_error(interpret_rwbo_output(bad, ""), "care metadata dimensions")
  empty <- bdd_to_wcnf("a ^ b", file, care = "false")
  empty_result <- interpret_rwbo_output(empty, "")
  expect_identical(empty_result$expression, "false")
  expect_true(empty_result$verified)
  expect_true(empty_result$optimal)
})

test_that("metadata binding permits eliminated variables outside care", {
  file <- tempfile(fileext = ".wcnf")
  on.exit(unlink(file))
  problem <- bdd_to_wcnf("a", file, care = "!b")
  # Mirrors chain preparation: target comes from the restricted root while
  # expression is the original condition, with another variable outside care.
  problem$expression <- "a ^ (b & c)"
  expect_false("c" %in% problem$variables)
  result <- interpret_rwbo_output(problem, "")
  expect_identical(result$expression, "a")
  expect_true(result$verified)
  expect_true(result$optimal)
  problem <- bdd_to_wcnf("!x", file, max_operators = 1L, care = "z | !z")
  expect_identical(problem$care, logical())
  expect_false("z" %in% problem$variables)
  expect_identical(problem$care_expression, "z | !z")
})

test_that("context leaves all gate semantics constrained outside its root rows", {
  file <- tempfile(fileext = ".wcnf")
  on.exit(unlink(file))
  problem <- bdd_to_wcnf("a ^ b", file, max_operators = 1L, care = "c")
  gate <- problem$gates[[1L]]
  model <- -seq_len(problem$nvars)
  positive <- c(gate$kind[["xor"]], gate$left[[3L]], gate$right[[4L]],
                gate$left_value[problem$inputs$a], gate$right_value[problem$inputs$b],
                gate$out[problem$target])
  model[positive] <- positive
  output <- function(model) paste("s OPTIMUM FOUND\no 2\nv", paste(model, collapse = " "), "0")
  result <- interpret_rwbo_output(problem, output(model))
  expect_identical(result$expression, "a ^ b")
  expect_identical(result$operator_count, 1L)
  expect_identical(result$cost, 2L)
  expect_true(result$optimal)
  expect_error(interpret_rwbo_output(problem, sub("o 2", "o 1", output(model))), "cost does not match")
  outside <- which(!problem$care)[[1L]]
  id <- gate$out[[outside]]
  model[[id]] <- -model[[id]]
  expect_error(interpret_rwbo_output(problem, output(model)), "hard clause")
})

test_that("the interpreter rejects weights or costs inconsistent with the objective", {
  case <- make_not_case()
  on.exit(unlink(case$problem$wcnf_file))
  lines <- readLines(case$problem$wcnf_file)
  bad <- lines
  bad[[length(bad) - 1L]] <- sub("^1 ", "2 ", bad[[length(bad) - 1L]])
  writeLines(bad, case$problem$wcnf_file)
  expect_error(interpret_rwbo_output(case$problem, case$output), "clause weight")
  bad <- lines
  bad[[1L]] <- sub(" 3$", " 2", bad[[1L]])
  writeLines(bad, case$problem$wcnf_file)
  expect_error(interpret_rwbo_output(case$problem, case$output), "does not match the saved problem")
  # A satisfied replacement soft clause hides the NOT cost in the file. The
  # decoded operator costs still reject this forged objective and solver cost.
  bad <- lines
  bad[[length(bad) - 1L]] <- tail(lines, 1L)
  writeLines(bad, case$problem$wcnf_file)
  expect_error(interpret_rwbo_output(case$problem, sub("o 1", "o 0", case$output)),
               "cost does not match the decoded formula")
})


test_that("binary-counter metadata is validated before solver models", {
  file <- tempfile(fileext = ".wcnf")
  on.exit(unlink(file))
  problem <- bdd_to_wcnf("a & b & c", file, max_operators = 4L)
  expect_false(interpret_rwbo_output(problem, "s UNKNOWN")$verified)
  bad <- problem; bad$binary_lower_bound <- 1L
  expect_error(interpret_rwbo_output(bad, "s UNKNOWN"), "count does not match|lower-bound")
  bad <- problem; bad$binary_lower_bound <- 5L
  expect_error(interpret_rwbo_output(bad, "s UNKNOWN"), "exceeds gate count")
  bad <- problem; bad$binary_auxiliary <- integer()
  expect_error(interpret_rwbo_output(bad, "s UNKNOWN"), "metadata")
  bad <- problem; bad$binary_auxiliary[[1L]] <- bad$gates[[1L]]$kind[[1L]]
  expect_error(interpret_rwbo_output(bad, "s UNKNOWN"), "SAT-id metadata")
  bad <- problem; bad$binary_auxiliary[[1L]] <- NA_integer_
  expect_error(interpret_rwbo_output(bad, "s UNKNOWN"), "SAT-id metadata")
  bad <- problem; bad$nvars <- bad$nvars + 1L
  expect_error(interpret_rwbo_output(bad, "s UNKNOWN"), "count does not match")
  bad <- problem; bad$nclauses <- bad$nclauses + 1L
  expect_error(interpret_rwbo_output(bad, "s UNKNOWN"), "Clause count")
  shortcut <- bdd_to_wcnf("a", file)
  bad <- shortcut; bad$binary_lower_bound <- 1L
  expect_error(interpret_rwbo_output(bad, ""), "binary lower-bound")
})


test_that("complete counter models preserve decoded costs and equivalence", {
  file <- tempfile(fileext = ".wcnf")
  on.exit(unlink(file))
  problem <- bdd_to_wcnf("a & b & c", file, max_operators = 4L)
  model <- -seq_len(problem$nvars)
  first <- problem$gates[[3L]]
  root <- problem$gates[[4L]]
  ab <- problem$inputs$a & problem$inputs$b
  positive <- c(problem$gates[[1L]]$kind[["unused"]],
                problem$gates[[2L]]$kind[["unused"]],
                first$kind[["and"]], first$left[[3L]], first$right[[4L]],
                first$left_value[problem$inputs$a], first$right_value[problem$inputs$b],
                first$out[ab], root$kind[["and"]], root$left[[5L]], root$right[[8L]],
                root$left_value[problem$inputs$c], root$right_value[ab], root$out[problem$target],
                # Actual prefix counts satisfy S(3,1) and S(4,2).
                problem$binary_auxiliary[c(3L, 6L)])
  model[positive] <- positive
  output <- function(model) paste("s OPTIMUM FOUND\no 4\nv", paste(model, collapse = " "), "0")
  result <- interpret_rwbo_output(problem, output(model))
  expect_true(result$verified)
  expect_true(result$optimal)
  expect_identical(result$operator_count, 2L)
  expect_identical(result$cost, 4L)
  model[[tail(problem$binary_auxiliary, 1L)]] <- -tail(problem$binary_auxiliary, 1L)
  expect_error(interpret_rwbo_output(problem, output(model)), "hard clause")
})
