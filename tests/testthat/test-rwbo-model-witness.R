test_that("forward witnesses cover every operator with complete padded SAT assignments", {
  file <- tempfile(fileext = ".wcnf")
  on.exit(unlink(file))
  for (preference in c(TRUE, FALSE)) {
    problem <- bdd_to_wcnf("!((a ^ b) & (c | d))", file, max_operators = 6L,
                           prefer_negations = preference)
    witness <- rwbo_model_witness(problem, quote(!((a ^ b) & (c | d))))
    expect_identical(witness$operator_count, 4L)
    expect_identical(witness$cost, if (preference) 7L else 4L)
    expect_identical(abs(witness$model), seq_len(problem$nvars))
    expect_true(any(witness$model < 0L))
    expect_identical(witness$values, with(problem$inputs, !(xor(a, b) & (c | d))))
    for (chunk_size in list(NULL, 1L, 7L, problem$nvars + 1L)) {
      stdout <- rwbo_witness_output(witness, chunk_size = chunk_size)
      result <- interpret_rwbo_output(problem, stdout)
      expect_true(result$verified)
      expect_false(result$optimal)
      expect_identical(result$status, "SATISFIABLE")
      expect_identical(result$cost, witness$cost)
      expect_identical(result$operator_count, witness$operator_count)
    }
  }
})

test_that("Boolean constants and both XOR spellings become ordinary source selectors", {
  file <- tempfile(fileext = ".wcnf")
  on.exit(unlink(file))
  problem <- bdd_to_wcnf("a ^ b", file, max_operators = 5L)
  formulas <- list(quote(xor(a & TRUE, b | FALSE)), quote((a && TRUE) ^ (b || FALSE)))
  for (formula in formulas) {
    witness <- rwbo_model_witness(problem, formula)
    expect_identical(witness$operator_count, 3L)
    expect_identical(witness$cost, 6L)
    expect_identical(witness$values, xor(problem$inputs$a, problem$inputs$b))
    result <- interpret_rwbo_output(problem, rwbo_witness_output(witness))
    expect_true(result$verified)
    expect_false(result$optimal)
    # The first active AND has source TRUE before source a after port sorting.
    gate <- problem$gates[[3L]]
    expect_identical(witness$model[gate$left[[2L]]], gate$left[[2L]])
    expect_identical(witness$model[gate$right[[3L]]], gate$right[[3L]])
  }
  for (bit in c(TRUE, FALSE)) {
    shortcut <- bdd_to_wcnf(if (bit) "true" else "false", file)
    witness <- rwbo_model_witness(shortcut, bit)
    expect_identical(witness$model, 1L)
    expect_identical(witness$operator_count, 0L)
    expect_identical(witness$cost, 0L)
    expect_identical(witness$values, bit)
    expect_true(interpret_rwbo_output(shortcut, rwbo_witness_output(witness))$verified)
  }
})

test_that("unused prefixes and every counter level receive their actual prefix values", {
  file <- tempfile(fileext = ".wcnf")
  on.exit(unlink(file))
  problem <- bdd_to_wcnf("a & b & c & d & e", file, max_operators = 7L)
  witness <- rwbo_model_witness(problem, quote(a & b & c & d & e))
  expect_identical(witness$operator_count, 4L)
  expect_length(problem$binary_auxiliary, 16L)
  expect_identical(witness$model[problem$binary_auxiliary] > 0L,
                   rep(c(FALSE, FALSE, FALSE, TRUE), 4L))
  for (gate in problem$gates[seq_len(3L)]) {
    expect_identical(witness$model[gate$kind[["unused"]]], gate$kind[["unused"]])
    expect_true(all(witness$model[c(gate$left, gate$right)] < 0L))
    expect_true(all(witness$model[c(gate$left_value, gate$right_value, gate$out)] < 0L))
  }
  result <- interpret_rwbo_output(problem, rwbo_witness_output(witness))
  expect_true(result$verified)
  expect_false(result$optimal)
  damaged <- witness
  final_id <- tail(problem$binary_auxiliary, 1L)
  damaged$model[[final_id]] <- -final_id
  expect_error(interpret_rwbo_output(problem, rwbo_witness_output(damaged)), "hard clause")
})

test_that("repeated subtrees remain separate tree occurrences", {
  file <- tempfile(fileext = ".wcnf")
  on.exit(unlink(file))
  for (preference in c(TRUE, FALSE)) {
    problem <- bdd_to_wcnf("a & b", file, max_operators = 4L, prefer_negations = preference)
    witness <- rwbo_model_witness(problem, quote((a & b) | (a & b)))
    expect_identical(witness$operator_count, 3L)
    expect_identical(witness$cost, if (preference) 6L else 3L)
    expect_identical(witness$model[problem$gates[[2L]]$kind[["and"]]],
                     problem$gates[[2L]]$kind[["and"]])
    expect_identical(witness$model[problem$gates[[3L]]$kind[["and"]]],
                     problem$gates[[3L]]$kind[["and"]])
    expect_true(interpret_rwbo_output(problem, rwbo_witness_output(witness))$verified)
  }
})

test_that("witnesses never compare candidates with the advertised target", {
  file <- tempfile(fileext = ".wcnf")
  on.exit(unlink(file))
  problem <- bdd_to_wcnf("a ^ b", file, max_operators = 3L)
  witness <- rwbo_model_witness(problem, quote(a & b))
  expect_identical(witness$values, problem$inputs$a & problem$inputs$b)
  expect_false(identical(witness$values, problem$target))
  expect_error(interpret_rwbo_output(problem, rwbo_witness_output(witness)), "hard clause|equivalence")
})

test_that("witness output distinguishes satisfiability from a proved simple optimum", {
  file <- tempfile(fileext = ".wcnf")
  on.exit(unlink(file))
  problem <- bdd_to_wcnf("a ^ b", file, max_operators = 1L, prefer_negations = FALSE)
  witness <- rwbo_model_witness(problem, quote(xor(b, a)))
  result <- interpret_rwbo_output(problem, rwbo_witness_output(witness))
  expect_true(result$verified)
  expect_false(result$optimal)
  # This two-variable function cannot use zero operators; the one-slot XOR
  # reaches the mathematical operator lower bound in the uniform cost model.
  optimum <- interpret_rwbo_output(problem, rwbo_witness_output(witness, status = "OPTIMUM FOUND"))
  expect_true(optimum$optimal)
  expect_identical(optimum$cost, 1L)
  stdout <- rwbo_witness_output(witness, chunk_size = 4L)
  rows <- strsplit(stdout, "\n", fixed = TRUE)[[1L]]
  expect_identical(rows[[1L]], "s SATISFIABLE")
  expect_identical(rows[[2L]], "o 1")
  expect_length(rows[startsWith(rows, "v ")], ceiling(problem$nvars / 4L))
  expect_true(all(endsWith(rows[startsWith(rows, "v ")], " 0")))
})

test_that("unsupported ASTs and impossible slot requests fail before formatting", {
  file <- tempfile(fileext = ".wcnf")
  on.exit(unlink(file))
  problem <- bdd_to_wcnf("a ^ b", file, max_operators = 1L)
  expect_error(rwbo_model_witness(problem, quote(a & b & a)), "operator slots")
  expect_error(rwbo_model_witness(problem, quote(a)), "at least one operator")
  expect_error(rwbo_model_witness(problem, quote(a & c)), "outside the problem pool")
  expect_error(rwbo_model_witness(problem, quote(sum(a, b))), "Unsupported witness operator")
  expect_error(rwbo_model_witness(problem, quote(xor(a))), "number of arguments")
  expect_error(rwbo_model_witness(problem, NA), "Boolean AST")
  witness <- rwbo_model_witness(problem, quote(a ^ b))
  for (chunk_size in list(0L, -1L, NA_integer_, Inf, 1.5, TRUE, c(1L, 2L), "2")) {
    expect_error(rwbo_witness_output(witness, chunk_size = chunk_size), "chunk_size")
  }
})
