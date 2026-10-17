# Standalone end-to-end tests, using the real Rcudd and Rwbo packages.
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_dir <- if (length(script_arg)) dirname(normalizePath(sub("^--file=", "", script_arg[[1L]]))) else {
  "inst/scripts"
}
source(file.path(script_dir, "bdd_rwbo_minimize.R"))
.rwbo_require("Rcudd")
.rwbo_require("Rwbo")
test_dir <- tempfile("rwbo-formula-tests-")
dir.create(test_dir)

expect_error <- function(expr, pattern) {
  error <- tryCatch(force(expr), error = identity)
  stopifnot(inherits(error, "error"), grepl(pattern, conditionMessage(error), fixed = TRUE))
}
solve_case <- function(expr, expected, bound = max(3L, expected), expected_cost = if (prefer_negations) 2L * expected else expected,
                       prefer_negations = TRUE) {
  problem <- Rcudd::bdd_to_wcnf(expr, tempfile(tmpdir = test_dir, fileext = ".wcnf"), bound,
                                prefer_negations = prefer_negations)
  result <- solve_rwbo(problem, time_limit = 60L)
  stopifnot(result$verified, result$optimal, identical(problem$prefer_negations, prefer_negations))
  if (!is.null(expected)) stopifnot(identical(result$operator_count, as.integer(expected)))
  stopifnot(identical(result$cost, as.integer(expected_cost)))
  operators <- gregexpr("[&|^!]", result$expression)[[1L]]
  count <- if (operators[[1L]] == -1L) 0L else length(operators)
  stopifnot(count == result$operator_count)
  weighted <- 2L * count - lengths(regmatches(result$expression, gregexpr("!", result$expression, fixed = TRUE)))
  if (!prefer_negations) weighted <- count
  stopifnot(weighted == result$cost)
  cat("PASS: ", expr, " -> ", result$expression, " (operators=", count, ", cost=", result$cost, ")\n", sep = "")
  invisible(list(problem = problem, result = result))
}

# Enumerate all two-input functions independently by operator cost. Truth
# vectors, rather than expression rewriting, determine the reference minima.
inputs <- expand.grid(a = c(FALSE, TRUE), b = c(FALSE, TRUE))
signature <- function(bits) paste(as.integer(bits), collapse = "")
for (prefer_negations in c(TRUE, FALSE)) {
  levels <- list(list(rep(FALSE, 4L), rep(TRUE, 4L), inputs$a, inputs$b))
  minimum <- setNames(rep(0L, 4L), vapply(levels[[1L]], signature, character(1L)))
  binary_cost <- if (prefer_negations) 2L else 1L
  for (cost in 1:3) {
    candidates <- lapply(levels[[cost]], `!`)
    if (cost >= binary_cost) for (left_cost in 0:(cost - binary_cost)) {
      right_cost <- cost - binary_cost - left_cost
      for (a in levels[[left_cost + 1L]]) {
        for (b in levels[[right_cost + 1L]]) candidates <- c(candidates, list(a & b, a | b, xor(a, b)))
      }
    }
    new <- list()
    for (candidate in candidates) {
      key <- signature(candidate)
      if (!key %in% names(minimum)) {
        minimum[[key]] <- cost
        new[[length(new) + 1L]] <- candidate
      }
    }
    levels[[cost + 1L]] <- new
  }
  stopifnot(length(minimum) == 16L)
  for (mask in 0:15) {
    bits <- as.logical(as.integer(intToBits(mask))[1:4])
    terms <- vapply(which(bits), function(row) {
      paste0("(", if (inputs$a[[row]]) "a" else "!a", " & ",
             if (inputs$b[[row]]) "b" else "!b", ")")
    }, character(1L))
    expr <- if (length(terms)) paste(terms, collapse = " | ") else "false"
    solve_case(expr, NULL, bound = 3L, expected_cost = minimum[[signature(bits)]],
               prefer_negations = prefer_negations)
  }
}

example <- solve_case(rwbo_example_expression(), 5L, 8L)
stopifnot(length(example$problem$target) == 64L)
solve_case("(a & !b) | (!a & b)", 1L) # XOR replaces a longer DNF
# No XOR in either input. Recover two XOR operators from truth-table semantics.
parity_input <- paste("(a & !b & !c)", "(!a & b & !c)",
                      "(!a & !b & c)", "(a & b & c)", sep = " | ")
stopifnot(!grepl("^", parity_input, fixed = TRUE))
parity <- solve_case(parity_input, 2L)
stopifnot(length(gregexpr("^", parity$result$expression, fixed = TRUE)[[1L]]) == 2L)
pair_input <- paste("(a & !b & c & !d)", "(a & !b & !c & d)",
                    "(!a & b & c & !d)", "(!a & b & !c & d)", sep = " | ")
stopifnot(!grepl("^", pair_input, fixed = TRUE))
pairs <- solve_case(pair_input, 3L, 4L) # (a ^ b) & (c ^ d)
stopifnot(length(gregexpr("^", pairs$result$expression, fixed = TRUE)[[1L]]) == 2L)
solve_case("!(a & b)", 2L, expected_cost = 3L)
solve_case("(s & a) | (!s & b)", 3L) # b ^ (s & (a ^ b))
solve_case("(x & y) | (x & !y)", 0L, 0L)
solve_case("!x", 1L, 1L, expected_cost = 1L)
solve_case("(long_name.1 & !KEEP.OUT.ATTRS) | (!long_name.1 & KEEP.OUT.ATTRS)", 1L)

# Index zero is eliminated, and CUDD creation order is b,a rather than a,b.
holes <- solve_case("(z & !z) | (b ^ a)", 1L)
stopifnot(identical(holes$problem$variable_indices, c(2L, 1L)),
          identical(holes$problem$target, xor(holes$problem$inputs$a, holes$problem$inputs$b)),
          is.null(holes$problem$bdd_expression))
solve_case("!(z ^ a)", 2L, expected_cost = 3L)
solve_case("true", 0L, 0L)
solve_case("false", 0L, 0L)
solve_case("\u00e4", 0L, 0L)

# The standalone native harness supplies this file from a root with holes in
# its support, after Cudd::ShuffleHeap. Confirm byte-identical encoding, then
# solve and validate the native-produced file with the real Rwbo binary.
native_fixture <- Sys.getenv("RCUDD_NATIVE_FIXTURE", "")
if (nzchar(native_fixture)) {
  native_problem <- Rcudd::bdd_to_wcnf("!(a ^ b)", tempfile(tmpdir = test_dir), 3L)
  stopifnot(identical(readLines(native_problem$wcnf_file), readLines(native_fixture)))
  native_problem$wcnf_file <- normalizePath(native_fixture, winslash = "/")
  native_result <- solve_rwbo(native_problem)
  stopifnot(native_result$verified, native_result$optimal, native_result$operator_count == 2L, native_result$cost == 3L)
  cat("PASS: existing reordered native BDD -> native WCNF -> Rwbo -> equivalent minimum cost3 / operators2\n")
}

too_small <- Rcudd::bdd_to_wcnf("!(a & b)", tempfile(tmpdir = test_dir), 1L)
unsat <- solve_rwbo(too_small)
stopifnot(unsat$status == "UNSATISFIABLE", !unsat$optimal, !unsat$verified, is.na(unsat$expression))
zero_bound <- Rcudd::bdd_to_wcnf("!a", tempfile(tmpdir = test_dir), 0L)
stopifnot(solve_rwbo(zero_bound)$status == "UNSATISFIABLE")
support_bound <- Rcudd::bdd_to_wcnf(rwbo_example_expression(), tempfile(tmpdir = test_dir), 4L)
stopifnot(solve_rwbo(support_bound)$status == "UNSATISFIABLE")

unknown <- Rcudd::interpret_rwbo_output(example$problem, "s UNKNOWN")
stopifnot(!unknown$verified, !unknown$optimal, is.na(unknown$expression))
missing_model <- Rcudd::interpret_rwbo_output(example$problem, "s OPTIMUM FOUND\no 10")
stopifnot(!missing_model$optimal, !missing_model$verified)
candidate <- Rcudd::interpret_rwbo_output(example$problem, sub("s OPTIMUM FOUND", "s SATISFIABLE",
                                                example$result$solver_output, fixed = TRUE))
stopifnot(candidate$verified, !candidate$optimal)
expect_error(Rcudd::interpret_rwbo_output(example$problem, "s OPTIMUM FOUND\no 10\nv 1 0"), "incomplete or invalid")
expect_error(Rcudd::interpret_rwbo_output(example$problem, sub("o 10", "o 99", example$result$solver_output,
                                                 fixed = TRUE)), "cost does not match")
expect_error(Rcudd::bdd_to_wcnf("a & b", tempfile(tmpdir = test_dir), max_variables = 1L), "truth-table limit")
expect_error(Rcudd::bdd_to_wcnf("a & b", tempfile(tmpdir = test_dir), max_operators = -1L), "whole number")
expect_error(Rcudd::bdd_to_wcnf("a & b", tempfile(tmpdir = test_dir), max_clauses = 1L), "Clause limit")
expect_error(Rcudd::bdd_to_wcnf("a & b", tempfile(tmpdir = test_dir), max_operators = 1000000L), "Clause limit")

# The corrected Rwbo wrapper must preserve complete long physical model lines.
# A wide care table gives a long model without making this output-protocol
# regression depend on an expensive multi-operator optimality proof.
long_problem <- Rcudd::bdd_to_wcnf("!a", tempfile(tmpdir = test_dir), 1L,
                                  max_variables = 11L,
                                  care = paste(letters[2:11], collapse = " ^ "))
long <- list(problem = long_problem, result = solve_rwbo(long_problem))
stopifnot(long$result$verified, long$result$optimal,
          long$result$operator_count == 1L, long$result$cost == 1L)
stopifnot(long$problem$nvars > 6000L)
long_lines <- strsplit(long$result$solver_output, "\n", fixed = TRUE)[[1L]]
model_lines <- grep("^v ", long_lines, value = TRUE)
stopifnot(length(model_lines) == 1L, nchar(model_lines, type = "bytes") > 3L * 8095L)
cat("PASS: complete long Rwbo model decoded and fully validated\n")
cat("All standalone BDD/Rwbo integration checks passed.\n")
