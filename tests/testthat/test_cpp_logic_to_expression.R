test_that("all comparison operators round-trip to readable R syntax", {
  for (op in c("<", ">", "<=", ">=", "==", "!=")) {
    expr <- paste("a", op, "b")
    converted <- expression_to_cpp_logic(expr)
    restored <- cpp_logic_to_expression(
      converted$expression, converted$placeholders, converted$order_variables
    )
    expect_identical(restored, expr)
  }
})

test_that("negated and combined ordering outcomes become single comparisons", {
  expressions <- c(
    "!L_n1_n2", "!G_n1_n2", "!E_n1_n2",
    "!(L_n1_n2 | E_n1_n2)", "!L_n1_n2 & !E_n1_n2",
    "G_n1_n2 | (!G_n1_n2 & E_n1_n2)",
    "L_n1_n2 & G_n1_n2", "L_n1_n2 | E_n1_n2 | G_n1_n2",
    "G_n2_n1 | E_n1_n2", "E_n1_n1", "L_n1_n1"
  )
  restored <- cpp_logic_to_expressions(expressions, order_variables = c(n1 = "a", n2 = "b"))
  expect_identical(restored, c(
    "a >= b", "a <= b", "a != b", "a > b", "a > b", "a >= b",
    "FALSE", "TRUE", "a <= b", "TRUE", "FALSE"
  ))
})

test_that("ordering simplification respects boolean grouping and unrelated terms", {
  variables <- c(n1 = "a", n2 = "b", n3 = "c")
  expect_identical(
    cpp_logic_to_expression("!L_n1_n2 & p1 & !E_n1_n2", c(p1 = "ready"), variables),
    "a > b && ready"
  )
  expect_identical(
    cpp_logic_to_expression("L_n1_n2 | E_n1_n3", order_variables = variables),
    "a < b || a == c"
  )
  expect_identical(
    cpp_logic_to_expression("L_n1_n2 & (E_n1_n2 | p1)", c(p1 = "ready"), variables),
    "a < b && (a == b || ready)"
  )
})

test_that("original fragments are restored structurally without evaluating them", {
  placeholders <- c(p1 = "a || b", p10 = "p1", p2 = "stop('never evaluate')")
  restored <- cpp_logic_to_expression("p1 & (!p10 | p2)", placeholders)
  inputs <- expand.grid(a = c(FALSE, TRUE), b = c(FALSE, TRUE), p1 = c(FALSE, TRUE))
  # Replace only the deliberately unevaluated call for the evaluation below.
  parsed <- str2lang(restored)
  expect_identical(parsed[[2L]][[2L]], quote(a || b))
  expect_identical(parsed[[3L]][[2L]][[3L]], quote(stop("never evaluate")))
  safe <- cpp_logic_to_expression("p1 & !p10", placeholders)
  for (i in seq_len(nrow(inputs))) {
    input <- as.list(inputs[i, ])
    expect_identical(eval(str2lang(safe), input), (input$a || input$b) && !input$p1)
  }
  expect_identical(cpp_logic_to_expression("p1", c(p1 = "`a b`")), "`a b`")
  expect_identical(cpp_logic_to_expression("p1", c(p1 = "NA")), "NA")
})

test_that("comparison operands preserve calls and grouping", {
  converted <- expression_to_cpp_logic(quote((a + b) >= f(c)))
  restored <- cpp_logic_to_expression(
    converted$expression, converted$placeholders, converted$order_variables
  )
  inputs <- list(a = 2L, b = 3L, c = 4L, f = identity)
  expect_true(eval(str2lang(restored), inputs))
  expect_setequal(all.vars(str2lang(restored)), c("a", "b", "c"))
})

test_that("reverse translation follows native XOR and negation precedence", {
  expressions <- c(
    "a ^ b & c", "a & b ^ c", "a | b ^ c & !a", "!(a | b) ^ c",
    "!!a", "true ^ a", "a ^ false", "!false & a", "(a ^ b) ^ c"
  )
  inputs <- expand.grid(a = c(FALSE, TRUE), b = c(FALSE, TRUE), c = c(FALSE, TRUE))
  for (expr in expressions) {
    # CUDD's renderer removes XOR, providing an independent precedence oracle.
    rendered_cpp <- bdd_literals(expr)$reduced_expression
    restored <- cpp_logic_to_expression(expr)
    for (i in seq_len(nrow(inputs))) {
      input <- c(as.list(inputs[i, ]), list(true = TRUE, false = FALSE))
      expect_identical(eval(str2lang(restored), input), eval(str2lang(rendered_cpp), input), info = expr)
    }
  }
  expect_identical(cpp_logic_to_expression("a ^ b & c"), "xor(a, b && c)")
})

test_that("reverse translation handles late operands and token numbers above nine", {
  converted <- expressions_to_cpp_logic(
    quote(b < c), quote(a < b), quote(d < e), quote(f < g), quote(h < i), quote(b >= i)
  )
  restored <- cpp_logic_to_expressions(
    converted$expressions, converted$placeholders, converted$order_variables
  )
  inputs <- list(a = 1L, b = 2L, c = 3L, d = 4L, e = 5L, f = 6L, g = 7L, h = 8L, i = 9L)
  expected <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
  evaluated <- vapply(restored, function(expr) eval(str2lang(expr), inputs), logical(1L), USE.NAMES = FALSE)
  expect_identical(evaluated, expected)
  expect_identical(
    cpp_logic_to_expression("G_n10_n2", order_variables = c(n10 = "j", n2 = "b")), "j > b"
  )
})

test_that("readable reduced chains agree with original conditions where reachable", {
  expressions <- c("a > b && ready", "a >= b || ready", "a <= b", "a <= b || a > b")
  result <- bdd_restrict_from_expressions(as.list(expressions))
  inputs <- expand.grid(a = -1:1, b = -1:1, ready = c(FALSE, TRUE))
  expect_false(any(grepl("[LEG]_n[0-9]+|p[0-9]+", vapply(
    result$chain, function(entry) entry$reduced_expression, character(1L)
  ))))
  for (i in seq_len(nrow(inputs))) {
    input <- as.list(inputs[i, ])
    reachable <- TRUE
    for (j in seq_along(expressions)) {
      original <- eval(str2lang(expressions[[j]]), input)
      readable <- eval(str2lang(result$chain[[j]]$reduced_expression), input)
      if (reachable) expect_identical(readable, original, info = paste("branch", j, "input", i))
      reachable <- reachable && !original
    }
  }
  for (expr in c("a > b", "a >= b", "a <= b")) {
    reduced <- bdd_restrict_from_expressions(list(expr))$chain[[1L]]
    expect_identical(reduced$reduced_expression, expr)
  }
})

test_that("independent relation combinations agree with concrete ordered values", {
  expressions <- c(
    "!(a < b) && (b <= c || a == c)",
    "a != b || (b > c && a >= c)",
    "xor(a <= b, b >= c)",
    "(a <= b && a >= b) || a > c",
    "!(a >= b && b >= c) || a >= c"
  )
  inputs <- expand.grid(a = 1:3, b = 1:3, c = 1:3)
  for (expr in expressions) {
    reduced <- bdd_restrict_from_expressions(list(expr))$chain[[1L]]$reduced_expression
    for (i in seq_len(nrow(inputs))) {
      input <- as.list(inputs[i, ])
      expect_identical(eval(str2lang(reduced), input), eval(str2lang(expr), input), info = expr)
    }
  }
})

test_that("reverse translation rejects malformed syntax and ambiguous mappings", {
  for (expr in c("", " ", "a b", "a &", "& a", "a || b", "()", "(a", "a)", "a(b)", "!")) {
    expect_error(cpp_logic_to_expression(expr), "Invalid C\\+\\+ logical expression", info = expr)
  }
  expect_error(cpp_logic_to_expression(NA_character_), "character scalar")
  expect_error(cpp_logic_to_expression(c("a", "b")), "character scalar")
  expect_error(cpp_logic_to_expressions(c("a", NA_character_)), "without missing values")
  expect_error(cpp_logic_to_expression("p1", "x"), "unique, non-empty names")
  duplicated_names <- setNames(c("x", "y"), c("p1", "p1"))
  expect_error(cpp_logic_to_expression("p1", duplicated_names), "unique, non-empty names")
  expect_error(cpp_logic_to_expression("G_n1_n2", order_variables = c(n1 = "x")), "Missing.*mapping")
  expect_identical(cpp_logic_to_expressions(character()), character())
})
