#' Restore readable R expressions from C++ logical strings
#'
#' Reverses the placeholder substitution performed by
#' [expressions_to_cpp_logic()]. Ordinary placeholders are replaced by their
#' original R expressions and comparison tokens by comparisons of the original
#' operands. The result uses `&&`, `||`, `!`, `xor()`, `TRUE` and `FALSE`.
#'
#' @param expr A single C++ style logical string.
#' @param placeholders Named character vector mapping ordinary placeholders to
#'   original R expressions, as returned by [expressions_to_cpp_logic()].
#' @param order_variables Named character vector mapping comparison operand
#'   tokens to original R expressions, as returned by
#'   [expressions_to_cpp_logic()].
#'
#' @details
#' Comparison combinations for the same pair of operands are combined using
#' the ordering model of [order_constraints()]: exactly one of less, equal and
#' greater holds. For example, `G_n1_n2 | E_n1_n2` becomes `a >= b`, and
#' `!G_n1_n2` becomes `a <= b`. This uses classical Boolean logic and a total
#' order; it does not reproduce R's missing-value logic.
#'
#' Substitutions operate on complete tokens and parsed language objects, which
#' preserves grouping even when a placeholder represents a compound expression.
#' No input expression is evaluated. Unmapped ordinary identifiers remain R
#' names; comparison placeholders require both operand mappings. The result
#' is not guaranteed to be the shortest equivalent expression.
#'
#' @return A character scalar containing a parseable R expression.
#'
#' @examples
#' converted <- expression_to_cpp_logic(quote(a >= b && ready))
#' cpp_logic_to_expression(
#'   converted$expression, converted$placeholders, converted$order_variables
#' )
#'
#' @export
cpp_logic_to_expression <- function(expr, placeholders = character(), order_variables = character()) {
  if (!is.character(expr) || length(expr) != 1L || is.na(expr)) {
    stop("`expr` must be a non-missing character scalar.")
  }
  return(cpp_logic_to_expressions(expr, placeholders, order_variables)[[1L]])
}

#' Restore multiple readable R expressions from C++ logical strings
#'
#' Uses a shared placeholder mapping to restore several expressions, including
#' reduced strings returned by [bdd_literals()] or [bdd_restrict_chain()].
#'
#' @param exprs Character vector of C++ style logical strings.
#' @inheritParams cpp_logic_to_expression
#'
#' @return A character vector of parseable R expressions, in input order.
#'
#' @examples
#' converted <- expressions_to_cpp_logic(quote(a > b), quote(a <= b))
#' cpp_logic_to_expressions(
#'   converted$expressions, converted$placeholders, converted$order_variables
#' )
#'
#' @export
cpp_logic_to_expressions <- function(exprs, placeholders = character(), order_variables = character()) {
  if (!is.character(exprs) || anyNA(exprs)) {
    stop("`exprs` must be a character vector without missing values.")
  }
  parse_mapping <- function(mapping, label) {
    if (!is.character(mapping) || anyNA(mapping) ||
          (length(mapping) > 0L &&
             (is.null(names(mapping)) || anyNA(names(mapping)) ||
                !all(nzchar(names(mapping))) || anyDuplicated(names(mapping)) > 0L))) {
      stop("`", label, "` must be a character vector with unique, non-empty names.")
    }
    return(lapply(mapping, str2lang))
  }
  placeholders <- parse_mapping(placeholders, "placeholders")
  order_variables <- parse_mapping(order_variables, "order_variables")

  return(vapply(exprs, function(expr) {
    node <- parse_cpp_logic(expr, placeholders, order_variables)
    return(deparse1(cpp_logic_as_language(node, order_variables), width.cutoff = 500L, backtick = TRUE))
  }, character(1L), USE.NAMES = FALSE))
}

# Comparisons carry a subset of the three possible ordering outcomes. Keeping
# this separate from the original R fragments avoids rewriting inside a
# substituted function call or accidentally interpreting an R name as a token.
cpp_logic_relation <- function(tokens, mask) {
  if (mask == 0L || mask == 7L) {
    return(list(type = "constant", value = mask == 7L))
  }
  return(list(type = "relation", tokens = tokens, mask = mask))
}

cpp_logic_negate <- function(node) {
  if (node$type == "constant") {
    return(list(type = "constant", value = !node$value))
  }
  if (node$type == "relation") {
    return(cpp_logic_relation(node$tokens, bitwXor(node$mask, 7L)))
  }
  if (node$type == "operator" && node$op == "!") {
    return(node$args[[1L]])
  }
  return(list(type = "operator", op = "!", args = list(node)))
}

cpp_logic_combine <- function(op, left, right) {
  mask_op <- switch(op, "&" = bitwAnd, "|" = bitwOr, "^" = bitwXor)
  if (left$type == "relation" && right$type == "relation" &&
        identical(left$tokens, right$tokens)) {
    return(cpp_logic_relation(left$tokens, mask_op(left$mask, right$mask)))
  }
  if (op == "^") {
    if (left$type == "constant") {
      return(if (left$value) cpp_logic_negate(right) else right)
    }
    if (right$type == "constant") {
      return(if (right$value) cpp_logic_negate(left) else left)
    }
    return(list(type = "operator", op = op, args = list(left, right)))
  }

  flatten <- function(node) {
    if (node$type == "operator" && node$op == op) return(node$args)
    return(list(node))
  }
  operands <- c(flatten(left), flatten(right))
  combined <- list()
  relation_positions <- list()
  for (node in operands) {
    if (node$type == "relation") {
      key <- paste(node$tokens, collapse = "_")
      position <- relation_positions[[key]]
      if (is.null(position)) {
        relation_positions[[key]] <- length(combined) + 1L
      } else {
        previous <- combined[[position]]
        # An earlier combination can already have become a constant.
        mask <- if (previous$type == "constant") {
          if (previous$value) 7L else 0L
        } else {
          previous$mask
        }
        combined[[position]] <- cpp_logic_relation(node$tokens, mask_op(mask, node$mask))
        next
      }
    }
    combined[[length(combined) + 1L]] <- node
  }

  kept <- list()
  for (node in combined) {
    if (node$type == "constant") {
      if ((op == "&" && !node$value) || (op == "|" && node$value)) {
        return(node)
      }
    } else {
      kept[[length(kept) + 1L]] <- node
    }
  }
  if (length(kept) == 0L) return(list(type = "constant", value = op == "&"))
  if (length(kept) == 1L) return(kept[[1L]])
  return(list(type = "operator", op = op, args = kept))
}

parse_cpp_logic <- function(expr, placeholders, order_variables) {
  tokens <- regmatches(expr, gregexpr("[!&|^()]|[^[:space:]!&|^()]+", expr))[[1L]]
  state <- new.env(parent = emptyenv())
  state$values <- list()
  state$operators <- character()
  precedence <- c("|" = 1L, "^" = 2L, "&" = 3L, "!" = 4L)
  expecting_operand <- TRUE
  invalid <- function() stop("Invalid C++ logical expression: ", expr)
  apply_operator <- function() {
    op <- state$operators[[length(state$operators)]]
    state$operators <- state$operators[-length(state$operators)]
    count <- length(state$values)
    if (op == "!") {
      state$values[[count]] <- cpp_logic_negate(state$values[[count]])
    } else {
      right <- state$values[[count]]
      state$values[[count]] <- NULL
      state$values[[count - 1L]] <- cpp_logic_combine(op, state$values[[count - 1L]], right)
    }
    return(invisible(NULL))
  }
  atom <- function(token) {
    if (tolower(token) %in% c("true", "false")) {
      return(list(type = "constant", value = tolower(token) == "true"))
    }
    if (token %in% names(placeholders)) {
      return(list(type = "atom", value = placeholders[[token]]))
    }
    parts <- regmatches(token, regexec("^([LEG])_(n[0-9]+)_(n[0-9]+)$", token))[[1L]]
    if (length(parts) > 0L) {
      pair <- parts[3:4]
      if (!all(pair %in% names(order_variables))) {
        stop("Missing `order_variables` mapping for comparison placeholder: ", token)
      }
      if (pair[[1L]] == pair[[2L]]) {
        return(list(type = "constant", value = parts[[2L]] == "E"))
      }
      mask <- switch(EXPR = parts[[2L]], L = 1L, E = 2L, G = 4L)
      if (pair[[1L]] > pair[[2L]]) {
        pair <- rev(pair)
        mask <- switch(EXPR = parts[[2L]], L = 4L, E = 2L, G = 1L)
      }
      return(cpp_logic_relation(pair, mask))
    }
    return(list(type = "atom", value = as.name(token)))
  }

  # Use the same precedence as the native parser. R's ^ is exponentiation and
  # binds more tightly than &, so str2lang() alone cannot parse this syntax.
  for (token in tokens) {
    if (token == "(" || token == "!") {
      if (!expecting_operand) invalid()
      state$operators <- c(state$operators, token)
    } else if (token == ")") {
      if (expecting_operand) invalid()
      while (length(state$operators) > 0L && state$operators[[length(state$operators)]] != "(") apply_operator()
      if (length(state$operators) == 0L) invalid()
      state$operators <- state$operators[-length(state$operators)]
    } else if (token %in% c("&", "|", "^")) {
      if (expecting_operand) invalid()
      while (length(state$operators) > 0L && state$operators[[length(state$operators)]] != "(" &&
               precedence[[state$operators[[length(state$operators)]]]] >= precedence[[token]]) apply_operator()
      state$operators <- c(state$operators, token)
      expecting_operand <- TRUE
    } else {
      if (!expecting_operand) invalid()
      state$values[[length(state$values) + 1L]] <- atom(token)
      expecting_operand <- FALSE
    }
  }
  if (expecting_operand) invalid()
  while (length(state$operators) > 0L) {
    if (state$operators[[length(state$operators)]] == "(") invalid()
    apply_operator()
  }
  return(state$values[[1L]])
}

cpp_logic_as_language <- function(node, order_variables) {
  if (node$type %in% c("constant", "atom")) return(node$value)
  if (node$type == "relation") {
    op <- c("<", "==", "<=", ">", "!=", ">=")[[node$mask]]
    return(call(op, order_variables[[node$tokens[[1L]]]], order_variables[[node$tokens[[2L]]]]))
  }
  operands <- lapply(node$args, cpp_logic_as_language, order_variables = order_variables)
  op <- switch(node$op, "&" = "&&", "|" = "||", "^" = "xor", "!" = "!")
  if (op == "!") return(call(op, operands[[1L]]))
  return(Reduce(function(left, right) return(call(op, left, right)), operands))
}
