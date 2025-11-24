#' Convert R expressions to C++ style logical strings
#'
#' The functions walk quoted R language objects (for example those produced by
#' `quote()`) or character strings and rewrite boolean operators to their C++
#' equivalents. Supported operators are `&&`, `||`, `xor()` and `!`. Sub-
#' expressions that are not one of these operators are replaced by stable
#' placeholders so that identical fragments share the same name. Logical
#' constants `TRUE` and `FALSE` are translated to lowercase `true` and `false`.
#'
#' Comparisons using `<`, `>`, `<=`, `>=`, `==` and `!=` are rewritten into
#' dedicated ordering placeholders of the form `L_n1_n2`, `E_n1_n2` and
#' `G_n1_n2` (for "less", "equal" and "greater"). Each variable that appears in
#' a comparison receives a numbered token `n1`, `n2`, … and these tokens are
#' used to build the comparison placeholders. Compound comparisons such as
#' `<=`, `>=` and `!=` are translated into disjunctions of the basic placeholders.
#' The mapping of variable tokens is returned alongside other placeholders so
#' that downstream code can enforce ordering constraints.
#'
#' @param expr A quoted expression containing a boolean statement or a character
#'   string that can be parsed to one. Inputs are not captured unevaluated, so
#'   use `quote()` or `expression()` when supplying inline expressions.
#' @param use_placeholders Logical flag indicating whether non-operator
#'   sub-expressions should be replaced by placeholders. When `FALSE`, the
#'   original `deparse()`d expression is kept in the output string while the
#'   placeholder mapping is still reported.
#'
#' @return A list with elements
#' * `expression`: the constructed C++ style logical expression string.
#' * `placeholders`: named character vector that maps placeholder names to the
#'   original expression strings.
#' * `order_variables`: named character vector that maps variable tokens (`n1`,
#'   `n2`, …) to the variable names used in comparisons.
#'
#' @examples
#' expression_to_cpp_logic(quote(((x && y) || f(z)) && w))
#'
#' expression_to_cpp_logic("xor(x, y) || f(z) && w")
#'
#' @export
expression_to_cpp_logic <- function(expr, use_placeholders = TRUE) {
  converted <- cpp_logic_from_expressions(list(expr), use_placeholders)

  return(list(
    expression = converted$expressions[[1L]],
    placeholders = converted$placeholders,
    order_variables = converted$order_variables
  ))
}

#' Convert multiple R expressions to C++ style logical strings with shared placeholders
#'
#' Processes several quoted expressions at once so that common subexpressions
#' are assigned the same placeholder across all inputs. This makes it easier to
#' apply consistent substitutions when the resulting expressions are evaluated
#' elsewhere (for example in C++ code).
#'
#' @param ... One or more quoted R expressions or character scalars that can be
#'   parsed with [str2lang()].
#' @inheritParams expression_to_cpp_logic
#'
#' @return A list with elements
#' * `expressions`: character vector with the C++ style logic strings for each
#'   input expression, in order.
#' * `placeholders`: named character vector that maps placeholder names to the
#'   original expression strings, shared across all inputs.
#' * `order_variables`: named character vector that maps variable tokens (`n1`,
#'   `n2`, …) to the variable names used in comparisons.
#'
#' @examples
#' expressions_to_cpp_logic(quote(x && y), quote(!(x && y)), quote(xor(y, z)))
#'
#' placeholders <- expressions_to_cpp_logic(quote(x && y), quote(!(x && y)))$placeholders
#' placeholders
#'
#' @export
expressions_to_cpp_logic <- function(..., use_placeholders = TRUE) {
  exprs <- list(...)

  stopifnot("At least one expression must be supplied." = length(exprs) >= 1L)

  return(cpp_logic_from_expressions(exprs, use_placeholders))
}

cpp_logic_from_expressions <- function(exprs, use_placeholders = TRUE) {
  state <- new.env(parent = emptyenv())
  state$placeholder_map <- list()
  state$order_placeholder_map <- list()
  state$counter <- 0L
  state$order_counter <- 0L
  state$variable_tokens <- list()
  state$variable_counter <- 0L

  # Move out of main function to avoid repeated definition (give state as argument if necessary)
  placeholder_for <- function(node) {
    key <- deparse(node)

    if (!key %in% names(state$placeholder_map)) {
      state$counter <- state$counter + 1L
      state$placeholder_map[[key]] <- paste0("p", state$counter)
    }

    if (use_placeholders) {
      return(state$placeholder_map[[key]])
    }

    return(key)
  }

  token_for_variable <- function(name) {
    if (!name %in% names(state$variable_tokens)) {
      state$variable_counter <- state$variable_counter + 1L
      state$variable_tokens[[name]] <- paste0("n", state$variable_counter)
    }

    return(state$variable_tokens[[name]])
  }

  register_order_placeholder <- function(type, left, right) {
    if (left == right) {
      stop("Order placeholders require distinct variable names.")
    }

    placeholder <- NULL
    key <- paste(type, left, right, sep = "|")

    if (key %in% names(state$order_placeholder_map)) {
      placeholder <- state$order_placeholder_map[[key]]$name
    } else {
      expr <- switch(
        EXPR = type,
        L = paste0(left, " < ", right),
        E = paste0(left, " == ", right),
        G = paste0(left, " > ", right)
      )

      left_token <- token_for_variable(left)
      right_token <- token_for_variable(right)

      placeholder <- paste0(type, "_", left_token, "_", right_token)
      state$order_placeholder_map[[key]] <- list(
        name = placeholder,
        expr = expr,
        first = left,
        second = right,
        type = type,
        left_token = left_token,
        right_token = right_token
      )
    }

    return(placeholder)
  }

  comparison_placeholder <- function(lhs_node, rhs_node, op) {
    lhs_name <- deparse(lhs_node)
    rhs_name <- deparse(rhs_node)

    if (lhs_name == rhs_name) {
      return(switch(
        EXPR = op,
        "<" = "false",
        ">" = "false",
        "<=" = "true",
        ">=" = "true",
        "==" = "true",
        "!=" = "false"
      ))
    }

    # Keep placeholder naming deterministic so equivalent comparisons reuse the
    # same token order regardless of how the operands were written.
    ordered_names <- sort(c(lhs_name, rhs_name))
    left <- ordered_names[[1L]]
    right <- ordered_names[[2L]]

    is_canonical_direction <- identical(lhs_name, left)

    l_name <- register_order_placeholder("L", left, right)
    e_name <- register_order_placeholder("E", left, right)
    g_name <- register_order_placeholder("G", left, right)

    if (op == "<") {
      return(if (is_canonical_direction) l_name else g_name)
    }
    if (op == ">") {
      return(if (is_canonical_direction) g_name else l_name)
    }
    if (op == "==") {
      return(e_name)
    }
    if (op == "<=") {
      return(if (is_canonical_direction) paste0("(", l_name, " | ", e_name, ")")
             else paste0("(", g_name, " | ", e_name, ")"))
    }
    if (op == ">=") {
      return(if (is_canonical_direction) paste0("(", g_name, " | ", e_name, ")")
             else paste0("(", l_name, " | ", e_name, ")"))
    }
    if (op == "!=") {
      return(paste0("(", l_name, " | ", g_name, ")"))
    }

    stop("Unsupported comparison operator: ", op)
  }

  to_cpp <- function(node) {
    is_logical_scalar <- is.logical(node) && length(node) == 1L && !is.na(node)
    is_name_logical <- is.name(node) && as.character(node) %in% c("TRUE", "FALSE")
    if (is_logical_scalar || is_name_logical) {
      return(if (isTRUE(node) || identical(as.character(node), "TRUE")) "true" else "false")
    }

    if (is.call(node)) {
      parts <- as.list(node)
      op <- as.character(parts[[1L]])
      comps <- parts[-1L]

      if (op %in% c("<", ">", "<=", ">=", "==", "!=")) {
        if (length(comps) != 2L) {
          stop("Binary comparison operators require exactly two operands.")
        }

        return(comparison_placeholder(comps[[1L]], comps[[2L]], op))
      }

      if (op == "(") {
        return(to_cpp(comps[[1L]]))
      }

      if (op %in% c("&&", "||", "xor", "!")) {
        mapped <- switch(
          EXPR = op,
          "&&" = "&",
          "||" = "|",
          xor = "^",
          "!" = "!"
        )

        if (op == "!") {
          return(paste0("!", to_cpp(comps[[1L]])))
        }

        left <- to_cpp(comps[[1L]])
        right <- to_cpp(comps[[2L]])
        return(paste0("(", left, " ", mapped, " ", right, ")"))
      }
    }

    return(placeholder_for(node))
  }

  coerce_expr <- function(expr) {
    if (is.expression(expr) && length(expr) == 1L) {
      expr <- expr[[1L]]
    }

    if (is.logical(expr) && length(expr) == 1L && !is.na(expr)) {
      return(expr)
    }

    if (is.character(expr)) {
      expr <- str2lang(expr)
    }

    if (!is.language(expr)) {
      stop("Each `expr` must be a language object or a character string representing one.")
    }

    return(expr)
  }

  translated <- vapply(exprs, function(expr) {
    return(to_cpp(coerce_expr(expr)))
  }, character(1L), USE.NAMES = FALSE)

  placeholders <- unlist(state$placeholder_map, use.names = FALSE)

  if (length(placeholders) == 0L) {
    placeholders <- stats::setNames(character(0L), character(0L))
  } else {
    names(placeholders) <- names(state$placeholder_map)
    placeholders <- stats::setNames(names(placeholders), placeholders)
  }

  variable_names <- names(state$variable_tokens)
  if (is.null(variable_names)) {
    variable_names <- character(0L)
  }

  order_variables <- vapply(variable_names, function(var) state$variable_tokens[[var]], character(1L))
  order_variables <- stats::setNames(variable_names, order_variables)

  if (length(order_variables) == 0L) {
    order_variables <- stats::setNames(character(0L), character(0L))
  }

  return(list(
    expressions = translated,
    placeholders = placeholders,
    order_variables = order_variables
  ))
}
