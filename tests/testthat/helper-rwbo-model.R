# Build a known formula's witness by forward evaluation, without a SAT solver
# or target comparison. Repeated AST subtrees deliberately become separate gates.
rwbo_model_witness <- function(problem, formula) {
  gate_count <- length(problem$gates)
  input_count <- length(problem$variables) + 2L
  row_count <- nrow(problem$inputs)
  state <- new.env(parent = emptyenv())
  state$nodes <- list()

  walk <- function(node) {
    if (is.logical(node) && length(node) == 1L && !is.na(node)) {
      return(list(source = as.integer(node), values = rep(node, row_count)))
    }
    if (is.symbol(node)) {
      token <- as.character(node)
      if (token %in% c("TRUE", "FALSE")) {
        bit <- identical(token, "TRUE")
        return(list(source = as.integer(bit), values = rep(bit, row_count)))
      }
      column <- which(problem$variables == token)
      if (length(column) != 1L) {
        stop("Witness variable is outside the problem pool: ", token, call. = FALSE)
      }
      return(list(source = column + 1L, values = problem$inputs[[column]]))
    }
    if (!is.call(node) || !is.symbol(node[[1L]])) {
      stop("Witness formula must be a Boolean AST.", call. = FALSE)
    }
    token <- as.character(node[[1L]])
    if (identical(token, "(") && length(node) == 2L) {
      return(walk(node[[2L]]))
    }
    kinds <- c("!" = "not", "&" = "and", "&&" = "and", "|" = "or", "||" = "or",
               "^" = "xor", xor = "xor")
    if (!token %in% names(kinds)) {
      stop("Unsupported witness operator: ", token, call. = FALSE)
    }
    kind <- unname(kinds[[token]])
    if (length(node) != if (kind == "not") 2L else 3L) {
      stop("Wrong number of arguments for witness operator: ", token, call. = FALSE)
    }
    left <- walk(node[[2L]])
    right <- list(source = NA_integer_, values = rep(FALSE, row_count))
    if (kind != "not") {
      right <- walk(node[[3L]])
      if (left$source > right$source) {
        swap <- left
        left <- right
        right <- swap
      }
    }
    values <- switch(kind,
      not = !left$values,
      and = left$values & right$values,
      or = left$values | right$values,
      xor = xor(left$values, right$values)
    )
    source_index <- input_count + length(state$nodes)
    state$nodes <- c(state$nodes, list(list(kind = kind, left = left, right = right, values = values)))
    return(list(source = source_index, values = values))
  }

  root <- walk(formula)
  operator_count <- length(state$nodes)
  if (operator_count > gate_count) {
    stop("Witness formula exceeds the available operator slots.", call. = FALSE)
  }
  if (gate_count > 0L && operator_count == 0L) {
    stop("A nontrivial witness needs at least one operator.", call. = FALSE)
  }
  model <- -seq_len(problem$nvars)
  if (gate_count == 0L) {
    # The tiny shortcut file has only the compatibility SAT variable. Its
    # actual expression is metadata, so candidate values remain independent.
    model[[1L]] <- 1L
    return(list(model = model, cost = 0L, operator_count = 0L, values = root$values))
  }

  unused <- gate_count - operator_count
  positive <- integer()
  for (index in seq_len(unused)) {
    positive <- c(positive, problem$gates[[index]]$kind[["unused"]])
  }
  source_with_padding <- function(source_index) {
    if (source_index >= input_count) {
      return(source_index + unused)
    }
    return(source_index)
  }
  for (index in seq_len(operator_count)) {
    node <- state$nodes[[index]]
    gate <- problem$gates[[unused + index]]
    left_source <- source_with_padding(node$left$source)
    positive <- c(positive, gate$kind[[node$kind]], gate$left[[left_source + 1L]],
                  gate$left_value[node$left$values], gate$out[node$values])
    if (node$kind != "not") {
      right_source <- source_with_padding(node$right$source)
      positive <- c(positive, gate$right[[right_source + 1L]], gate$right_value[node$right$values])
    }
  }
  binary <- vapply(state$nodes, function(node) {
    return(node$kind != "not")
  }, logical(1L))
  if (length(problem$binary_auxiliary) > 0L) {
    prefix <- cumsum(c(rep(0L, unused), as.integer(binary)))
    bound <- problem$binary_lower_bound
    counter_bits <- logical()
    for (level in seq_len(bound)) {
      indices <- seq.int(level, gate_count - bound + level)
      counter_bits <- c(counter_bits, prefix[indices] >= level)
    }
    positive <- c(positive, problem$binary_auxiliary[counter_bits])
  }
  model[positive] <- positive
  cost <- operator_count + if (isTRUE(problem$prefer_negations)) sum(binary) else 0L
  return(list(model = model, cost = as.integer(cost), operator_count = operator_count, values = root$values))
}

rwbo_witness_output <- function(witness, status = "SATISFIABLE", chunk_size = NULL) {
  model <- witness$model
  if (is.null(chunk_size)) {
    chunk_size <- max(1L, length(model))
  }
  if (!is.numeric(chunk_size) || is.object(chunk_size) || !is.null(dim(chunk_size)) ||
        length(chunk_size) != 1L || is.na(chunk_size) || !is.finite(chunk_size) ||
        chunk_size < 1L || chunk_size != floor(chunk_size)) {
    stop("'chunk_size' must be a positive whole number.", call. = FALSE)
  }
  rows <- "v 0"
  if (length(model) > 0L) {
    groups <- split(model, ceiling(seq_along(model) / chunk_size))
    rows <- vapply(groups, function(group) {
      return(paste("v", paste(group, collapse = " "), "0"))
    }, character(1L), USE.NAMES = FALSE)
  }
  return(paste0(paste(c(paste("s", status), paste("o", witness$cost), rows), collapse = "\n"), "\n"))
}
