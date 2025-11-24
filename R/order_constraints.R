#' Generate ordering constraints for comparison placeholders
#'
#' Constructs logical constraints for placeholders of the form `L_n1_n2`,
#' `E_n1_n2` and `G_n1_n2` that encode the outcome of pairwise comparisons.
#' The constraints ensure that exactly one relation holds for each unordered
#' pair of tokens, add transitivity rules and enforce reflexive equality
#' (`E_x_x`).
#'
#' @param tokens Character vector of placeholder tokens such as `"n1"`,
#'   `"n2"`, ... representing the variables that are compared.
#'
#' @return Character vector with boolean expressions in C++ syntax (operators
#'   `!`, `&`, `|`). The expressions can be passed to downstream routines as
#'   additional constraints.
#'
#' @examples
#' order_constraints(c("n1", "n2", "n3"))
#'
#' @export
order_constraints <- function(tokens) {

  tokens <- unique(tokens)
  tokens <- tokens[!is.na(tokens) & nzchar(tokens)]

  if (length(tokens) == 0L) {
    return(character())
  }

  tokens <- sort(tokens)

  constraints <- paste0("E_", tokens, "_", tokens)

  if (length(tokens) >= 2L) {
    token_pairs <- utils::combn(tokens, 2L, simplify = FALSE)
    for (pair in token_pairs) {
      constraints <- c(constraints, exclusivity_for_pair(pair[[1L]], pair[[2L]]))
    }
  }

  if (length(tokens) >= 3L) {
    token_triplets <- utils::combn(tokens, 3L, simplify = FALSE)
    for (triplet in token_triplets) {
      constraints <- c(constraints, transition_rules(triplet[[1L]], triplet[[2L]], triplet[[3L]]))
    }
  }

  return(unique(constraints))
}


implication <- function(antecedent, consequent) {
  return(paste0("!(", antecedent, ") | ", consequent))
}


transition_rules <- function(first, second, third) {
  constraints <- c(
    implication(paste0("L_", first, "_", second, " & L_", second, "_", third), paste0("L_", first, "_", third)),
    implication(paste0("G_", first, "_", second, " & G_", second, "_", third), paste0("G_", first, "_", third)),
    implication(paste0("E_", first, "_", second, " & E_", second, "_", third), paste0("E_", first, "_", third)),
    implication(paste0("L_", first, "_", second, " & E_", second, "_", third), paste0("L_", first, "_", third)),
    implication(paste0("E_", first, "_", second, " & L_", second, "_", third), paste0("L_", first, "_", third)),
    implication(paste0("G_", first, "_", second, " & E_", second, "_", third), paste0("G_", first, "_", third)),
    implication(paste0("E_", first, "_", second, " & G_", second, "_", third), paste0("G_", first, "_", third))
  )
  return(constraints)
}


exclusivity_for_pair <- function(first, second) {
  exclusion <- function(lhs, rhs) paste0("!(", lhs, "_", first, "_", second, " & ", rhs, "_", first, "_", second, ")")

  constraints <- c(
    paste0("(G_", first, "_", second, " | L_", first, "_", second, " | E_", first, "_", second, ")"),
    exclusion("G", "L"),
    exclusion("G", "E"),
    exclusion("L", "E")
  )

  return(constraints)
}
