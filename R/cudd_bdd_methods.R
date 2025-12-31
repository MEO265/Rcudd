.cudd_bdd_wrap <- function(ptr, bdd) {
  return(methods::new("CuddBDD", ptr = ptr, manager_ptr = .cudd_bdd_manager_ptr(bdd)))
}

.cudd_bdd_wrap_list <- function(ptrs, bdd) {
  return(lapply(ptrs, .cudd_bdd_wrap, bdd = bdd))
}

.cudd_check_bdd_list_manager <- function(bdd, vars, op) {
  for (var in vars) {
    if (!.cudd_check_same_manager(bdd, var, op)) {
      return(FALSE)
    }
  }
  return(TRUE)
}

.cudd_zdd_wrap <- function(ptr, bdd) {
  return(methods::new("CuddZDD", ptr = ptr, manager_ptr = .cudd_bdd_manager_ptr(bdd)))
}

#' Additional BDD methods
#'
#' Convenience wrappers for additional methods on CUDD BDD objects.
#'
#' @param bdd A [`CuddBDD`] instance.
#' @param other,cube,var,g,h,upper,bias,phases,ub,f,d,y_bdd Additional [`CuddBDD`]
#'   instances used by the operation.
#' @param manager A [`CuddManager`] instance.
#' @param limit Optional non-negative integer limit passed to CUDD.
#' @param index,index1,index2,phase Integer index values used by the operation.
#' @param permut Integer vector describing a variable permutation.
#' @param x,y,vector,vars,x_vars,g_list Lists of [`CuddBDD`] instances.
#' @param nvars,num_vars,threshold Non-negative integer values used by the operation.
#' @param weight Optional integer vector used in shortest path calculations.
#' @param mode,verbosity,precision Integer values used for reporting.
#' @param max_depth,direction Integer values used by clipping operations.
#' @param safe Logical scalar controlling approximate operations.
#' @param quality,quality1,quality0 Numeric scalars controlling approximate operations.
#' @param minterm,inputs,y_index Integer vectors used by the operation.
#' @param prob Numeric vector of probabilities for correlation weights.
#' @param n Integer size for equation solving.
#' @param upper_bound Integer upper bound for min hamming distance.
#' @param hardlimit Logical scalar for short path routines.
#' @param m Numeric scalar used by split operations.
#' @return A [`CuddBDD`] instance, a logical scalar, numeric scalar, character
#'   scalar, or list depending on the operation.
#' @name cudd_bdd_methods
NULL

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_is_zero <- function(bdd) {
  return(.Call(c_cudd_bdd_is_zero, .cudd_bdd_ptr(bdd)))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_is_one <- function(bdd) {
  return(.Call(c_cudd_bdd_is_one, .cudd_bdd_ptr(bdd)))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_is_cube <- function(bdd) {
  return(.Call(c_cudd_bdd_is_cube, .cudd_bdd_ptr(bdd)))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_is_var <- function(bdd) {
  return(.Call(c_cudd_bdd_is_var, .cudd_bdd_ptr(bdd)))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_print <- function(bdd, nvars, verbosity = 1L) {
  .Call(c_cudd_bdd_print, .cudd_bdd_ptr(bdd), nvars, verbosity)
  return(invisible(NULL))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_summary <- function(bdd, nvars, mode = 0L) {
  .Call(c_cudd_bdd_summary, .cudd_bdd_ptr(bdd), nvars, mode)
  return(invisible(NULL))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_apa_print_minterm <- function(bdd, nvars) {
  .Call(c_cudd_bdd_apa_print_minterm, .cudd_bdd_ptr(bdd), nvars)
  return(invisible(NULL))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_apa_print_minterm_exp <- function(bdd, nvars, precision = 6L) {
  .Call(c_cudd_bdd_apa_print_minterm_exp, .cudd_bdd_ptr(bdd), nvars, precision)
  return(invisible(NULL))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_ldbl_count_minterm <- function(bdd, nvars) {
  return(.Call(c_cudd_bdd_ldbl_count_minterm, .cudd_bdd_ptr(bdd), nvars))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_shortest_path <- function(bdd, weight = NULL) {
  result <- .Call(c_cudd_bdd_shortest_path, .cudd_bdd_ptr(bdd), weight)
  output <- list(
    bdd = .cudd_bdd_wrap(result[[1L]], bdd),
    support = result[[2L]],
    length = result[[3L]]
  )
  return(output)
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_largest_cube <- function(bdd) {
  result <- .Call(c_cudd_bdd_largest_cube, .cudd_bdd_ptr(bdd))
  output <- list(
    bdd = .cudd_bdd_wrap(result[[1L]], bdd),
    length = result[[2L]]
  )
  return(output)
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_shortest_length <- function(bdd, weight = NULL) {
  return(.Call(c_cudd_bdd_shortest_length, .cudd_bdd_ptr(bdd), weight))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_equiv_dc <- function(bdd, g, d) {
  if (!.cudd_check_same_manager(bdd, g, "EquivDC") ||
        !.cudd_check_same_manager(bdd, d, "EquivDC")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  return(.Call(c_cudd_bdd_equiv_dc, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(g), .cudd_bdd_ptr(d)))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_cof_minterm <- function(bdd) {
  return(.Call(c_cudd_bdd_cof_minterm, .cudd_bdd_ptr(bdd)))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_find_essential <- function(bdd) {
  ptr <- .Call(c_cudd_bdd_find_essential, .cudd_bdd_ptr(bdd))
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_print_two_literal_clauses <- function(bdd) { # nolint: object_length_linter.
  .Call(c_cudd_bdd_print_two_literal_clauses, .cudd_bdd_ptr(bdd))
  return(invisible(NULL))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_count_minterm <- function(bdd, nvars) {
  return(.Call(c_cudd_bdd_count_minterm, .cudd_bdd_ptr(bdd), nvars))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_count_path <- function(bdd) {
  return(.Call(c_cudd_bdd_count_path, .cudd_bdd_ptr(bdd)))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_support <- function(bdd) {
  ptr <- .Call(c_cudd_bdd_support, .cudd_bdd_ptr(bdd))
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_support_size <- function(bdd) {
  return(.Call(c_cudd_bdd_support_size, .cudd_bdd_ptr(bdd)))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_support_indices <- function(bdd) {
  return(.Call(c_cudd_bdd_support_indices, .cudd_bdd_ptr(bdd)))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_classify_support <- function(bdd, other) {
  if (!.cudd_check_same_manager(bdd, other, "ClassifySupport")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  result <- .Call(c_cudd_bdd_classify_support, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(other))
  output <- .cudd_bdd_wrap_list(result, bdd)
  names(output) <- c("common", "only_bdd", "only_other")
  return(output)
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_count_leaves <- function(bdd) {
  return(.Call(c_cudd_bdd_count_leaves, .cudd_bdd_ptr(bdd)))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_density <- function(bdd, nvars) {
  return(.Call(c_cudd_bdd_density, .cudd_bdd_ptr(bdd), nvars))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_under_approx <- function(bdd, num_vars, threshold = 0L, safe = FALSE, quality = 1.0) {
  ptr <- .Call(
    c_cudd_bdd_under_approx,
    .cudd_bdd_ptr(bdd),
    num_vars,
    threshold,
    safe,
    quality
  )
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_over_approx <- function(bdd, num_vars, threshold = 0L, safe = FALSE, quality = 1.0) {
  ptr <- .Call(
    c_cudd_bdd_over_approx,
    .cudd_bdd_ptr(bdd),
    num_vars,
    threshold,
    safe,
    quality
  )
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_remap_under_approx <- function(bdd, num_vars, threshold = 0L, quality = 1.0) {
  ptr <- .Call(
    c_cudd_bdd_remap_under_approx,
    .cudd_bdd_ptr(bdd),
    num_vars,
    threshold,
    quality
  )
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_remap_over_approx <- function(bdd, num_vars, threshold = 0L, quality = 1.0) {
  ptr <- .Call(
    c_cudd_bdd_remap_over_approx,
    .cudd_bdd_ptr(bdd),
    num_vars,
    threshold,
    quality
  )
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_biased_under_approx <- function(bdd, bias, num_vars, threshold = 0L, quality1 = 1.0, quality0 = 1.0) {
  if (!.cudd_check_same_manager(bdd, bias, "BiasedUnderApprox")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(
    c_cudd_bdd_biased_under_approx,
    .cudd_bdd_ptr(bdd),
    .cudd_bdd_ptr(bias),
    num_vars,
    threshold,
    quality1,
    quality0
  )
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_biased_over_approx <- function(bdd, bias, num_vars, threshold = 0L, quality1 = 1.0, quality0 = 1.0) {
  if (!.cudd_check_same_manager(bdd, bias, "BiasedOverApprox")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(
    c_cudd_bdd_biased_over_approx,
    .cudd_bdd_ptr(bdd),
    .cudd_bdd_ptr(bias),
    num_vars,
    threshold,
    quality1,
    quality0
  )
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_leq <- function(bdd, other) {
  if (!.cudd_check_same_manager(bdd, other, "Leq")) {
    stop("Cannot compare BDDs from different CuddManager instances.", call. = FALSE)
  }
  return(.Call(c_cudd_bdd_leq, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(other)))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_and_abstract <- function(bdd, other, cube, limit = 0L) {
  if (!.cudd_check_same_manager(bdd, other, "AndAbstract") ||
        !.cudd_check_same_manager(bdd, cube, "AndAbstract")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(
    c_cudd_bdd_and_abstract,
    .cudd_bdd_ptr(bdd),
    .cudd_bdd_ptr(other),
    .cudd_bdd_ptr(cube),
    limit
  )
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_exist_abstract <- function(bdd, cube, limit = 0L) {
  if (!.cudd_check_same_manager(bdd, cube, "ExistAbstract")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_exist_abstract, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(cube), limit)
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_univ_abstract <- function(bdd, cube) {
  if (!.cudd_check_same_manager(bdd, cube, "UnivAbstract")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_univ_abstract, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(cube))
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_xor_exist_abstract <- function(bdd, other, cube) {
  if (!.cudd_check_same_manager(bdd, other, "XorExistAbstract") ||
        !.cudd_check_same_manager(bdd, cube, "XorExistAbstract")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(
    c_cudd_bdd_xor_exist_abstract,
    .cudd_bdd_ptr(bdd),
    .cudd_bdd_ptr(other),
    .cudd_bdd_ptr(cube)
  )
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_boolean_diff <- function(bdd, index) {
  ptr <- .Call(c_cudd_bdd_boolean_diff, .cudd_bdd_ptr(bdd), index)
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_var_is_dependent <- function(bdd, var) {
  if (!.cudd_check_same_manager(bdd, var, "VarIsDependent")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  return(.Call(c_cudd_bdd_var_is_dependent, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(var)))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_correlation <- function(bdd, other) {
  if (!.cudd_check_same_manager(bdd, other, "Correlation")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  return(.Call(c_cudd_bdd_correlation, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(other)))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_correlation_weights <- function(bdd, other, prob) {
  if (!.cudd_check_same_manager(bdd, other, "CorrelationWeights")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  return(.Call(c_cudd_bdd_correlation_weights, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(other), prob))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_xor_method <- function(bdd, other) {
  if (!.cudd_check_same_manager(bdd, other, "Xor")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_xor_method, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(other))
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_ite <- function(bdd, g, h, limit = 0L) {
  if (!.cudd_check_same_manager(bdd, g, "Ite") ||
        !.cudd_check_same_manager(bdd, h, "Ite")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(
    c_cudd_bdd_ite,
    .cudd_bdd_ptr(bdd),
    .cudd_bdd_ptr(g),
    .cudd_bdd_ptr(h),
    limit
  )
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_ite_constant <- function(bdd, g, h) {
  if (!.cudd_check_same_manager(bdd, g, "IteConstant") ||
        !.cudd_check_same_manager(bdd, h, "IteConstant")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_ite_constant, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(g), .cudd_bdd_ptr(h))
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_intersect <- function(bdd, other) {
  if (!.cudd_check_same_manager(bdd, other, "Intersect")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_intersect, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(other))
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_and_limit <- function(bdd, other, limit = 0L) {
  if (!.cudd_check_same_manager(bdd, other, "And")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_and_limit, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(other), limit)
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_or_limit <- function(bdd, other, limit = 0L) {
  if (!.cudd_check_same_manager(bdd, other, "Or")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_or_limit, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(other), limit)
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_nand <- function(bdd, other) {
  if (!.cudd_check_same_manager(bdd, other, "Nand")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_nand, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(other))
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_nor <- function(bdd, other) {
  if (!.cudd_check_same_manager(bdd, other, "Nor")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_nor, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(other))
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_xnor <- function(bdd, other, limit = 0L) {
  if (!.cudd_check_same_manager(bdd, other, "Xnor")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_xnor, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(other), limit)
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_cofactor <- function(bdd, other) {
  if (!.cudd_check_same_manager(bdd, other, "Cofactor")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_cofactor, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(other))
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_constrain <- function(bdd, other) {
  if (!.cudd_check_same_manager(bdd, other, "Constrain")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_constrain, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(other))
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_compose <- function(bdd, other, index) {
  if (!.cudd_check_same_manager(bdd, other, "Compose")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_compose, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(other), index)
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_permute <- function(bdd, permut) {
  ptr <- .Call(c_cudd_bdd_permute, .cudd_bdd_ptr(bdd), permut)
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_swap_variables <- function(bdd, x, y) {
  if (!.cudd_check_bdd_list_manager(bdd, x, "SwapVariables") ||
        !.cudd_check_bdd_list_manager(bdd, y, "SwapVariables")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_swap_variables, .cudd_bdd_ptr(bdd), lapply(x, .cudd_bdd_ptr), lapply(y, .cudd_bdd_ptr))
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_vector_compose <- function(bdd, vector) {
  if (!.cudd_check_bdd_list_manager(bdd, vector, "VectorCompose")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_vector_compose, .cudd_bdd_ptr(bdd), lapply(vector, .cudd_bdd_ptr))
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_approx_conj_decomp <- function(bdd) {
  result <- .Call(c_cudd_bdd_approx_conj_decomp, .cudd_bdd_ptr(bdd))
  output <- .cudd_bdd_wrap_list(result, bdd)
  names(output) <- c("g", "h")
  return(output)
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_approx_disj_decomp <- function(bdd) {
  result <- .Call(c_cudd_bdd_approx_disj_decomp, .cudd_bdd_ptr(bdd))
  output <- .cudd_bdd_wrap_list(result, bdd)
  names(output) <- c("g", "h")
  return(output)
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_iter_conj_decomp <- function(bdd) {
  result <- .Call(c_cudd_bdd_iter_conj_decomp, .cudd_bdd_ptr(bdd))
  output <- .cudd_bdd_wrap_list(result, bdd)
  names(output) <- c("g", "h")
  return(output)
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_iter_disj_decomp <- function(bdd) {
  result <- .Call(c_cudd_bdd_iter_disj_decomp, .cudd_bdd_ptr(bdd))
  output <- .cudd_bdd_wrap_list(result, bdd)
  names(output) <- c("g", "h")
  return(output)
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_gen_conj_decomp <- function(bdd) {
  result <- .Call(c_cudd_bdd_gen_conj_decomp, .cudd_bdd_ptr(bdd))
  output <- .cudd_bdd_wrap_list(result, bdd)
  names(output) <- c("g", "h")
  return(output)
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_gen_disj_decomp <- function(bdd) {
  result <- .Call(c_cudd_bdd_gen_disj_decomp, .cudd_bdd_ptr(bdd))
  output <- .cudd_bdd_wrap_list(result, bdd)
  names(output) <- c("g", "h")
  return(output)
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_var_conj_decomp <- function(bdd) {
  result <- .Call(c_cudd_bdd_var_conj_decomp, .cudd_bdd_ptr(bdd))
  output <- .cudd_bdd_wrap_list(result, bdd)
  names(output) <- c("g", "h")
  return(output)
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_var_disj_decomp <- function(bdd) {
  result <- .Call(c_cudd_bdd_var_disj_decomp, .cudd_bdd_ptr(bdd))
  output <- .cudd_bdd_wrap_list(result, bdd)
  names(output) <- c("g", "h")
  return(output)
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_li_compaction <- function(bdd, other) {
  if (!.cudd_check_same_manager(bdd, other, "LICompaction")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_li_compaction, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(other))
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_squeeze <- function(bdd, other) {
  if (!.cudd_check_same_manager(bdd, other, "Squeeze")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_squeeze, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(other))
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_interpolate <- function(bdd, other) {
  if (!.cudd_check_same_manager(bdd, other, "Interpolate")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_interpolate, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(other))
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_minimize <- function(bdd, other) {
  if (!.cudd_check_same_manager(bdd, other, "Minimize")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_minimize, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(other))
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_subset_compress <- function(bdd, nvars, threshold) {
  ptr <- .Call(c_cudd_bdd_subset_compress, .cudd_bdd_ptr(bdd), nvars, threshold)
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_superset_compress <- function(bdd, nvars, threshold) {
  ptr <- .Call(c_cudd_bdd_superset_compress, .cudd_bdd_ptr(bdd), nvars, threshold)
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_literal_set_intersection <- function(bdd, other) { # nolint: object_length_linter.
  if (!.cudd_check_same_manager(bdd, other, "LiteralSetIntersection")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_literal_set_intersection, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(other))
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_c_projection <- function(bdd, other) {
  if (!.cudd_check_same_manager(bdd, other, "CProjection")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_c_projection, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(other))
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_min_hamming_dist <- function(bdd, minterm, upper_bound) {
  result <- .Call(c_cudd_bdd_min_hamming_dist, .cudd_bdd_ptr(bdd), minterm, upper_bound)
  names(result) <- c("distance", "minterm")
  return(result)
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_eval <- function(bdd, inputs) {
  ptr <- .Call(c_cudd_bdd_eval, .cudd_bdd_ptr(bdd), inputs)
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_decreasing <- function(bdd, index) {
  ptr <- .Call(c_cudd_bdd_decreasing, .cudd_bdd_ptr(bdd), index)
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_increasing <- function(bdd, index) {
  ptr <- .Call(c_cudd_bdd_increasing, .cudd_bdd_ptr(bdd), index)
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_make_prime <- function(bdd, other) {
  if (!.cudd_check_same_manager(bdd, other, "MakePrime")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_make_prime, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(other))
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_subset_heavy_branch <- function(bdd, num_vars, threshold) {
  ptr <- .Call(c_cudd_bdd_subset_heavy_branch, .cudd_bdd_ptr(bdd), num_vars, threshold)
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_superset_heavy_branch <- function(bdd, num_vars, threshold) {
  ptr <- .Call(c_cudd_bdd_superset_heavy_branch, .cudd_bdd_ptr(bdd), num_vars, threshold)
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_subset_short_paths <- function(bdd, num_vars, threshold, hardlimit = FALSE) {
  ptr <- .Call(c_cudd_bdd_subset_short_paths, .cudd_bdd_ptr(bdd), num_vars, threshold, hardlimit)
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_superset_short_paths <- function(bdd, num_vars, threshold, hardlimit = FALSE) {
  ptr <- .Call(c_cudd_bdd_superset_short_paths, .cudd_bdd_ptr(bdd), num_vars, threshold, hardlimit)
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_print_cover <- function(bdd) {
  .Call(c_cudd_bdd_print_cover, .cudd_bdd_ptr(bdd))
  return(invisible(NULL))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_print_cover_with_cube <- function(bdd, cube) {
  if (!.cudd_check_same_manager(bdd, cube, "PrintCover")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  .Call(c_cudd_bdd_print_cover_with_cube, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(cube))
  return(invisible(NULL))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_pick_one_cube <- function(bdd) {
  return(.Call(c_cudd_bdd_pick_one_cube, .cudd_bdd_ptr(bdd)))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_pick_one_minterm <- function(bdd, vars) {
  if (!.cudd_check_bdd_list_manager(bdd, vars, "PickOneMinterm")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_pick_one_minterm, .cudd_bdd_ptr(bdd), lapply(vars, .cudd_bdd_ptr))
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_isop <- function(bdd, upper) {
  if (!.cudd_check_same_manager(bdd, upper, "Isop")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_isop, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(upper))
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_port_to_zdd <- function(bdd) {
  ptr <- .Call(c_cudd_bdd_port_to_zdd, .cudd_bdd_ptr(bdd))
  return(.cudd_zdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_factored_form_string <- function(bdd) {
  return(.Call(c_cudd_bdd_factored_form_string, .cudd_bdd_ptr(bdd)))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_print_factored_form <- function(bdd) {
  .Call(c_cudd_bdd_print_factored_form, .cudd_bdd_ptr(bdd))
  return(invisible(NULL))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_clipping_and <- function(bdd, other, max_depth, direction = 0L) {
  if (!.cudd_check_same_manager(bdd, other, "ClippingAnd")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(
    c_cudd_bdd_clipping_and,
    .cudd_bdd_ptr(bdd),
    .cudd_bdd_ptr(other),
    max_depth,
    direction
  )
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_clipping_and_abstract <- function(bdd, other, cube, max_depth, direction = 0L) {
  if (!.cudd_check_same_manager(bdd, other, "ClippingAndAbstract") ||
        !.cudd_check_same_manager(bdd, cube, "ClippingAndAbstract")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(
    c_cudd_bdd_clipping_and_abstract,
    .cudd_bdd_ptr(bdd),
    .cudd_bdd_ptr(other),
    .cudd_bdd_ptr(cube),
    max_depth,
    direction
  )
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_var_are_symmetric <- function(bdd, index1, index2) {
  return(.Call(c_cudd_bdd_var_are_symmetric, .cudd_bdd_ptr(bdd), index1, index2))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_adj_permute_x <- function(bdd, x) {
  if (!.cudd_check_bdd_list_manager(bdd, x, "AdjPermuteX")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_adj_permute_x, .cudd_bdd_ptr(bdd), lapply(x, .cudd_bdd_ptr))
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_is_var_essential <- function(bdd, index, phase) {
  return(.Call(c_cudd_bdd_is_var_essential, .cudd_bdd_ptr(bdd), index, phase))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_np_and <- function(bdd, other) {
  if (!.cudd_check_same_manager(bdd, other, "NPAnd")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_np_and, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(other))
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_constrain_decomp <- function(bdd) {
  result <- .Call(c_cudd_bdd_constrain_decomp, .cudd_bdd_ptr(bdd))
  return(.cudd_bdd_wrap_list(result, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_char_to_vect <- function(bdd) {
  result <- .Call(c_cudd_bdd_char_to_vect, .cudd_bdd_ptr(bdd))
  return(.cudd_bdd_wrap_list(result, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_leq_unless <- function(bdd, g, d) {
  if (!.cudd_check_same_manager(bdd, g, "LeqUnless") ||
        !.cudd_check_same_manager(bdd, d, "LeqUnless")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  return(.Call(c_cudd_bdd_leq_unless, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(g), .cudd_bdd_ptr(d)))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_maximally_expand <- function(bdd, ub, f) {
  if (!.cudd_check_same_manager(bdd, ub, "MaximallyExpand") ||
        !.cudd_check_same_manager(bdd, f, "MaximallyExpand")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(
    c_cudd_bdd_maximally_expand,
    .cudd_bdd_ptr(bdd),
    .cudd_bdd_ptr(ub),
    .cudd_bdd_ptr(f)
  )
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_largest_prime_unate <- function(bdd, phases) {
  if (!.cudd_check_same_manager(bdd, phases, "LargestPrimeUnate")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_largest_prime_unate, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(phases))
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_solve_eqn <- function(bdd, y_bdd, n) {
  if (!.cudd_check_same_manager(bdd, y_bdd, "SolveEqn")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  result <- .Call(c_cudd_bdd_solve_eqn, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(y_bdd), n)
  output <- list(
    bdd = .cudd_bdd_wrap(result[[1L]], bdd),
    g = .cudd_bdd_wrap_list(result[[2L]], bdd),
    y_index = result[[3L]]
  )
  return(output)
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_verify_sol <- function(bdd, g_list, y_index) {
  if (!.cudd_check_bdd_list_manager(bdd, g_list, "VerifySol")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(
    c_cudd_bdd_verify_sol,
    .cudd_bdd_ptr(bdd),
    lapply(g_list, .cudd_bdd_ptr),
    y_index
  )
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_split_set <- function(bdd, x_vars, m) {
  if (!.cudd_check_bdd_list_manager(bdd, x_vars, "SplitSet")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  ptr <- .Call(c_cudd_bdd_split_set, .cudd_bdd_ptr(bdd), lapply(x_vars, .cudd_bdd_ptr), m)
  return(.cudd_bdd_wrap(ptr, bdd))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_estimate_cofactor <- function(bdd, index, phase) {
  return(.Call(c_cudd_bdd_estimate_cofactor, .cudd_bdd_ptr(bdd), index, phase))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_estimate_cofactor_simple <- function(bdd, index) { # nolint: object_length_linter.
  return(.Call(c_cudd_bdd_estimate_cofactor_simple, .cudd_bdd_ptr(bdd), index))
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_zdd_isop <- function(bdd, upper) {
  if (!.cudd_check_same_manager(bdd, upper, "zddIsop")) {
    stop("Cannot combine BDDs from different CuddManager instances.", call. = FALSE)
  }
  result <- .Call(c_cudd_bdd_zdd_isop, .cudd_bdd_ptr(bdd), .cudd_bdd_ptr(upper))
  output <- list(
    bdd = .cudd_bdd_wrap(result[[1L]], bdd),
    zdd = .cudd_zdd_wrap(result[[2L]], bdd)
  )
  return(output)
}

#' @rdname cudd_bdd_methods
#' @export
cudd_bdd_transfer <- function(bdd, manager) {
  ptr <- .Call(c_cudd_bdd_transfer, .cudd_bdd_ptr(bdd), .cudd_manager_ptr(manager))
  return(methods::new("CuddBDD", ptr = ptr, manager_ptr = .cudd_manager_ptr(manager)))
}
