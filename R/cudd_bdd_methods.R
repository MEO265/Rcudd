.cudd_bdd_wrap <- function(ptr, bdd) {
  return(methods::new("CuddBDD", ptr = ptr, manager_ptr = .cudd_bdd_manager_ptr(bdd)))
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
#' @param other,cube,var,g,h,upper Additional [`CuddBDD`] instances used by the
#'   operation.
#' @param limit Optional non-negative integer limit passed to CUDD.
#' @param index Integer index values used by the operation.
#' @param permut Integer vector describing a variable permutation.
#' @param x,y,vector,vars Lists of [`CuddBDD`] instances.
#' @param nvars,num_vars,threshold Non-negative integer values used by the operation.
#' @param minterm,inputs Integer vectors used by the operation.
#' @param upper_bound Integer upper bound for min hamming distance.
#' @param hardlimit Logical scalar for short path routines.
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
cudd_bdd_is_var <- function(bdd) {
  return(.Call(c_cudd_bdd_is_var, .cudd_bdd_ptr(bdd)))
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
