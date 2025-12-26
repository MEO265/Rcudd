# R Wrapper for CUDD: CU Decision Diagram Package

<!-- badges: start -->

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/Rcudd)](https://cran.r-project.org/package=Rcudd)
[![R-CMD-check](https://github.com/MEO265/Rcudd/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MEO265/Rcudd/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/MEO265/Rcudd/graph/badge.svg)](https://codecov.io/gh/MEO265/Rcudd)

------------------------------------------------------------------------

<!-- badges: end -->

`Rucdd` is an (sofar very basic) R wrapper for the [CUDD C-library](https://github.com/cuddorg/cudd), 
for the manipulation of decision diagrams which supports binary decision diagrams (BDDs),
algebraic decision diagrams (ADDs), and Zero-Suppressed BDDs (ZDDs).

## Capabilities

At this stage, the package provides functions based on the CUDD library and especially its C++ object-oriented wrapper 
but does not grant any possibility to use the C++ classes using R objects directly.

## Contribution

If you are interested in contributing to this package, feel free to create an issue or a pull request.
Especially, help with porting the C++ classes and all their methods to R would be highly appreciated.

## Acknowledgments
The CUDD library on which this package is based was developed at the University of Colorado and was published under the BSD license.
No connection to the original authors is implied or should be inferred.
