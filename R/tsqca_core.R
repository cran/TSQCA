###############################################
# Core utilities for TSQCA
###############################################

#' Binary calibration helper for TSQCA
#'
#' Converts a numeric vector into a crisp set (0/1) based on a threshold.
#'
#' @param x Numeric vector.
#' @param thr Numeric scalar. Cases with \code{x >= thr} are coded as 1,
#'   others as 0.
#'
#' @return Integer vector of 0/1 with the same length as \code{x}.
#' @keywords internal
qca_bin <- function(x, thr) {
  ifelse(x >= thr, 1L, 0L)
}

#' Extract solution information from a QCA minimization result
#'
#' Internal helper to obtain the solution expression, consistency
#' (\code{inclS}) and coverage (\code{covS}) from an object returned by
#' \code{QCA::minimize()}.
#'
#' @param sol A solution object returned by \code{QCA::minimize()}.
#'
#' @return A list with elements \code{expression}, \code{inclS}, \code{covS}.
#'   If extraction fails, returns \code{"No solution"} and \code{NA_real_}
#'   for the numeric values.
#' @keywords internal
qca_extract <- function(sol) {
  if (is.null(sol)) {
    return(list(
      expression = "No solution",
      inclS      = NA_real_,
      covS       = NA_real_
    ))
  }
  
  expr_vec <- try(sol$i.sol$C1P1$solution[[1]], silent = TRUE)
  expression <- if (inherits(expr_vec, "try-error") || is.null(expr_vec)) {
    "No solution"
  } else {
    paste(expr_vec, collapse = " + ")
  }
  
  incl_val <- try(sol$i.sol$C1P1$IC$sol.incl.cov$inclS, silent = TRUE)
  inclS <- if (inherits(incl_val, "try-error") || is.null(incl_val)) {
    NA_real_
  } else {
    incl_val
  }
  
  cov_val <- try(sol$i.sol$C1P1$IC$sol.incl.cov$covS, silent = TRUE)
  covS <- if (inherits(cov_val, "try-error") || is.null(cov_val)) {
    NA_real_
  } else {
    cov_val
  }
  
  list(
    expression = expression,
    inclS      = inclS,
    covS       = covS
  )
}
