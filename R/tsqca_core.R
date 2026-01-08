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

#' Get the number of intermediate solutions
#'
#' @param sol A solution object returned by \code{QCA::minimize()}.
#' @return Integer. Number of intermediate solutions, or 0 if none.
#' @keywords internal
get_n_solutions <- function(sol) {
  if (is.null(sol)) return(0L)
  
  # Use sol$solution as primary source (contains correct distinct solutions)
  sol_list <- try(sol$solution, silent = TRUE)
  if (!inherits(sol_list, "try-error") && !is.null(sol_list) && length(sol_list) > 0) {
    return(length(sol_list))
  }
  
  # Fallback: count from i.sol structure
  if (!is.null(sol$i.sol) && length(sol$i.sol) > 0) {
    total_count <- 0L
    for (model_name in names(sol$i.sol)) {
      model_sols <- sol$i.sol[[model_name]]$solution
      if (!is.null(model_sols) && length(model_sols) > 0) {
        total_count <- total_count + length(model_sols)
      }
    }
    if (total_count > 0) {
      return(total_count)
    }
  }
  
  return(0L)
}

#' Extract solution information from a QCA minimization result
#'
#' Internal helper to obtain the solution expression, consistency
#' (\code{inclS}) and coverage (\code{covS}) from an object returned by
#' \code{QCA::minimize()}.
#'
#' @param sol A solution object returned by \code{QCA::minimize()}.
#' @param extract_mode Character. How to handle multiple intermediate solutions:
#'   \itemize{
#'     \item \code{"first"} - return only the first solution (M1). Default.
#'     \item \code{"all"} - return all solutions concatenated.
#'     \item \code{"essential"} - return essential prime implicants (terms 
#'       common to all solutions), plus selective prime implicants and 
#'       solution count.
#'   }
#'
#' @return A list with elements depending on \code{extract_mode}.
#'
#'   For \code{"first"}: \code{expression}, \code{inclS}, \code{covS}.
#'
#'   For \code{"all"}: adds \code{n_solutions}.
#'
#'   For \code{"essential"}: adds \code{selective_terms}, \code{unique_terms},
#'   \code{n_solutions}.
#'
#'   If extraction fails, returns \code{"No solution"} and \code{NA_real_}
#'   for numeric values.
#' @keywords internal
qca_extract <- function(sol, extract_mode = c("first", "all", "essential")) {
  
  extract_mode <- match.arg(extract_mode)
  
  # Base null response
  null_response <- function(mode) {
    base <- list(
      expression   = "No solution",
      inclS        = NA_real_,
      covS         = NA_real_,
      n_solutions  = 0L
    )
    if (mode == "essential") {
      base$selective_terms <- NA_character_
      base$unique_terms     <- NA_character_
    }
    base
  }
  
  if (is.null(sol)) {
    return(null_response(extract_mode))
  }
  
  # === Use sol$solution as the primary source (contains correct distinct solutions) ===
  sol_list <- NULL
  
  # Try sol$solution first (preferred - contains correct distinct solutions)
  if (!is.null(sol$solution) && length(sol$solution) > 0) {
    sol_list <- sol$solution
  }
  
  # Fallback: try i.sol entries
  if (is.null(sol_list) || length(sol_list) == 0) {
    sol_list <- try(sol$i.sol$C1P1$solution, silent = TRUE)
    if (inherits(sol_list, "try-error") || is.null(sol_list) || length(sol_list) == 0) {
      # Try first i.sol entry
      if (!is.null(sol$i.sol) && length(sol$i.sol) > 0) {
        sol_list <- try(sol$i.sol[[1]]$solution, silent = TRUE)
        if (inherits(sol_list, "try-error")) sol_list <- NULL
      }
    }
  }
  
  if (is.null(sol_list) || length(sol_list) == 0) {
    return(null_response(extract_mode))
  }
  
  # === FIXED: Try multiple paths to get metrics ===
  inclS <- NA_real_
  covS <- NA_real_
  
  # Path 1: sol$IC$sol.incl.cov (for single solution without dir.exp)
  if (is.na(inclS)) {
    incl_val <- try(sol$IC$sol.incl.cov$inclS, silent = TRUE)
    if (!inherits(incl_val, "try-error") && !is.null(incl_val)) {
      inclS <- incl_val
    }
  }
  if (is.na(covS)) {
    cov_val <- try(sol$IC$sol.incl.cov$covS, silent = TRUE)
    if (!inherits(cov_val, "try-error") && !is.null(cov_val)) {
      covS <- cov_val
    }
  }
  
  # Path 2: sol$IC$overall (for multiple solutions - overall metrics)
  if (is.na(inclS)) {
    incl_val <- try(sol$IC$overall$sol.incl.cov$inclS, silent = TRUE)
    if (!inherits(incl_val, "try-error") && !is.null(incl_val)) {
      inclS <- incl_val
    }
  }
  if (is.na(covS)) {
    cov_val <- try(sol$IC$overall$sol.incl.cov$covS, silent = TRUE)
    if (!inherits(cov_val, "try-error") && !is.null(cov_val)) {
      covS <- cov_val
    }
  }
  
  # Path 3: sol$i.sol$C1P1$IC$sol.incl.cov (for intermediate solutions with dir.exp)
  if (is.na(inclS)) {
    incl_val <- try(sol$i.sol$C1P1$IC$sol.incl.cov$inclS, silent = TRUE)
    if (!inherits(incl_val, "try-error") && !is.null(incl_val)) {
      inclS <- incl_val
    }
  }
  if (is.na(covS)) {
    cov_val <- try(sol$i.sol$C1P1$IC$sol.incl.cov$covS, silent = TRUE)
    if (!inherits(cov_val, "try-error") && !is.null(cov_val)) {
      covS <- cov_val
    }
  }
  
  # Path 4: First element of i.sol
  if (is.na(inclS) && !is.null(sol$i.sol) && length(sol$i.sol) > 0) {
    incl_val <- try(sol$i.sol[[1]]$IC$sol.incl.cov$inclS, silent = TRUE)
    if (!inherits(incl_val, "try-error") && !is.null(incl_val)) {
      inclS <- incl_val
    }
  }
  if (is.na(covS) && !is.null(sol$i.sol) && length(sol$i.sol) > 0) {
    cov_val <- try(sol$i.sol[[1]]$IC$sol.incl.cov$covS, silent = TRUE)
    if (!inherits(cov_val, "try-error") && !is.null(cov_val)) {
      covS <- cov_val
    }
  }
  
  # Get total solution count (always use get_n_solutions for consistency)
  n_solutions <- get_n_solutions(sol)
  
  # Mode-specific processing
  if (extract_mode == "first") {
    expression <- paste(sol_list[[1]], collapse = " + ")
    return(list(
      expression  = expression,
      inclS       = inclS,
      covS        = covS,
      n_solutions = n_solutions
    ))
  }
  
  if (extract_mode == "all") {
    all_exprs <- sapply(seq_along(sol_list), function(i) {
      paste0("M", i, ": ", paste(sol_list[[i]], collapse = " + "))
    })
    expression <- paste(all_exprs, collapse = "; ")
    return(list(
      expression  = expression,
      inclS       = inclS,
      covS        = covS,
      n_solutions = n_solutions
    ))
  }
  
  if (extract_mode == "essential") {
    # Split each solution into terms
    sol_terms <- lapply(sol_list, function(x) {
      unlist(strsplit(paste(x, collapse = " + "), " \\+ "))
    })
    
    # Essential prime implicants: intersection of all solutions
    essential_terms <- Reduce(intersect, sol_terms)
    
    # All terms: union of all solutions
    all_terms <- Reduce(union, sol_terms)
    
    # Selective prime implicants: in some but not all solutions
    selective_terms <- setdiff(all_terms, essential_terms)
    
    # Unique terms: only in specific solution
    if (n_solutions > 1) {
      unique_terms_list <- lapply(seq_along(sol_terms), function(i) {
        other_terms <- unique(unlist(sol_terms[-i]))
        setdiff(sol_terms[[i]], other_terms)
      })
      unique_terms_formatted <- sapply(seq_along(unique_terms_list), function(i) {
        if (length(unique_terms_list[[i]]) > 0) {
          paste0("M", i, ":", paste(unique_terms_list[[i]], collapse = "+"))
        } else {
          NULL
        }
      })
      unique_terms_str <- paste(unique_terms_formatted[!sapply(unique_terms_formatted, is.null)],
                                collapse = "; ")
      if (unique_terms_str == "") unique_terms_str <- NA_character_
    } else {
      unique_terms_str <- NA_character_
    }
    
    # Essential expression
    expression <- if (length(essential_terms) > 0) {
      paste(essential_terms, collapse = " + ")
    } else {
      "No essential prime implicants"
    }
    
    # Selective expression
    selective_str <- if (length(selective_terms) > 0) {
      paste(selective_terms, collapse = " + ")
    } else {
      NA_character_
    }
    
    return(list(
      expression       = expression,
      inclS            = inclS,
      covS             = covS,
      selective_terms  = selective_str,
      unique_terms     = unique_terms_str,
      n_solutions      = n_solutions
    ))
  }
}
