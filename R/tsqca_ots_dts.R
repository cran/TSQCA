###############################################
# OTS–QCA (Y sweep) and DTS–QCA (2D sweep)
# v0.3.0: QCA-compatible argument names + negated outcome support
###############################################

#' OTS–QCA: Outcome threshold sweep
#'
#' Sweeps the threshold of the outcome Y while keeping the thresholds of
#' all X conditions fixed.
#'
#' @param dat Data frame containing the outcome and condition variables.
#' @param outcome Character. Outcome variable name. Supports negation with
#'   tilde prefix (e.g., \code{"~Y"}) following QCA package conventions.
#' @param conditions Character vector. Names of condition variables.
#' @param sweep_range Numeric vector. Candidate thresholds for Y.
#' @param thrX Named numeric vector. Fixed thresholds for X variables,
#'   with names matching \code{conditions}.
#' @param dir.exp Directional expectations for \code{minimize}.
#'   If \code{NULL}, all set to 1.
#' @param include Inclusion rule for \code{minimize}.
#' @param incl.cut Consistency cutoff for \code{truthTable}.
#' @param n.cut Frequency cutoff for \code{truthTable}.
#' @param pri.cut PRI cutoff for \code{minimize}.
#' @param extract_mode Character. How to handle multiple solutions:
#'   \code{"first"} (default), \code{"all"}, or \code{"essential"}.
#'   See \code{\link{qca_extract}} for details.
#' @param return_details Logical. If \code{TRUE} (default), returns both
#'   summary and detailed objects for use with \code{generate_report()}.
#' @param Yvar Deprecated. Use \code{outcome} instead.
#' @param Xvars Deprecated. Use \code{conditions} instead.
#'
#' @return
#' If \code{return_details = FALSE}, a data frame with columns:
#' \itemize{
#'   \item \code{thrY} — threshold for Y
#'   \item \code{expression} — minimized solution expression
#'   \item \code{inclS} — solution consistency
#'   \item \code{covS} — solution coverage
#'   \item (additional columns depending on \code{extract_mode})
#' }
#'
#' If \code{return_details = TRUE}, a list with:
#' \itemize{
#'   \item \code{summary} — the data frame above
#'   \item \code{details} — per-Y-threshold list of
#'     \code{thrY}, \code{thrX_vec}, \code{truth_table}, \code{solution}
#' }
#'
#' @importFrom QCA truthTable minimize
#' @export
#' @examples
#' # Load sample data
#' data(sample_data)
#' 
#' # Set fixed thresholds for conditions
#' thrX <- c(X1 = 7, X2 = 7, X3 = 7)
#' 
#' # Run outcome threshold sweep (standard)
#' result <- otSweep(
#'   dat = sample_data,
#'   outcome = "Y",
#'   conditions = c("X1", "X2", "X3"),
#'   sweep_range = 6:9,
#'   thrX = thrX
#' )
#' head(result$summary)
#' 
#' # Run with negated outcome (~Y)
#' # Analyzes conditions for Y < threshold
#' result_neg <- otSweep(
#'   dat = sample_data,
#'   outcome = "~Y",
#'   conditions = c("X1", "X2", "X3"),
#'   sweep_range = 6:9,
#'   thrX = thrX
#' )
#' head(result_neg$summary)
otSweep <- function(dat, 
                    outcome = NULL, conditions = NULL,
                    sweep_range, thrX,
                    dir.exp = NULL, include = "?",
                    incl.cut = 0.8, n.cut = 1, pri.cut = 0,
                    extract_mode = c("first", "all", "essential"),
                    return_details = TRUE,
                    Yvar = NULL, Xvars = NULL) {
  
  # === Backward compatibility for deprecated arguments ===
  if (!is.null(Yvar) && is.null(outcome)) {
    outcome <- Yvar
    warning("Argument 'Yvar' is deprecated. Use 'outcome' instead.",
            call. = FALSE)
  }
  if (!is.null(Xvars) && is.null(conditions)) {
    conditions <- Xvars
    warning("Argument 'Xvars' is deprecated. Use 'conditions' instead.",
            call. = FALSE)
  }
  
  # === Validate required arguments ===
  if (is.null(outcome)) {
    stop("Argument 'outcome' is required.")
  }
  if (is.null(conditions)) {
    stop("Argument 'conditions' is required.")
  }
  
  # === Handle negated outcome ===
  negate_outcome <- grepl("^~", outcome)
  outcome_clean <- sub("^~", "", outcome)
  
  # Validate outcome variable exists

  if (!outcome_clean %in% names(dat)) {
    stop("Variable '", outcome_clean, "' not found in data.")
  }
  
  # Validate condition variables exist
  missing_conds <- setdiff(conditions, names(dat))
  if (length(missing_conds) > 0) {
    stop("Condition variable(s) not found in data: ", 
         paste(missing_conds, collapse = ", "))
  }
  
  extract_mode <- match.arg(extract_mode)
  
  # Initialize output data frame based on extract_mode
  df_out <- data.frame(
    thrY       = numeric(0),
    expression = character(0),
    inclS      = numeric(0),
    covS       = numeric(0),
    n_solutions = integer(0),
    stringsAsFactors = FALSE
  )
  
  # Add columns based on extract_mode
  if (extract_mode == "essential") {
    df_out$selective_terms <- character(0)
    df_out$unique_terms     <- character(0)
  }
  
  details_list <- list()
  
  # Track thresholds with multiple solutions (for warning in "first" mode)
  multi_sol_thresholds <- c()
  
  # Handle dir.exp: NULL or scalar -> expand to vector (before loop)
  local_dir.exp <- dir.exp
  if (is.null(local_dir.exp) || length(local_dir.exp) == 1) {
    local_dir.exp <- rep(if (is.null(dir.exp)) 1 else dir.exp[1], length(conditions))
    names(local_dir.exp) <- conditions
  }
  
  for (thrY in sweep_range) {
    
    # Binarize outcome variable (use cleaned name)
    dat_bin <- data.frame(Y = qca_bin(dat[[outcome_clean]], thrY))
    for (x in conditions) {
      dat_bin[[x]] <- qca_bin(dat[[x]], thrX[x])
    }
    
    # Determine outcome string for truthTable (with ~ if negated)
    outcome_tt <- if (negate_outcome) "~Y" else "Y"
    
    tt <- try(
      QCA::truthTable(
        dat_bin,
        outcome    = outcome_tt,
        conditions = conditions,
        show.cases = FALSE,
        incl.cut1  = incl.cut,
        n.cut      = n.cut,
        pri.cut    = pri.cut
      ),
      silent = TRUE
    )
    
    if (inherits(tt, "try-error")) {
      new_row <- data.frame(
        thrY        = thrY,
        expression  = "No solution",
        inclS       = NA_real_,
        covS        = NA_real_,
        n_solutions = 0L,
        stringsAsFactors = FALSE
      )
      
      if (extract_mode == "essential") {
        new_row$selective_terms <- NA_character_
        new_row$unique_terms     <- NA_character_
      }
      
      df_out <- rbind(df_out, new_row)
      
      if (return_details) {
        details_list[[as.character(thrY)]] <- list(
          thrY        = thrY,
          thrX_vec    = thrX,
          truth_table = NULL,
          solution    = NULL,
          dat_bin     = NULL
        )
      }
      next
    }
    
    sol <- try(
      QCA::minimize(
        tt,
        include    = include,
        dir.exp    = local_dir.exp,
        details    = TRUE,
        show.cases = FALSE,
        pri.cut    = pri.cut
      ),
      silent = TRUE
    )
    
    if (inherits(sol, "try-error")) {
      new_row <- data.frame(
        thrY        = thrY,
        expression  = "No solution",
        inclS       = NA_real_,
        covS        = NA_real_,
        n_solutions = 0L,
        stringsAsFactors = FALSE
      )
      
      if (extract_mode == "essential") {
        new_row$selective_terms <- NA_character_
        new_row$unique_terms     <- NA_character_
      }
      
      df_out <- rbind(df_out, new_row)
      
      if (return_details) {
        details_list[[as.character(thrY)]] <- list(
          thrY        = thrY,
          thrX_vec    = thrX,
          truth_table = tt,
          solution    = NULL,
          dat_bin     = dat_bin
        )
      }
      next
    }
    
    info <- qca_extract(sol, extract_mode = extract_mode)
    
    # Track multiple solutions (for warning)
    if (info$n_solutions > 1) {
      multi_sol_thresholds <- c(multi_sol_thresholds, thrY)
    }
    
    # Build result row based on extract_mode
    new_row <- data.frame(
      thrY        = thrY,
      expression  = info$expression,
      inclS       = info$inclS,
      covS        = info$covS,
      n_solutions = info$n_solutions,
      stringsAsFactors = FALSE
    )
    
    if (extract_mode == "essential") {
      new_row$selective_terms <- info$selective_terms
      new_row$unique_terms     <- info$unique_terms
    }
    
    df_out <- rbind(df_out, new_row)
    
    if (return_details) {
      details_list[[as.character(thrY)]] <- list(
        thrY        = thrY,
        thrX_vec    = thrX,
        truth_table = tt,
        solution    = sol,
        dat_bin     = dat_bin
      )
    }
  }
  
  # Issue warning for multiple solutions
  if (length(multi_sol_thresholds) > 0) {
    warning(
      "Multiple intermediate solutions exist for thrY = ",
      paste(multi_sol_thresholds, collapse = ", "),
      " (n_solutions > 1). ",
      "Only the first solution (M1) and its fit metrics are shown. ",
      "Use generate_report() for full analysis.",
      call. = FALSE
    )
  }
  
  if (return_details) {
    result <- list(
      summary = df_out, 
      details = details_list,
      params = list(
        outcome = outcome,
        conditions = conditions,
        negate_outcome = negate_outcome,
        thrX = thrX,
        sweep_range = sweep_range,
        incl.cut = incl.cut,
        n.cut = n.cut,
        pri.cut = pri.cut,
        include = include,
        dir.exp = local_dir.exp
      )
    )
    class(result) <- c("otSweep_result", "tsqca_result", "list")
    return(result)
  }
  
  df_out
}


###############################################
# DTS–QCA (2D sweep)
###############################################

#' DTS–QCA: Two-dimensional X–Y threshold sweep
#'
#' Sweeps thresholds for multiple X variables and the outcome Y jointly.
#' For each combination of X thresholds and each candidate Y threshold, the
#' data are binarized and a crisp-set QCA is executed.
#'
#' @param dat Data frame containing the outcome and condition variables.
#' @param outcome Character. Outcome variable name. Supports negation with
#'   tilde prefix (e.g., \code{"~Y"}) following QCA package conventions.
#' @param conditions Character vector. Names of condition variables.
#' @param sweep_list_X Named list. Each element is a numeric vector of
#'   candidate thresholds for the corresponding X.
#' @param sweep_range_Y Numeric vector. Candidate thresholds for Y.
#' @param dir.exp Directional expectations for \code{minimize}.
#'   If \code{NULL}, all set to 1.
#' @param include Inclusion rule for \code{minimize}.
#' @param incl.cut Consistency cutoff for \code{truthTable}.
#' @param n.cut Frequency cutoff for \code{truthTable}.
#' @param pri.cut PRI cutoff for \code{minimize}.
#' @param extract_mode Character. How to handle multiple solutions:
#'   \code{"first"} (default), \code{"all"}, or \code{"essential"}.
#'   See \code{\link{qca_extract}} for details.
#' @param return_details Logical. If \code{TRUE} (default), returns both
#'   summary and detailed objects for use with \code{generate_report()}.
#' @param Yvar Deprecated. Use \code{outcome} instead.
#' @param Xvars Deprecated. Use \code{conditions} instead.
#'
#' @return
#' If \code{return_details = FALSE}, a data frame with columns:
#' \itemize{
#'   \item \code{combo_id} — index of threshold combination
#'   \item \code{thrY} — threshold for Y
#'   \item \code{thrX} — character summary of X thresholds
#'   \item \code{expression} — minimized solution expression
#'   \item \code{inclS} — solution consistency
#'   \item \code{covS} — solution coverage
#'   \item (additional columns depending on \code{extract_mode})
#' }
#'
#' If \code{return_details = TRUE}, a list with:
#' \itemize{
#'   \item \code{summary} — the data frame above
#'   \item \code{details} — list of runs with
#'     \code{combo_id}, \code{thrY}, \code{thrX_vec},
#'     \code{truth_table}, \code{solution}
#' }
#'
#' @importFrom QCA truthTable minimize
#' @examples
#' # Load sample data
#' data(sample_data)
#' 
#' # Quick demonstration with reduced complexity (< 5 seconds)
#' # Using 2 conditions and 2 threshold levels
#' sweep_list_X <- list(
#'   X1 = 6:7,  # Reduced from 6:8 to 6:7
#'   X2 = 6:7   # Reduced from 6:8 to 6:7
#' )
#' 
#' sweep_range_Y <- 6:7  # Reduced from 6:8 to 6:7
#' 
#' # Run dual threshold sweep with reduced parameters
#' # This explores 2 × 2^2 = 8 threshold combinations
#' result_quick <- dtSweep(
#'   dat = sample_data,
#'   outcome = "Y",
#'   conditions = c("X1", "X2"),  # Reduced from 3 to 2 conditions
#'   sweep_list_X = sweep_list_X,
#'   sweep_range_Y = sweep_range_Y
#' )
#' head(result_quick$summary)
#' 
#' \donttest{
#' # Full analysis with all conditions and thresholds
#' # This explores 3 × 3^3 = 81 threshold combinations (takes ~10-15 seconds)
#' sweep_list_X_full <- list(
#'   X1 = 6:8,
#'   X2 = 6:8,
#'   X3 = 6:8
#' )
#' 
#' sweep_range_Y_full <- 6:8
#' 
#' result_full <- dtSweep(
#'   dat = sample_data,
#'   outcome = "Y",
#'   conditions = c("X1", "X2", "X3"),
#'   sweep_list_X = sweep_list_X_full,
#'   sweep_range_Y = sweep_range_Y_full
#' )
#' 
#' # Analyze threshold-dependent causal structures
#' head(result_full$summary)
#' }
#' @export
dtSweep <- function(dat, 
                    outcome = NULL, conditions = NULL,
                    sweep_list_X, sweep_range_Y,
                    dir.exp = NULL, include = "?",
                    incl.cut = 0.8, n.cut = 1, pri.cut = 0,
                    extract_mode = c("first", "all", "essential"),
                    return_details = TRUE,
                    Yvar = NULL, Xvars = NULL) {
  
  # === Backward compatibility for deprecated arguments ===
  if (!is.null(Yvar) && is.null(outcome)) {
    outcome <- Yvar
    warning("Argument 'Yvar' is deprecated. Use 'outcome' instead.",
            call. = FALSE)
  }
  if (!is.null(Xvars) && is.null(conditions)) {
    conditions <- Xvars
    warning("Argument 'Xvars' is deprecated. Use 'conditions' instead.",
            call. = FALSE)
  }
  
  # === Validate required arguments ===
  if (is.null(outcome)) {
    stop("Argument 'outcome' is required.")
  }
  if (is.null(conditions)) {
    stop("Argument 'conditions' is required.")
  }
  
  # === Handle negated outcome ===
  negate_outcome <- grepl("^~", outcome)
  outcome_clean <- sub("^~", "", outcome)
  
  # Validate outcome variable exists
  if (!outcome_clean %in% names(dat)) {
    stop("Variable '", outcome_clean, "' not found in data.")
  }
  
  # Validate condition variables exist
  missing_conds <- setdiff(conditions, names(dat))
  if (length(missing_conds) > 0) {
    stop("Condition variable(s) not found in data: ", 
         paste(missing_conds, collapse = ", "))
  }
  
  extract_mode <- match.arg(extract_mode)
  
  combo_X <- expand.grid(
    sweep_list_X,
    KEEP.OUT.ATTRS  = FALSE,
    stringsAsFactors = FALSE
  )
  
  # Initialize output data frame based on extract_mode
  df_out <- data.frame(
    combo_id    = integer(0),
    thrY        = numeric(0),
    thrX        = character(0),
    expression  = character(0),
    inclS       = numeric(0),
    covS        = numeric(0),
    n_solutions = integer(0),
    stringsAsFactors = FALSE
  )
  
  # Add columns based on extract_mode
  if (extract_mode == "essential") {
    df_out$selective_terms <- character(0)
    df_out$unique_terms     <- character(0)
  }
  
  details_list <- list()
  
  # Handle dir.exp: NULL or scalar -> expand to vector
  if (is.null(dir.exp) || length(dir.exp) == 1) {
    dir.exp <- rep(if (is.null(dir.exp)) 1 else dir.exp[1], length(conditions))
    names(dir.exp) <- conditions
  }
  
  # Track combinations with multiple solutions (for warning in "first" mode)
  multi_sol_combos <- c()
  
  combo_id <- 1L
  
  for (i in seq_len(nrow(combo_X))) {
    
    thrX_vec <- as.numeric(combo_X[i, ])
    names(thrX_vec) <- names(combo_X)
    thrX_label <- paste(names(thrX_vec), thrX_vec,
                        sep = "=", collapse = ", ")
    
    for (thrY in sweep_range_Y) {
      
      # Binarize outcome variable (use cleaned name)
      dat_bin <- data.frame(Y = qca_bin(dat[[outcome_clean]], thrY))
      for (x in conditions) {
        dat_bin[[x]] <- qca_bin(dat[[x]], thrX_vec[x])
      }
      
      # Determine outcome string for truthTable (with ~ if negated)
      outcome_tt <- if (negate_outcome) "~Y" else "Y"
      
      tt <- try(
        QCA::truthTable(
          dat_bin,
          outcome    = outcome_tt,
          conditions = conditions,
          show.cases = FALSE,
          incl.cut1  = incl.cut,
          n.cut      = n.cut,
          pri.cut    = pri.cut
        ),
        silent = TRUE
      )
      
      if (inherits(tt, "try-error")) {
        new_row <- data.frame(
          combo_id    = combo_id,
          thrY        = thrY,
          thrX        = thrX_label,
          expression  = "No solution",
          inclS       = NA_real_,
          covS        = NA_real_,
          n_solutions = 0L,
          stringsAsFactors = FALSE
        )
        
        if (extract_mode == "essential") {
          new_row$selective_terms <- NA_character_
          new_row$unique_terms     <- NA_character_
        }
        
        df_out <- rbind(df_out, new_row)
        
        if (return_details) {
          details_list[[length(details_list) + 1L]] <- list(
            combo_id    = combo_id,
            thrY        = thrY,
            thrX_vec    = thrX_vec,
            truth_table = NULL,
            solution    = NULL,
            dat_bin     = NULL
          )
        }
        
        next
      }
      
      sol <- try(
        QCA::minimize(
          tt,
          include    = include,
          dir.exp    = dir.exp,
          details    = TRUE,
          show.cases = FALSE,
          pri.cut    = pri.cut
        ),
        silent = TRUE
      )
      
      if (inherits(sol, "try-error")) {
        new_row <- data.frame(
          combo_id    = combo_id,
          thrY        = thrY,
          thrX        = thrX_label,
          expression  = "No solution",
          inclS       = NA_real_,
          covS        = NA_real_,
          n_solutions = 0L,
          stringsAsFactors = FALSE
        )
        
        if (extract_mode == "essential") {
          new_row$selective_terms <- NA_character_
          new_row$unique_terms     <- NA_character_
        }
        
        df_out <- rbind(df_out, new_row)
        
        if (return_details) {
          details_list[[length(details_list) + 1L]] <- list(
            combo_id    = combo_id,
            thrY        = thrY,
            thrX_vec    = thrX_vec,
            truth_table = tt,
            solution    = NULL,
            dat_bin     = dat_bin
          )
        }
        
        next
      }
      
      info <- qca_extract(sol, extract_mode = extract_mode)
      
      # Track multiple solutions
      if (info$n_solutions > 1) {
        multi_sol_combos <- c(multi_sol_combos, 
                              paste0("combo_id=", combo_id, ", thrY=", thrY))
      }
      
      # Build result row based on extract_mode
      new_row <- data.frame(
        combo_id    = combo_id,
        thrY        = thrY,
        thrX        = thrX_label,
        expression  = info$expression,
        inclS       = info$inclS,
        covS        = info$covS,
        n_solutions = info$n_solutions,
        stringsAsFactors = FALSE
      )
      
      if (extract_mode == "essential") {
        new_row$selective_terms <- info$selective_terms
        new_row$unique_terms     <- info$unique_terms
      }
      
      df_out <- rbind(df_out, new_row)
      
      if (return_details) {
        details_list[[length(details_list) + 1L]] <- list(
          combo_id    = combo_id,
          thrY        = thrY,
          thrX_vec    = thrX_vec,
          truth_table = tt,
          solution    = sol,
          dat_bin     = dat_bin
        )
      }
    }
    
    combo_id <- combo_id + 1L
  }
  
  # Issue warning for multiple solutions
  if (length(multi_sol_combos) > 0) {
    n_multi <- length(multi_sol_combos)
    warning(
      "Multiple intermediate solutions exist for ", n_multi, " combination(s) ",
      "(n_solutions > 1). ",
      "Only the first solution (M1) and its fit metrics are shown. ",
      "Use generate_report() for full analysis.",
      call. = FALSE
    )
  }
  
  if (return_details) {
    result <- list(
      summary = df_out, 
      details = details_list,
      params = list(
        outcome = outcome,
        conditions = conditions,
        negate_outcome = negate_outcome,
        sweep_list_X = sweep_list_X,
        sweep_range_Y = sweep_range_Y,
        incl.cut = incl.cut,
        n.cut = n.cut,
        pri.cut = pri.cut,
        include = include,
        dir.exp = dir.exp
      )
    )
    class(result) <- c("dtSweep_result", "tsqca_result", "list")
    return(result)
  }
  
  df_out
}
