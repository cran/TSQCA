###############################################
# CTS–QCA (single X) and MCTS–QCA (multiple X)
###############################################

#' CTS–QCA: Single-condition threshold sweep
#'
#' Performs a threshold sweep for one focal condition X. For each threshold
#' in \code{sweep_range}, the outcome Y and all X variables are binarized
#' using user-specified thresholds, and a crisp-set QCA is executed.
#'
#' @param dat Data frame containing the outcome and condition variables.
#' @param Yvar Character. Outcome variable name.
#' @param Xvars Character vector. Names of condition variables.
#' @param sweep_var Character. Name of the condition to be swept.
#'   Must be one of \code{Xvars}.
#' @param sweep_range Numeric vector. Candidate thresholds for \code{sweep_var}.
#' @param thrY Numeric. Threshold for Y (fixed).
#' @param thrX_default Numeric. Default threshold for non-swept X variables.
#' @param dir.exp Optional named numeric vector of directional expectations
#'   for \code{\link[QCA]{minimize}}. If \code{NULL}, all set to 1.
#' @param include Inclusion rule for \code{minimize} (e.g., \code{"?"}).
#' @param incl.cut Consistency cutoff for \code{\link[QCA]{truthTable}}.
#' @param n.cut Frequency cutoff for \code{truthTable}.
#' @param pri.cut PRI cutoff for \code{minimize}.
#' @param return_details Logical. If \code{TRUE}, returns both a summary
#'   data frame and detailed objects for each threshold.
#'
#' @return
#' If \code{return_details = FALSE}, a data frame with columns:
#' \itemize{
#'   \item \code{threshold} — swept threshold for \code{sweep_var}
#'   \item \code{expression} — minimized solution expression
#'   \item \code{inclS} — solution consistency
#'   \item \code{covS} — solution coverage
#' }
#'
#' If \code{return_details = TRUE}, a list with:
#' \itemize{
#'   \item \code{summary} — the data frame above
#'   \item \code{details} — per-threshold list of
#'     \code{threshold}, \code{thrX_vec}, \code{truth_table}, \code{solution}
#' }
#'
#' @importFrom QCA truthTable minimize
#' @export
#' @examples
#' # Load sample data
#' data(sample_data)
#' 
#' # Run single condition threshold sweep on X3
#' result <- ctSweepS(
#'   dat = sample_data,
#'   Yvar = "Y",
#'   Xvars = c("X1", "X2", "X3"),
#'   sweep_var = "X3",
#'   sweep_range = 6:8,
#'   thrY = 7,
#'   thrX_default = 7
#' )
#' head(result)
ctSweepS <- function(dat, Yvar, Xvars,
                     sweep_var, sweep_range,
                     thrY, thrX_default = 7,
                     dir.exp = NULL, include = "?",
                     incl.cut = 0.8, n.cut = 2, pri.cut = 0.5,
                     return_details = FALSE) {
  
  if (!sweep_var %in% Xvars) {
    stop("sweep_var must be one of Xvars.")
  }
  
  df_out <- data.frame(
    threshold  = numeric(0),
    expression = character(0),
    inclS      = numeric(0),
    covS       = numeric(0),
    stringsAsFactors = FALSE
  )
  
  details_list <- list()
  
  for (thr in sweep_range) {
    
    # vector of thresholds for all X
    thrX_vec <- rep(thrX_default, length(Xvars))
    names(thrX_vec) <- Xvars
    thrX_vec[sweep_var] <- thr
    
    # binarize Y and X
    dat_bin <- data.frame(Y = qca_bin(dat[[Yvar]], thrY))
    for (x in Xvars) {
      dat_bin[[x]] <- qca_bin(dat[[x]], thrX_vec[x])
    }
    
    # Truth table (wrapped in try to handle errors)
    tt <- try(
      QCA::truthTable(
        dat_bin,
        outcome    = Yvar,
        conditions = Xvars,
        show.cases = FALSE,
        incl.cut   = incl.cut,
        n.cut      = n.cut,
        pri.cut    = pri.cut
      ),
      silent = TRUE
    )
    
    if (inherits(tt, "try-error")) {
      df_out <- rbind(df_out, data.frame(
        threshold  = thr,
        expression = "No solution",
        inclS      = NA_real_,
        covS       = NA_real_,
        stringsAsFactors = FALSE
      ))
      
      if (return_details) {
        details_list[[as.character(thr)]] <- list(
          threshold   = thr,
          thrX_vec    = thrX_vec,
          truth_table = NULL,
          solution    = NULL
        )
      }
      next
    }
    
    # Directional expectations (local copy to avoid modifying argument)
    local_dir.exp <- dir.exp
    if (is.null(local_dir.exp)) {
      local_dir.exp <- rep(1, length(Xvars))
      names(local_dir.exp) <- Xvars
    }
    
    # Minimize (wrapped in try to handle errors)
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
      df_out <- rbind(df_out, data.frame(
        threshold  = thr,
        expression = "No solution",
        inclS      = NA_real_,
        covS       = NA_real_,
        stringsAsFactors = FALSE
      ))
      
      if (return_details) {
        details_list[[as.character(thr)]] <- list(
          threshold   = thr,
          thrX_vec    = thrX_vec,
          truth_table = tt,
          solution    = NULL
        )
      }
      next
    }
    
    sol_info <- qca_extract(sol)
    
    df_out <- rbind(df_out, data.frame(
      threshold  = thr,
      expression = sol_info$expression,
      inclS      = sol_info$inclS,
      covS       = sol_info$covS,
      stringsAsFactors = FALSE
    ))
    
    if (return_details) {
      details_list[[as.character(thr)]] <- list(
        threshold   = thr,
        thrX_vec    = thrX_vec,
        truth_table = tt,
        solution    = sol
      )
    }
  }
  
  df_out <- df_out[order(df_out$threshold), ]
  
  if (return_details) {
    return(list(summary = df_out, details = details_list))
  }
  
  df_out
}


###############################################
# MCTS–QCA (multiple X)
###############################################

#' MCTS–QCA: Multi-condition threshold sweep
#'
#' Performs a grid search over thresholds of multiple X variables.
#' For each combination of thresholds in \code{sweep_list}, the outcome Y
#' and all X variables are binarized, and a crisp-set QCA is executed.
#'
#' @param dat Data frame containing the outcome and condition variables.
#' @param Yvar Character. Outcome variable name.
#' @param Xvars Character vector. Names of condition variables.
#' @param sweep_list Named list. Each element is a numeric vector of
#'   candidate thresholds for the corresponding X. Names must match
#'   \code{Xvars}.
#' @param thrY Numeric. Threshold for Y (fixed).
#' @param dir.exp Directional expectations for \code{minimize}.
#'   If \code{NULL}, all set to 1.
#' @param include Inclusion rule for \code{minimize}.
#' @param incl.cut Consistency cutoff for \code{truthTable}.
#' @param n.cut Frequency cutoff for \code{truthTable}.
#' @param pri.cut PRI cutoff for \code{minimize}.
#' @param return_details Logical. If \code{TRUE}, returns both summary
#'   and detailed objects.
#'
#' @return
#' If \code{return_details = FALSE}, a data frame with columns:
#' \itemize{
#'   \item \code{combo_id} — index of the threshold combination
#'   \item \code{threshold} — character string summarizing thresholds,
#'     e.g. \code{"X1=6, X2=7, X3=7"}
#'   \item \code{expression} — minimized solution expression
#'   \item \code{inclS} — solution consistency
#'   \item \code{covS} — solution coverage
#' }
#'
#' If \code{return_details = TRUE}, a list with:
#' \itemize{
#'   \item \code{summary} — the data frame above
#'   \item \code{details} — per-combination list of
#'     \code{combo_id}, \code{thrX_vec}, \code{truth_table}, \code{solution}
#' }
#'
#' @importFrom QCA truthTable minimize
#' @export
#' @examples
#' # Load sample data
#' data(sample_data)
#' 
#' # Quick demonstration with 2 conditions (< 5 seconds)
#' # This explores 2^2 = 4 threshold combinations
#' sweep_list <- list(
#'   X1 = 6:7,  # Reduced from 6:8 to 6:7
#'   X2 = 6:7   # Reduced from 6:8 to 6:7
#' )
#' 
#' # Run multiple condition threshold sweep with reduced parameters
#' result_quick <- ctSweepM(
#'   dat = sample_data,
#'   Yvar = "Y",
#'   Xvars = c("X1", "X2"),  # Reduced from 3 to 2 conditions
#'   sweep_list = sweep_list,
#'   thrY = 7
#' )
#' head(result_quick)
#' 
#' \donttest{
#' # Full multi-condition analysis with 3 conditions
#' # This explores 3^3 = 27 threshold combinations (takes ~5-8 seconds)
#' sweep_list_full <- list(
#'   X1 = 6:8,
#'   X2 = 6:8,
#'   X3 = 6:8
#' )
#' 
#' result_full <- ctSweepM(
#'   dat = sample_data,
#'   Yvar = "Y",
#'   Xvars = c("X1", "X2", "X3"),
#'   sweep_list = sweep_list_full,
#'   thrY = 7
#' )
#' 
#' # Visualize threshold-dependent solution paths
#' head(result_full)
#' }
ctSweepM <- function(dat, Yvar, Xvars,
                     sweep_list, thrY,
                     dir.exp = NULL, include = "?",
                     incl.cut = 0.8, n.cut = 2, pri.cut = 0.5,
                     return_details = FALSE) {
  
  combo_mat <- expand.grid(
    sweep_list,
    KEEP.OUT.ATTRS  = FALSE,
    stringsAsFactors = FALSE
  )
  
  df_out <- data.frame(
    combo_id   = seq_len(nrow(combo_mat)),
    threshold  = NA_character_,
    expression = NA_character_,
    inclS      = NA_real_,
    covS       = NA_real_,
    stringsAsFactors = FALSE
  )
  
  details_list <- list()
  
  for (i in seq_len(nrow(combo_mat))) {
    
    thrX_vec <- as.numeric(combo_mat[i, ])
    names(thrX_vec) <- names(combo_mat)
    
    dat_bin <- data.frame(Y = qca_bin(dat[[Yvar]], thrY))
    for (x in names(thrX_vec)) {
      dat_bin[[x]] <- qca_bin(dat[[x]], thrX_vec[x])
    }
    
    tt <- try(
      QCA::truthTable(
        dat_bin,
        outcome    = Yvar,
        conditions = Xvars,
        show.cases = FALSE,
        incl.cut   = incl.cut,
        n.cut      = n.cut,
        pri.cut    = pri.cut
      ),
      silent = TRUE
    )
    
    if (inherits(tt, "try-error")) {
      df_out$threshold[i]  <- paste(names(thrX_vec), thrX_vec,
                                    sep = "=", collapse = ", ")
      df_out$expression[i] <- "No solution"
      df_out$inclS[i]      <- NA_real_
      df_out$covS[i]       <- NA_real_
      
      if (return_details) {
        details_list[[i]] <- list(
          combo_id    = i,
          thrX_vec    = thrX_vec,
          truth_table = NULL,
          solution    = NULL
        )
      }
      next
    }
    
    local_dir.exp <- dir.exp
    if (is.null(local_dir.exp)) {
      local_dir.exp <- rep(1, length(Xvars))
      names(local_dir.exp) <- Xvars
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
      df_out$threshold[i]  <- paste(names(thrX_vec), thrX_vec,
                                    sep = "=", collapse = ", ")
      df_out$expression[i] <- "No solution"
      df_out$inclS[i]      <- NA_real_
      df_out$covS[i]       <- NA_real_
      
      if (return_details) {
        details_list[[i]] <- list(
          combo_id    = i,
          thrX_vec    = thrX_vec,
          truth_table = tt,
          solution    = NULL
        )
      }
      next
    }
    
    sol_info <- qca_extract(sol)
    
    df_out$threshold[i]  <- paste(names(thrX_vec), thrX_vec,
                                  sep = "=", collapse = ", ")
    df_out$expression[i] <- sol_info$expression
    df_out$inclS[i]      <- sol_info$inclS
    df_out$covS[i]       <- sol_info$covS
    
    if (return_details) {
      details_list[[i]] <- list(
        combo_id    = i,
        thrX_vec    = thrX_vec,
        truth_table = tt,
        solution    = sol
      )
    }
  }
  
  if (return_details) {
    return(list(summary = df_out, details = details_list))
  }
  
  df_out
}
