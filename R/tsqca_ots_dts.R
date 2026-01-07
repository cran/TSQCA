###############################################
# OTS–QCA (Y sweep) and DTS–QCA (2D sweep)
###############################################

#' OTS–QCA: Outcome threshold sweep
#'
#' Sweeps the threshold of the outcome Y while keeping the thresholds of
#' all X conditions fixed.
#'
#' @param dat Data frame containing the outcome and condition variables.
#' @param Yvar Character. Outcome variable name.
#' @param Xvars Character vector. Names of condition variables.
#' @param sweep_range Numeric vector. Candidate thresholds for Y.
#' @param thrX Named numeric vector. Fixed thresholds for X variables,
#'   with names matching \code{Xvars}.
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
#'   \item \code{thrY} — threshold for Y
#'   \item \code{expression} — minimized solution expression
#'   \item \code{inclS} — solution consistency
#'   \item \code{covS} — solution coverage
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
#' # Run outcome threshold sweep
#' result <- otSweep(
#'   dat = sample_data,
#'   Yvar = "Y",
#'   Xvars = c("X1", "X2", "X3"),
#'   sweep_range = 6:9,
#'   thrX = thrX
#' )
#' head(result)
otSweep <- function(dat, Yvar, Xvars,
                    sweep_range, thrX,
                    dir.exp = NULL, include = "?",
                    incl.cut = 0.8, n.cut = 2, pri.cut = 0.5,
                    return_details = FALSE) {
  
  df_out <- data.frame(
    thrY       = numeric(0),
    expression = character(0),
    inclS      = numeric(0),
    covS       = numeric(0),
    stringsAsFactors = FALSE
  )
  
  details_list <- list()
  
  for (thrY in sweep_range) {
    
    dat_bin <- data.frame(Y = qca_bin(dat[[Yvar]], thrY))
    for (x in Xvars) {
      dat_bin[[x]] <- qca_bin(dat[[x]], thrX[x])
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
      df_out <- rbind(df_out, data.frame(
        thrY       = thrY,
        expression = "No solution",
        inclS      = NA_real_,
        covS       = NA_real_,
        stringsAsFactors = FALSE
      ))
      
      if (return_details) {
        details_list[[as.character(thrY)]] <- list(
          thrY        = thrY,
          thrX_vec    = thrX,
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
      df_out <- rbind(df_out, data.frame(
        thrY       = thrY,
        expression = "No solution",
        inclS      = NA_real_,
        covS       = NA_real_,
        stringsAsFactors = FALSE
      ))
      
      if (return_details) {
        details_list[[as.character(thrY)]] <- list(
          thrY        = thrY,
          thrX_vec    = thrX,
          truth_table = tt,
          solution    = NULL
        )
      }
      next
    }
    
    info <- qca_extract(sol)
    
    df_out <- rbind(df_out, data.frame(
      thrY       = thrY,
      expression = info$expression,
      inclS      = info$inclS,
      covS       = info$covS,
      stringsAsFactors = FALSE
    ))
    
    if (return_details) {
      details_list[[as.character(thrY)]] <- list(
        thrY        = thrY,
        thrX_vec    = thrX,
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
#' @param Yvar Character. Outcome variable name.
#' @param Xvars Character vector. Names of condition variables.
#' @param sweep_list_X Named list. Each element is a numeric vector of
#'   candidate thresholds for the corresponding X.
#' @param sweep_range_Y Numeric vector. Candidate thresholds for Y.
#' @param dir.exp Directional expectations for \code{minimize}.
#'   If \code{NULL}, all set to 1.
#' @param include Inclusion rule for \code{minimize}.
#' @param incl.cut Consistency cutoff for \code{truthTable}.
#' @param n.cut Frequency cutoff for \code{truthTable}.
#' @param pri.cut PRI cutoff for \code{minimize}.
#' @param return_details Logical. If \code{TRUE}, returns both summary and
#'   detailed objects.
#'
#' @return
#' If \code{return_details = FALSE}, a data frame with columns:
#' \itemize{
#'   \item \code{combo_id} — index of the X-threshold combination
#'   \item \code{thrY} — threshold for Y
#'   \item \code{thrX} — character label summarizing the X thresholds
#'   \item \code{expression} — minimized solution expression
#'   \item \code{inclS} — solution consistency
#'   \item \code{covS} — solution coverage
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
#'   Yvar = "Y",
#'   Xvars = c("X1", "X2"),  # Reduced from 3 to 2 conditions
#'   sweep_list_X = sweep_list_X,
#'   sweep_range_Y = sweep_range_Y
#' )
#' head(result_quick)
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
#'   Yvar = "Y",
#'   Xvars = c("X1", "X2", "X3"),
#'   sweep_list_X = sweep_list_X_full,
#'   sweep_range_Y = sweep_range_Y_full
#' )
#' 
#' # Analyze threshold-dependent causal structures
#' head(result_full)
#' }
#' @export
dtSweep <- function(dat, Yvar, Xvars,
                    sweep_list_X, sweep_range_Y,
                    dir.exp = NULL, include = "?",
                    incl.cut = 0.8, n.cut = 2, pri.cut = 0.5,
                    return_details = FALSE) {
  
  combo_X <- expand.grid(
    sweep_list_X,
    KEEP.OUT.ATTRS  = FALSE,
    stringsAsFactors = FALSE
  )
  
  df_out <- data.frame(
    combo_id   = integer(0),
    thrY       = numeric(0),
    thrX       = character(0),
    expression = character(0),
    inclS      = numeric(0),
    covS       = numeric(0),
    stringsAsFactors = FALSE
  )
  
  details_list <- list()
  
  if (is.null(dir.exp)) {
    dir.exp <- rep(1, length(Xvars))
    names(dir.exp) <- Xvars
  }
  
  combo_id <- 1L
  
  for (i in seq_len(nrow(combo_X))) {
    
    thrX_vec <- as.numeric(combo_X[i, ])
    names(thrX_vec) <- names(combo_X)
    thrX_label <- paste(names(thrX_vec), thrX_vec,
                        sep = "=", collapse = ", ")
    
    for (thrY in sweep_range_Y) {
      
      dat_bin <- data.frame(Y = qca_bin(dat[[Yvar]], thrY))
      for (x in Xvars) {
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
        df_out <- rbind(df_out, data.frame(
          combo_id   = combo_id,
          thrY       = thrY,
          thrX       = thrX_label,
          expression = "No solution",
          inclS      = NA_real_,
          covS       = NA_real_,
          stringsAsFactors = FALSE
        ))
        
        if (return_details) {
          details_list[[length(details_list) + 1L]] <- list(
            combo_id    = combo_id,
            thrY        = thrY,
            thrX_vec    = thrX_vec,
            truth_table = NULL,
            solution    = NULL
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
        df_out <- rbind(df_out, data.frame(
          combo_id   = combo_id,
          thrY       = thrY,
          thrX       = thrX_label,
          expression = "No solution",
          inclS      = NA_real_,
          covS       = NA_real_,
          stringsAsFactors = FALSE
        ))
        
        if (return_details) {
          details_list[[length(details_list) + 1L]] <- list(
            combo_id    = combo_id,
            thrY        = thrY,
            thrX_vec    = thrX_vec,
            truth_table = tt,
            solution    = NULL
          )
        }
        
        next
      }
      
      info <- qca_extract(sol)
      
      df_out <- rbind(df_out, data.frame(
        combo_id   = combo_id,
        thrY       = thrY,
        thrX       = thrX_label,
        expression = info$expression,
        inclS      = info$inclS,
        covS       = info$covS,
        stringsAsFactors = FALSE
      ))
      
      if (return_details) {
        details_list[[length(details_list) + 1L]] <- list(
          combo_id    = combo_id,
          thrY        = thrY,
          thrX_vec    = thrX_vec,
          truth_table = tt,
          solution    = sol
        )
      }
    }
    
    combo_id <- combo_id + 1L
  }
  
  if (return_details) {
    return(list(summary = df_out, details = details_list))
  }
  
  df_out
}

