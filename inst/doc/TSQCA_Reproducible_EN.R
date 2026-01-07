## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(TSQCA)
library(QCA)

## -----------------------------------------------------------------------------
# Adjust the file name as needed
library(TSQCA)
data("sample_data")
dat <- sample_data

# Outcome and conditions
Yvar  <- "Y"
Xvars <- c("X1", "X2", "X3")

# Quick inspection
str(dat)
summary(dat)

## -----------------------------------------------------------------------------
thrY_base <- 7
thrX_base <- 7

# Fixed X thresholds (for OTS–QCA)
thrX_vec <- c(
  X1 = thrX_base,
  X2 = thrX_base,
  X3 = thrX_base
)
thrX_vec

## ----error=TRUE---------------------------------------------------------------
try({
sweep_var   <- "X3"   # Condition (X) whose threshold is swept
sweep_range <- 6:9    # Candidate threshold values to evaluate
thrY         <- 7     # Outcome (Y) threshold (fixed)
thrX_default <- 7     # Threshold for other X conditions (fixed)

res_cts <- ctSweepS(
  dat            = dat,
  Yvar           = "Y",
  Xvars          = c("X1", "X2", "X3"),
  sweep_var      = "X3",
  sweep_range    = 6:9,
  thrY           = 7,
  thrX_default   = 7,
  dir.exp        = 1,
  return_details = FALSE
)

head(res_cts)
})

## ----eval=FALSE---------------------------------------------------------------
# write.csv(res_cts, file = "TSQCA_CTS_results.csv", row.names = FALSE)

## ----error=TRUE---------------------------------------------------------------
try({
res_mcts <- ctSweepM(
  dat            = dat,
  Yvar           = "Y",
  Xvars          = c("X1", "X2", "X3"),
  sweep_vars     = c("X2", "X3"),
  sweep_range    = 6:9,
  thrY           = 7,
  thrX_default   = 7,
  dir.exp        = 1,
  return_details = FALSE
)

head(res_mcts)
})

## ----eval=FALSE---------------------------------------------------------------
# write.csv(res_mcts, file = "TSQCA_MCTS_results.csv", row.names = FALSE)

## -----------------------------------------------------------------------------
sweep_range_ots <- 6:9

res_ots <- otSweep(
  dat            = dat,
  Yvar           = "Y",
  Xvars          = c("X1", "X2", "X3"),
  sweep_range    = sweep_range_ots,
  thrX           = thrX_vec,
  dir.exp        = 1,
  return_details = FALSE
)

res_ots


## ----eval=FALSE---------------------------------------------------------------
# write.csv(res_ots, file = "TSQCA_OTS_results.csv", row.names = FALSE)

## -----------------------------------------------------------------------------
sweep_list_dts_X <- list(
  X1 = 6:8,
  X2 = 6:8,
  X3 = 6:8
)

sweep_range_dts_Y <- 6:8

res_dts <- dtSweep(
  dat            = dat,
  Yvar           = "Y",
  Xvars          = c("X1", "X2", "X3"),
  sweep_list_X   = sweep_list_dts_X,
  sweep_range_Y  = sweep_range_dts_Y,
  dir.exp        = 1,
  return_details = FALSE
)

res_dts


## ----eval=FALSE---------------------------------------------------------------
# write.csv(res_dts, file = "TSQCA_DTS_results.csv", row.names = FALSE)

## -----------------------------------------------------------------------------
sessionInfo()

