## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(TSQCA)
data("sample_data")
dat <- sample_data
str(dat)

Yvar  <- "Y"
Xvars <- c("X1", "X2", "X3")

## ----eval=FALSE---------------------------------------------------------------
# # 正しい方法：各変数の閾値を明示的に指定
# sweep_list <- list(
#   X1 = 1,      # 2値変数: 閾値1を使用
#   X2 = 6:8,    # 連続変数: 閾値をスイープ
#   X3 = 6:8     # 連続変数: 閾値をスイープ
# )
# 
# res_mixed <- ctSweepM(
#   dat            = dat,
#   Yvar           = "Y",
#   Xvars          = c("X1", "X2", "X3"),
#   sweep_list     = sweep_list,
#   thrY           = 7,
#   dir.exp        = 1
# )

## ----eval=FALSE---------------------------------------------------------------
# # 間違い：2値変数に対してスイープ範囲を指定
# sweep_list <- list(
#   X1 = 6:8,    # すべての値が0になる（0 < 6 かつ 1 < 6 のため）
#   X2 = 6:8,
#   X3 = 6:8
# )

## ----eval=FALSE---------------------------------------------------------------
# # 変数の範囲を確認
# summary(dat[, c("X1", "X2", "X3")])
# 
# # 2値変数を識別（0と1のみ）
# sapply(dat[, c("X1", "X2", "X3")], function(x) {
#   unique_vals <- sort(unique(x))
#   if (length(unique_vals) == 2 && all(unique_vals == c(0, 1))) {
#     "2値変数（閾値 = 1 を使用）"
#   } else {
#     paste("連続変数（範囲:", min(x), "-", max(x), ")")
#   }
# })

## ----error=TRUE---------------------------------------------------------------
try({
sweep_var   <- "X3"   # 閾値を変化させる対象の条件（X）
sweep_range <- 6:9    # 試す閾値候補

thrY         <- 7     # Y の閾値（固定）
thrX_default <- 7     # その他 X の閾値（固定）

res_cts <- ctSweepS(
  dat            = dat,
  Yvar           = "Y",
  Xvars          = c("X1", "X2", "X3"),
  sweep_var      = sweep_var,
  sweep_range    = sweep_range,
  thrY           = thrY,
  thrX_default   = thrX_default,
  dir.exp        = 1,
  return_details = FALSE
)

head(res_cts)
})

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

## -----------------------------------------------------------------------------
res_ots <- otSweep(
  dat            = dat,
  Yvar           = "Y",
  Xvars          = c("X1", "X2", "X3"),
  sweep_range    = 6:9,
  thrX           = c(X1 = 7, X2 = 7, X3 = 7),
  dir.exp        = 1,
  return_details = FALSE
)

head(res_ots)

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

head(res_dts)

## -----------------------------------------------------------------------------
sessionInfo()

