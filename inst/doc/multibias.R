## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(multibias)

## -----------------------------------------------------------------------------
df_observed <- data_observed(
  df_uc_sel,
  bias = c("uc", "sel"),
  exposure = "X",
  outcome = "Y",
  confounders = c("C1", "C2", "C3")
)

## -----------------------------------------------------------------------------
bp <- bias_params(
  coef_list = list(
    u = c(-0.19, 0.61, 0.72, -0.09, 0.10, -0.15),
    s = c(-0.01, 0.92, 0.94)
  )
)

## -----------------------------------------------------------------------------
df_validation <- data_validation(
  df_uc_sel_source,
  true_exposure = "X",
  true_outcome = "Y",
  confounders = c("C1", "C2", "C3", "U"),
  selection = "S"
)

## -----------------------------------------------------------------------------
multibias_adjust(
  data_observed = df_observed,
  bias_params = bp
)

## -----------------------------------------------------------------------------
multibias_adjust(
  data_observed = df_observed,
  data_validation = df_validation
)

## -----------------------------------------------------------------------------
multibias_adjust(
  data_observed = df_observed,
  data_validation = df_validation,
  bootstrap = TRUE,
  bootstrap_reps = 10
)

