## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(multibias)

## ----out.width = '70%', echo = FALSE------------------------------------------
knitr::include_graphics("img/uc_emc_sel_DAG.png")

## ----eval = TRUE--------------------------------------------------------------
head(df_uc_em_sel)

## ----eval = TRUE--------------------------------------------------------------
df_observed <- data_observed(
  data = df_uc_em_sel,
  bias = c("uc", "em", "sel"),
  exposure = "Xstar",
  outcome = "Y",
  confounders = c("C1", "C2", "C3")
)

print(df_observed)
summary(df_observed)

## ----eval = TRUE--------------------------------------------------------------
u_model <- glm(U ~ X + Y,
  family = binomial(link = "logit"),
  data = df_uc_em_sel_source
)
x_model <- glm(X ~ Xstar + Y + C1 + C2 + C3,
  family = binomial(link = "logit"),
  data = df_uc_em_sel_source
)
s_model <- glm(S ~ Xstar + Y + C1 + C2 + C3,
  family = binomial(link = "logit"),
  data = df_uc_em_sel_source
)

## -----------------------------------------------------------------------------
bp <- bias_params(
  coef_list = list(
    u = c(
      rnorm(1, mean = u_model$coef[1], sd = summary(u_model)$coef[1, 2]),
      rnorm(1, mean = u_model$coef[2], sd = summary(u_model)$coef[2, 2]),
      rnorm(1, mean = u_model$coef[3], sd = summary(u_model)$coef[3, 2])
    ),
    x = c(
      rnorm(1, mean = x_model$coef[1], sd = summary(x_model)$coef[1, 2]),
      rnorm(1, mean = x_model$coef[2], sd = summary(x_model)$coef[2, 2]),
      rnorm(1, mean = x_model$coef[3], sd = summary(x_model)$coef[3, 2]),
      rnorm(1, mean = x_model$coef[4], sd = summary(x_model)$coef[4, 2]),
      rnorm(1, mean = x_model$coef[5], sd = summary(x_model)$coef[5, 2]),
      rnorm(1, mean = x_model$coef[6], sd = summary(x_model)$coef[6, 2])
    ),
    s = c(
      rnorm(1, mean = s_model$coef[1], sd = summary(s_model)$coef[1, 2]),
      rnorm(1, mean = s_model$coef[2], sd = summary(s_model)$coef[2, 2]),
      rnorm(1, mean = s_model$coef[3], sd = summary(s_model)$coef[3, 2]),
      rnorm(1, mean = s_model$coef[4], sd = summary(s_model)$coef[4, 2]),
      rnorm(1, mean = s_model$coef[5], sd = summary(s_model)$coef[5, 2]),
      rnorm(1, mean = s_model$coef[6], sd = summary(s_model)$coef[6, 2])
    )
  )
)

## -----------------------------------------------------------------------------
multibias_adjust(
  data_observed = df_observed,
  bias_params = bp,
  bootstrap = TRUE,
  bootstrap_reps = 10,
  level = 0.95
)

