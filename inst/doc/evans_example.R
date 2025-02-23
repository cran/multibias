## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(multibias)

## ----eval = TRUE--------------------------------------------------------------
head(evans)

## ----eval = TRUE--------------------------------------------------------------
biased_model <- glm(CHD ~ SMK + HPT,
  family = binomial(link = "logit"),
  data = evans
)
or <- round(exp(coef(biased_model)[2]), 2)
or_ci_low <- round(
  exp(coef(biased_model)[2] - 1.96 * summary(biased_model)$coef[2, 2]), 2
)
or_ci_high <- round(
  exp(coef(biased_model)[2] + 1.96 * summary(biased_model)$coef[2, 2]), 2
)

print(paste0("Biased Odds Ratio: ", or))
print(paste0("95% CI: (", or_ci_low, ", ", or_ci_high, ")"))

## -----------------------------------------------------------------------------
cor(evans$SMK, evans$AGE)
cor(evans$CHD, evans$AGE)

## ----eval = TRUE--------------------------------------------------------------
u_0 <- qlogis(0.25)
u_x <- log(0.5)
u_y <- log(2.5)
u_c <- log(2)

## ----eval = TRUE--------------------------------------------------------------
df_obs <- data_observed(
  data = evans,
  exposure = "SMK",
  outcome = "CHD",
  confounders = "HPT"
)

set.seed(1234)
adjust_uc(
  df_obs,
  u_model_coefs = c(u_0, u_x, u_y, u_c)
)

## ----eval = TRUE--------------------------------------------------------------
full_model <- glm(CHD ~ SMK + HPT + AGE,
  family = binomial(link = "logit"),
  data = evans
)
or <- round(exp(coef(full_model)[2]), 2)
or_ci_low <- round(
  exp(coef(biased_model)[2] - 1.96 * summary(full_model)$coef[2, 2]), 2
)
or_ci_high <- round(
  exp(coef(biased_model)[2] + 1.96 * summary(full_model)$coef[2, 2]), 2
)

print(paste0("Odds Ratio: ", or))
print(paste0("95% CI: (", or_ci_low, ", ", or_ci_high, ")"))

