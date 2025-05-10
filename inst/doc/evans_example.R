## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(multibias)
library(dplyr)

## ----eval = TRUE--------------------------------------------------------------
evans <- read.csv("evans.csv")

summary_stats <- evans %>%
  summarise(
    n = n(),
    mean_age = mean(AGE),
    sd_age = sd(AGE),
    prop_smokers = mean(SMK),
    prop_chd = mean(CHD),
    prop_hpt = mean(HPT)
  )

print(summary_stats)

## ----eval = TRUE--------------------------------------------------------------
df_obs <- data_observed(
  data = evans,
  bias = "uc",
  exposure = "SMK",
  outcome = "CHD",
  confounders = "HPT"
)

print(df_obs)
summary(df_obs)

## -----------------------------------------------------------------------------
cor(evans$SMK, evans$AGE)
cor(evans$CHD, evans$AGE)

## ----eval = TRUE--------------------------------------------------------------
u_0 <- qlogis(0.25)
u_x <- log(0.5)
u_y <- log(2.5)
u_c <- log(2)

u_coefs <- list(u = c(u_0, u_x, u_y, u_c))

## ----eval = TRUE--------------------------------------------------------------
set.seed(1234)
multibias_adjust(
  data_observed = df_obs,
  bias_params = bias_params(coef_list = u_coefs),
  bootstrap = TRUE,
  bootstrap_reps = 100
)

## ----eval = TRUE--------------------------------------------------------------
full_model <- glm(CHD ~ SMK + HPT + AGE,
  family = binomial(link = "logit"),
  data = evans
)
or <- round(exp(coef(full_model)[2]), 2)
or_ci_low <- round(
  exp(coef(full_model)[2] - 1.96 * summary(full_model)$coef[2, 2]), 2
)
or_ci_high <- round(
  exp(coef(full_model)[2] + 1.96 * summary(full_model)$coef[2, 2]), 2
)

print(paste0("Odds Ratio: ", or))
print(paste0("95% CI: (", or_ci_low, ", ", or_ci_high, ")"))

