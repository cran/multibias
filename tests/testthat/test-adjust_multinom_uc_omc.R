# library(nnet)

set.seed(1234)
n <- 10000
nreps <- 10

# 0 confounders

nobias_model <- glm(Y ~ X + U,
                    family = binomial(link = "logit"),
                    data = df_uc_omc_source)

# uy_model <- multinom(
#   paste0(U, Y) ~ X + Ystar,
#   data = df_uc_omc_source
# )
# summary(uy_model, digits = 2)

single_run <- adjust_multinom_uc_omc(
  df_uc_omc,
  exposure = "X",
  outcome = "Ystar",
  u1y0_model_coefs = c(-0.32, 0.59, 0.01),
  u0y1_model_coefs = c(-2.98, 0.71, 1.65),
  u1y1_model_coefs = c(-2.59, 1.27, 1.64)
)

est <- vector()
for (i in 1:nreps) {
  bdf <- df_uc_omc[sample(seq_len(n), n, replace = TRUE), ]
  results <- adjust_multinom_uc_omc(
    bdf,
    exposure = "X",
    outcome = "Ystar",
    u1y0_model_coefs = c(-0.32, 0.59, 0.01),
    u0y1_model_coefs = c(-2.98, 0.71, 1.65),
    u1y1_model_coefs = c(-2.59, 1.27, 1.64)
  )
  est[i] <- results$estimate
}

or_true <- exp(summary(nobias_model)$coef[2, 1])
or_adjusted <- median(est)

test_that("odds ratio and confidence interval output", {
  expect_gt(or_adjusted, or_true - 0.1)
  expect_lt(or_adjusted, or_true + 0.1)
  expect_vector(
    single_run$ci,
    ptype = double(),
    size = 2
  )
})

# 1 confounder

nobias_model <- glm(Y ~ X + C1 + U,
                    family = binomial(link = "logit"),
                    data = df_uc_omc_source)

# uy_model <- multinom(
#   paste0(U, Y) ~ X + Ystar + C1,
#   data = df_uc_omc_source
# )
# summary(uy_model, digits = 2)

single_run <- adjust_multinom_uc_omc(
  df_uc_omc,
  exposure = "X",
  outcome = "Ystar",
  confounders = "C1",
  u1y0_model_coefs = c(-0.19, 0.61, 0.00, -0.07),
  u0y1_model_coefs = c(-3.21, 0.60, 1.60, 0.36),
  u1y1_model_coefs = c(-2.72, 1.24, 1.59, 0.34)
)

est <- vector()
for (i in 1:nreps) {
  bdf <- df_uc_omc[sample(seq_len(n), n, replace = TRUE), ]
  results <- adjust_multinom_uc_omc(
    bdf,
    exposure = "X",
    outcome = "Ystar",
    confounders = "C1",
    u1y0_model_coefs = c(-0.19, 0.61, 0.00, -0.07),
    u0y1_model_coefs = c(-3.21, 0.60, 1.60, 0.36),
    u1y1_model_coefs = c(-2.72, 1.24, 1.59, 0.34)
  )
  est[i] <- results$estimate
}

or_true <- exp(summary(nobias_model)$coef[2, 1])
or_adjusted <- median(est)

test_that("odds ratio and confidence interval output", {
  expect_gt(or_adjusted, or_true - 0.1)
  expect_lt(or_adjusted, or_true + 0.1)
  expect_vector(
    single_run$ci,
    ptype = double(),
    size = 2
  )
})

# 2 confounders

nobias_model <- glm(Y ~ X + C1 + C2 + U,
                    family = binomial(link = "logit"),
                    data = df_uc_omc_source)

# uy_model <- multinom(
#   paste0(U, Y) ~ X + Ystar + C1 + C2,
#   data = df_uc_omc_source
# )
# summary(uy_model, digits = 2)

single_run <- adjust_multinom_uc_omc(
  df_uc_omc,
  exposure = "X",
  outcome = "Ystar",
  confounders = c("C1", "C2"),
  u1y0_model_coefs = c(-0.31, 0.60, 0.01, -0.08, 0.10),
  u0y1_model_coefs = c(-3.07, 0.66, 1.65, 0.42, -0.85),
  u1y1_model_coefs = c(-2.63, 1.23, 1.64, 0.32, -0.77)
)

est <- vector()
for (i in 1:nreps) {
  bdf <- df_uc_omc[sample(seq_len(n), n, replace = TRUE), ]
  results <- adjust_multinom_uc_omc(
    bdf,
    exposure = "X",
    outcome = "Ystar",
    confounders = c("C1", "C2"),
    u1y0_model_coefs = c(-0.31, 0.60, 0.01, -0.08, 0.10),
    u0y1_model_coefs = c(-3.07, 0.66, 1.65, 0.42, -0.85),
    u1y1_model_coefs = c(-2.63, 1.23, 1.64, 0.32, -0.77)
  )
  est[i] <- results$estimate
}

or_true <- exp(summary(nobias_model)$coef[2, 1])
or_adjusted <- median(est)

test_that("odds ratio and confidence interval output", {
  expect_gt(or_adjusted, or_true - 0.1)
  expect_lt(or_adjusted, or_true + 0.1)
  expect_vector(
    single_run$ci,
    ptype = double(),
    size = 2
  )
})

# 3 confounders

nobias_model <- glm(Y ~ X + C1 + C2 + C3 + U,
                    family = binomial(link = "logit"),
                    data = df_uc_omc_source)

# uy_model <- multinom(
#   paste0(U, Y) ~ X + Ystar + C1 + C2 + C3,
#   data = df_uc_omc_source
# )
# summary(uy_model, digits = 2)

single_run <- adjust_multinom_uc_omc(
  df_uc_omc,
  exposure = "X",
  outcome = "Ystar",
  confounders = c("C1", "C2", "C3"),
  u1y0_model_coefs = c(-0.2, 0.62, 0.01, -0.08, 0.10, -0.15),
  u0y1_model_coefs = c(-3.3, 0.63, 1.65, 0.42, -0.85, 0.26),
  u1y1_model_coefs = c(-2.7, 1.22, 1.64, 0.32, -0.77, 0.09)
)

est <- vector()
for (i in 1:nreps) {
  bdf <- df_uc_omc[sample(seq_len(n), n, replace = TRUE), ]
  results <- adjust_multinom_uc_omc(
    bdf,
    exposure = "X",
    outcome = "Ystar",
    confounders = c("C1", "C2", "C3"),
    u1y0_model_coefs = c(-0.2, 0.62, 0.01, -0.08, 0.10, -0.15),
    u0y1_model_coefs = c(-3.3, 0.63, 1.65, 0.42, -0.85, 0.26),
    u1y1_model_coefs = c(-2.7, 1.22, 1.64, 0.32, -0.77, 0.09)
  )
  est[i] <- results$estimate
}

or_true <- exp(summary(nobias_model)$coef[2, 1])
or_adjusted <- median(est)

test_that("odds ratio and confidence interval output", {
  expect_gt(or_adjusted, or_true - 0.1)
  expect_lt(or_adjusted, or_true + 0.1)
  expect_vector(
    single_run$ci,
    ptype = double(),
    size = 2
  )
})
