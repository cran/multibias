set.seed(1234)
n <- 10000
nreps <- 10

# cont Y just for testing that function runs
df_emc$Y_cont <- plogis(df_emc$Y) + rnorm(nrow(df_emc), mean = 0, sd = 0.1)

# 0 confounders

nobias_model <- glm(Y ~ X,
                    family = binomial(link = "logit"),
                    data = df_emc_source)

x_model <- glm(X ~ Xstar + Y,
               family = binomial(link = "logit"),
               data = df_emc_source)

single_run <- adjust_emc(
  df_emc,
  exposure = "Xstar",
  outcome = "Y_cont",
  x_model_coefs = c(
    x_model$coef[1],
    x_model$coef[2],
    x_model$coef[3]
  )
)

est <- vector()
for (i in 1:nreps) {
  bdf <- df_emc[sample(seq_len(n), n, replace = TRUE), ]
  results <- adjust_emc(
    bdf,
    exposure = "Xstar",
    outcome = "Y",
    x_model_coefs = c(
      x_model$coef[1],
      x_model$coef[2],
      x_model$coef[3]
    )
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

nobias_model <- glm(Y ~ X + C1,
                    family = binomial(link = "logit"),
                    data = df_emc_source)

x_model <- glm(X ~ Xstar + Y + C1,
               family = binomial(link = "logit"),
               data = df_emc_source)

single_run <- adjust_emc(
  df_emc,
  exposure = "Xstar",
  outcome = "Y_cont",
  confounders = "C1",
  x_model_coefs = c(
    x_model$coef[1],
    x_model$coef[2],
    x_model$coef[3],
    x_model$coef[4]
  )
)

est <- vector()
for (i in 1:nreps) {
  bdf <- df_emc[sample(seq_len(n), n, replace = TRUE), ]
  results <- adjust_emc(
    bdf,
    exposure = "Xstar",
    outcome = "Y",
    confounders = "C1",
    x_model_coefs = c(
      x_model$coef[1],
      x_model$coef[2],
      x_model$coef[3],
      x_model$coef[4]
    )
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

nobias_model <- glm(Y ~ X + C1 + C2,
                    family = binomial(link = "logit"),
                    data = df_emc_source)

x_model <- glm(X ~ Xstar + Y + C1 + C2,
               family = binomial(link = "logit"),
               data = df_emc_source)

single_run <- adjust_emc(
  df_emc,
  exposure = "Xstar",
  outcome = "Y_cont",
  confounders = c("C1", "C2"),
  x_model_coefs = c(
    x_model$coef[1],
    x_model$coef[2],
    x_model$coef[3],
    x_model$coef[4],
    x_model$coef[5]
  )
)

est <- vector()
for (i in 1:nreps) {
  bdf <- df_emc[sample(seq_len(n), n, replace = TRUE), ]
  results <- adjust_emc(
    bdf,
    exposure = "Xstar",
    outcome = "Y",
    confounders = c("C1", "C2"),
    x_model_coefs = c(
      x_model$coef[1],
      x_model$coef[2],
      x_model$coef[3],
      x_model$coef[4],
      x_model$coef[5]
    )
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

nobias_model <- glm(Y ~ X + C1 + C2 + C3,
                    family = binomial(link = "logit"),
                    data = df_emc_source)

x_model <- glm(X ~ Xstar + Y + C1 + C2 + C3,
               family = binomial(link = "logit"),
               data = df_emc_source)

single_run <- adjust_emc(
  df_emc,
  exposure = "Xstar",
  outcome = "Y_cont",
  confounders = c("C1", "C2", "C3"),
  x_model_coefs = c(
    x_model$coef[1],
    x_model$coef[2],
    x_model$coef[3],
    x_model$coef[4],
    x_model$coef[5],
    x_model$coef[6]
  )
)

est <- vector()
for (i in 1:nreps) {
  bdf <- df_emc[sample(seq_len(n), n, replace = TRUE), ]
  results <- adjust_emc(
    bdf,
    exposure = "Xstar",
    outcome = "Y",
    confounders = c("C1", "C2", "C3"),
    x_model_coefs = c(
      x_model$coef[1],
      x_model$coef[2],
      x_model$coef[3],
      x_model$coef[4],
      x_model$coef[5],
      x_model$coef[6]
    )
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