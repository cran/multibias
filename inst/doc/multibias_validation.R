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
biased_model <- glm(Y ~ Xstar + C1 + C2 + C3, ,
                    family = binomial(link = "logit"),
                    data = df_uc_em_sel)
biased_or <- round(exp(coef(biased_model)[2]), 2)
print(paste0("Biased Odds Ratio: ", biased_or))

## ----eval = TRUE--------------------------------------------------------------
u_model <- glm(U ~ X + Y,
               family = binomial(link = "logit"),
               data = df_uc_em_sel_source)
x_model <- glm(X ~ Xstar + Y + C1 + C2 + C3,
               family = binomial(link = "logit"),
               data = df_uc_em_sel_source)
s_model <- glm(S ~ Xstar + Y + C1 + C2 + C3,
               family = binomial(link = "logit"),
               data = df_uc_em_sel_source)

## ----eval = FALSE-------------------------------------------------------------
# library(doParallel)
# 
# no_cores <- detectCores() - 1
# registerDoParallel(cores = no_cores)
# cl <- makeCluster(no_cores)
# 
# set.seed(1234)
# nreps <- 1000
# est <- vector(length = nreps)

## ----eval = FALSE-------------------------------------------------------------
# or <- foreach(i = 1:nreps, .combine = c,
#               .packages = c("dplyr", "multibias")) %dopar% {
# 
#   df_sample <- df_uc_em_sel[sample(seq_len(nrow(df_uc_em_sel)),
#                                    nrow(df_uc_em_sel),
#                                    replace = TRUE), ]
# 
#   est[i] <- adjust_uc_em_sel(
#     data_observed = data_observed(
#       data = df_sample,
#       exposure = "Xstar",
#       outcome = "Y",
#       confounders = c("C1", "C2", "C3")
#     ),
#     u_model_coefs = c(
#       rnorm(1, mean = u_model$coef[1], sd = summary(u_model)$coef[1, 2]),
#       rnorm(1, mean = u_model$coef[2], sd = summary(u_model)$coef[2, 2]),
#       rnorm(1, mean = u_model$coef[3], sd = summary(u_model)$coef[3, 2])
#     ),
#     x_model_coefs = c(
#       rnorm(1, mean = x_model$coef[1], sd = summary(x_model)$coef[1, 2]),
#       rnorm(1, mean = x_model$coef[2], sd = summary(x_model)$coef[2, 2]),
#       rnorm(1, mean = x_model$coef[3], sd = summary(x_model)$coef[3, 2]),
#       rnorm(1, mean = x_model$coef[4], sd = summary(x_model)$coef[4, 2]),
#       rnorm(1, mean = x_model$coef[5], sd = summary(x_model)$coef[5, 2]),
#       rnorm(1, mean = x_model$coef[6], sd = summary(x_model)$coef[6, 2])
#     ),
#     s_model_coefs = c(
#       rnorm(1, mean = s_model$coef[1], sd = summary(s_model)$coef[1, 2]),
#       rnorm(1, mean = s_model$coef[2], sd = summary(s_model)$coef[2, 2]),
#       rnorm(1, mean = s_model$coef[3], sd = summary(s_model)$coef[3, 2]),
#       rnorm(1, mean = s_model$coef[4], sd = summary(s_model)$coef[4, 2]),
#       rnorm(1, mean = s_model$coef[5], sd = summary(s_model)$coef[5, 2]),
#       rnorm(1, mean = s_model$coef[6], sd = summary(s_model)$coef[6, 2])
#     )
#   )$estimate
# }

## ----eval = FALSE-------------------------------------------------------------
# # odds ratio estimate
# round(median(or), 2)
# #> 2.02
# 
# # confidence interval
# round(quantile(or, c(.025, .975)), 2)
# #> 1.93 2.11

