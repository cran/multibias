## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(multibias)
library(dplyr)

## -----------------------------------------------------------------------------
nhanes <- read.csv("nhanes.csv")

## -----------------------------------------------------------------------------
nhanes_filtered <- nhanes |>
  mutate(alcohol_extreme = case_when(
    alcohol_day_total > 14 * 1.5 & gender_female == 1 ~ 1,
    alcohol_day_total > 28 * 1.5 & gender_female == 0 ~ 1,
    TRUE ~ 0
  )) |>
  filter(age >= 18) |>
  filter(eligstat == 1) |>
  filter(alcohol_12mo > 0)

## ----message = FALSE----------------------------------------------------------
nhanes_filtered |>
  group_by(alcohol_extreme, mortstat) |>
  summarize(count = n()) |>
  ungroup() |>
  mutate(proportion = count / sum(count))

## ----message = FALSE----------------------------------------------------------
nhanes_filtered |>
  group_by(alcohol_extreme) |>
  summarize(
    age_mean = mean(age),
    gender_prop = mean(gender_female)
  )

## ----message = FALSE----------------------------------------------------------
nhanes_filtered |>
  group_by(mortstat) |>
  summarize(
    age_mean = mean(age),
    gender_prop = mean(gender_female)
  )

## -----------------------------------------------------------------------------
base_mod <- glm(
  mortstat ~ alcohol_extreme + gender_female + age,
  data = nhanes_filtered,
  family = binomial(link = "logit")
)

exp(coef(base_mod))
exp(confint(base_mod))

## -----------------------------------------------------------------------------
df_obs <- data_observed(
  data = nhanes_filtered,
  exposure = "alcohol_extreme",
  outcome = "mortstat",
  confounders = c("age", "gender_female")
)

print(df_obs)

## -----------------------------------------------------------------------------
df_temp1 <- nhanes_filtered |>
  filter(!is.na(smoked_100cigs))

df_val1 <- data_validation(
  data = df_temp1,
  true_exposure = "alcohol_extreme",
  true_outcome = "mortstat",
  confounders = c("age", "gender_female", "smoked_100cigs")
)

## -----------------------------------------------------------------------------
set.seed(1234)
adjust_uc(df_obs, df_val1)

## -----------------------------------------------------------------------------
df_temp2 <- nhanes_filtered |>
  filter(!is.na(smoked_100cigs)) |>
  mutate(alcohol_adj = if_else(
    income == "$100,000 and Over" | education == "College graduate or above",
    alcohol_day_total * 1.5,
    alcohol_day_total
  )) |>
  mutate(alcohol_extreme_adj = case_when(
    alcohol_adj > 14 * 2 & gender_female == 1 ~ 1,
    alcohol_adj > 14 * 4 & gender_female == 0 ~ 1,
    TRUE ~ 0
  ))

df_val2 <- data_validation(
  data = df_temp2,
  true_exposure = "alcohol_extreme_adj",
  true_outcome = "mortstat",
  confounders = c("age", "gender_female", "smoked_100cigs"),
  misclassified_exposure = "alcohol_extreme"
)

## -----------------------------------------------------------------------------
set.seed(1234)
adjust_uc_em(df_obs, df_val2)

## -----------------------------------------------------------------------------
df_temp3a <- nhanes_filtered |>
  filter(!is.na(smoked_100cigs)) |>
  mutate(alcohol_adj = if_else(
    income == "$100,000 and Over" | education == "College graduate or above",
    alcohol_day_total * 1.5,
    alcohol_day_total
  )) |>
  mutate(alcohol_extreme_adj = case_when(
    alcohol_adj > 14 * 2 & gender_female == 1 ~ 1,
    alcohol_adj > 14 * 4 & gender_female == 0 ~ 1,
    TRUE ~ 0
  )) |>
  mutate(
    weight = if_else(weight_day2 == 0, weight_day1, weight_day2)
  )

## -----------------------------------------------------------------------------
selected_sample <- sample(
  x = df_temp3a$seqn,
  size = nrow(df_temp3a),
  replace = TRUE,
  prob = df_temp3a$weight
)

df_selected_sample <- data.frame()
for (id in selected_sample) {
  df_selected_sample <- rbind(
    df_selected_sample,
    df_temp3a[df_temp3a$seqn == id, ]
  )
}

not_selected_sample <- df_temp3a$seqn[!(df_temp3a$seqn %in% selected_sample)]
df_not_selected_sample <- df_temp3a[df_temp3a$seqn %in% not_selected_sample, ]

## -----------------------------------------------------------------------------
length(not_selected_sample) + length(unique(selected_sample)) == nrow(df_temp3a)

## -----------------------------------------------------------------------------
df_selected_sample$selection <- 1
df_not_selected_sample$selection <- 0
df_temp3b <- rbind(df_selected_sample, df_not_selected_sample)

## -----------------------------------------------------------------------------
df_val3 <- data_validation(
  data = df_temp3b,
  true_exposure = "alcohol_extreme_adj",
  true_outcome = "mortstat",
  confounders = c("age", "gender_female", "smoked_100cigs"),
  misclassified_exposure = "alcohol_extreme",
  selection = "selection"
)

## -----------------------------------------------------------------------------
set.seed(1234)
adjust_uc_em_sel(df_obs, df_val3)

