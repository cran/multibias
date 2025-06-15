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

## -----------------------------------------------------------------------------
exposure_outcome_table <- nhanes_filtered |>
  group_by(alcohol_extreme, mortstat) |>
  summarize(count = n()) |>
  ungroup() |>
  mutate(proportion = count / sum(count))

# Distribution of Exposure and Outcome:
print(exposure_outcome_table)

demographic_table <- nhanes_filtered |>
  group_by(alcohol_extreme) |>
  summarize(
    n = n(),
    age_mean = mean(age),
    age_sd = sd(age),
    gender_prop = mean(gender_female)
  ) |>
  mutate(
    alcohol_extreme = if_else(
      alcohol_extreme == 1, "Extreme", "Non-extreme"
    )
  )

# Demographic Characteristics by Exposure Status:
print(demographic_table)

## -----------------------------------------------------------------------------
# Create observed data object with uncontrolled confounding bias
df_obs1 <- data_observed(
  data = nhanes_filtered,
  bias = "uc",
  exposure = "alcohol_extreme",
  outcome = "mortstat",
  confounders = c("age", "gender_female")
)

summary(df_obs1)

## -----------------------------------------------------------------------------
# Create validation data with smoking information
df_temp1 <- nhanes_filtered |>
  filter(!is.na(smoked_100cigs))

df_val1 <- data_validation(
  data = df_temp1,
  true_exposure = "alcohol_extreme",
  true_outcome = "mortstat",
  confounders = c("age", "gender_female", "smoked_100cigs")
)

# Perform bias adjustment
set.seed(1234)
uc_adjusted <- multibias_adjust(
  df_obs1,
  df_val1,
  bootstrap = TRUE,
  bootstrap_reps = 100
)

# Uncontrolled Confounding Adjusted Results:
print(uc_adjusted)

## -----------------------------------------------------------------------------
# Create observed data object with both biases
df_obs2 <- data_observed(
  data = nhanes_filtered,
  bias = c("em", "uc"),
  exposure = "alcohol_extreme",
  outcome = "mortstat",
  confounders = c("age", "gender_female")
)

# Create validation data with adjusted alcohol consumption
df_temp2 <- nhanes_filtered |>
  filter(!is.na(smoked_100cigs)) |>
  mutate(
    # Assume 50% higher consumption for high SES individuals
    alcohol_adj = if_else(
      income == "$100,000 and Over" | education == "College graduate or above",
      alcohol_day_total * 1.5,
      alcohol_day_total
    )
  ) |>
  mutate(alcohol_extreme_adj = case_when(
    alcohol_adj > 14 * 1.5 & gender_female == 1 ~ 1,
    alcohol_adj > 28 * 1.5 & gender_female == 0 ~ 1,
    TRUE ~ 0
  ))

df_val2 <- data_validation(
  data = df_temp2,
  true_exposure = "alcohol_extreme_adj",
  true_outcome = "mortstat",
  confounders = c("age", "gender_female", "smoked_100cigs"),
  misclassified_exposure = "alcohol_extreme"
)

# Perform bias adjustment
set.seed(1234)
uc_em_adjusted <- multibias_adjust(
  df_obs2,
  df_val2,
  bootstrap = TRUE,
  bootstrap_reps = 100
)

# Exposure Misclassification & Confounding Adjusted Results:
print(uc_em_adjusted)

## -----------------------------------------------------------------------------
# Create observed data object with all three biases
df_obs3 <- data_observed(
  data = nhanes_filtered,
  bias = c("em", "uc", "sel"),
  exposure = "alcohol_extreme",
  outcome = "mortstat",
  confounders = c("age", "gender_female")
)

# Prepare data for selection bias adjustment
df_temp3a <- nhanes_filtered |>
  filter(!is.na(smoked_100cigs)) |>
  mutate(
    alcohol_adj = if_else(
      income == "$100,000 and Over" | education == "College graduate or above",
      alcohol_day_total * 1.5,
      alcohol_day_total
    ),
    alcohol_extreme_adj = case_when(
      alcohol_adj > 14 * 1.5 & gender_female == 1 ~ 1,
      alcohol_adj > 28 * 1.5 & gender_female == 0 ~ 1,
      TRUE ~ 0
    ),
    # Combine survey weights
    weight = if_else(weight_day2 == 0, weight_day1, weight_day2)
  )

# Create selection indicator based on sampling weights
set.seed(1234)
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

df_selected_sample$selection <- 1
df_not_selected_sample <- df_temp3a[!(df_temp3a$seqn %in% selected_sample), ]
df_not_selected_sample$selection <- 0

df_temp3b <- rbind(df_selected_sample, df_not_selected_sample)

# Create validation data with selection information
df_val3 <- data_validation(
  data = df_temp3b,
  true_exposure = "alcohol_extreme_adj",
  true_outcome = "mortstat",
  confounders = c("age", "gender_female", "smoked_100cigs"),
  misclassified_exposure = "alcohol_extreme",
  selection = "selection"
)

# Perform final bias adjustment
set.seed(1234)
final_adjusted <- multibias_adjust(
  df_obs3,
  df_val3,
  bootstrap = TRUE,
  bootstrap_reps = 100
)

# Triple Bias Adjusted Results:
print(final_adjusted)

## -----------------------------------------------------------------------------
multibias_plot(
  df_obs1,
  multibias_result_list = list(
    "Adjusted - Single Bias" = uc_adjusted,
    "Adjusted - Double Biases" = uc_em_adjusted,
    "Adjusted - Triple Biases" = final_adjusted
  ),
  log_scale = TRUE
)

