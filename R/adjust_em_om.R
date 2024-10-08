#' Adust for exposure misclassification and outcome misclassification.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `adjust_emc_omc()` was renamed to `adjust_em_om()`
#' @keywords internal
#'
#' @export
adjust_emc_omc <- function(
    data,
    exposure,
    outcome,
    confounders = NULL,
    x_model_coefs = NULL,
    y_model_coefs = NULL,
    x1y0_model_coefs = NULL,
    x0y1_model_coefs = NULL,
    x1y1_model_coefs = NULL,
    level = 0.95) {
  lifecycle::deprecate_warn("1.5.3", "adjust_emc_omc()", "adjust_em_om()")
  adjust_em_om(
    data,
    exposure,
    outcome,
    confounders,
    x_model_coefs,
    y_model_coefs,
    x1y0_model_coefs = NULL,
    x0y1_model_coefs = NULL,
    x1y1_model_coefs = NULL,
    level
  )
}

# bias adjust with x_model_coefs and y_model_coefs

em_om_single <- function(
    data,
    exposure,
    outcome,
    confounders,
    x_model_coefs,
    y_model_coefs) {
  n <- nrow(data)
  len_c <- length(confounders)
  len_x_coefs <- length(x_model_coefs)
  len_y_coefs <- length(y_model_coefs)

  force_len(
    len_x_coefs,
    3 + len_c,
    paste0(
      "Incorrect length of X model coefficients. ",
      "Length should equal 3 + number of confounders."
    )
  )
  force_len(
    len_y_coefs,
    3 + len_c,
    paste0(
      "Incorrect length of Y model coefficients. ",
      "Length should equal 3 + number of confounders."
    )
  )

  xstar <- data[, exposure]
  ystar <- data[, outcome]

  x1_0 <- x_model_coefs[1]
  x1_xstar <- x_model_coefs[2]
  x1_ystar <- x_model_coefs[3]

  y1_0 <- y_model_coefs[1]
  y1_x <- y_model_coefs[2]
  y1_ystar <- y_model_coefs[3]

  if (is.null(confounders)) {
    df <- data.frame(Xstar = xstar, Ystar = ystar)
    df$Xpred <- rbinom(
      n, 1, plogis(
        x1_0 + x1_xstar * df$Xstar + x1_ystar * df$Ystar
      )
    )
    df$Ypred <- rbinom(
      n, 1, plogis(
        y1_0 + y1_x * df$Xpred + y1_ystar * df$Ystar
      )
    )

    final <- glm(
      Ypred ~ Xpred,
      family = binomial(link = "logit"),
      data = df
    )
  } else if (len_c == 1) {
    c1 <- data[, confounders]
    df <- data.frame(Xstar = xstar, Ystar = ystar, C1 = c1)

    x1_c1 <- x_model_coefs[4]
    y1_c1 <- y_model_coefs[4]

    df$Xpred <- rbinom(
      n, 1, plogis(
        x1_0 + x1_xstar * df$Xstar + x1_ystar * df$Ystar + x1_c1 * df$C1
      )
    )
    df$Ypred <- rbinom(
      n, 1, plogis(
        y1_0 + y1_x * df$Xpred + y1_ystar * df$Ystar + y1_c1 * df$C1
      )
    )

    final <- glm(
      Ypred ~ Xpred + C1,
      family = binomial(link = "logit"),
      data = df
    )
  } else if (len_c == 2) {
    c1 <- data[, confounders[1]]
    c2 <- data[, confounders[2]]

    df <- data.frame(Xstar = xstar, Ystar = ystar, C1 = c1, C2 = c2)

    x1_c1 <- x_model_coefs[4]
    x1_c2 <- x_model_coefs[5]

    y1_c1 <- y_model_coefs[4]
    y1_c2 <- y_model_coefs[5]

    df$Xpred <- rbinom(
      n, 1, plogis(
        x1_0 + x1_xstar * df$Xstar + x1_ystar * df$Ystar +
          x1_c1 * df$C1 + x1_c2 * df$C2
      )
    )
    df$Ypred <- rbinom(
      n, 1, plogis(
        y1_0 + y1_x * df$Xpred + y1_ystar * df$Ystar +
          y1_c1 * df$C1 + y1_c2 * df$C2
      )
    )

    final <- glm(
      Ypred ~ Xpred + C1 + C2,
      family = binomial(link = "logit"),
      data = df
    )
  } else if (len_c == 3) {
    c1 <- data[, confounders[1]]
    c2 <- data[, confounders[2]]
    c3 <- data[, confounders[3]]

    df <- data.frame(Xstar = xstar, Ystar = ystar, C1 = c1, C2 = c2, C3 = c3)

    x1_c1 <- x_model_coefs[4]
    x1_c2 <- x_model_coefs[5]
    x1_c3 <- x_model_coefs[6]

    y1_c1 <- y_model_coefs[4]
    y1_c2 <- y_model_coefs[5]
    y1_c3 <- y_model_coefs[6]

    df$Xpred <- rbinom(
      n, 1, plogis(
        x1_0 + x1_xstar * df$Xstar +
          x1_ystar * df$Ystar + x1_c1 * df$C1 +
          x1_c2 * df$C2 + x1_c3 * df$C3
      )
    )
    df$Ypred <- rbinom(
      n, 1, plogis(
        y1_0 + y1_x * df$Xpred +
          y1_ystar * df$Ystar +
          y1_c1 * df$C1 + y1_c2 * df$C2
      )
    )

    final <- glm(
      Ypred ~ Xpred + C1 + C2 + C3,
      family = binomial(link = "logit"),
      data = df
    )
  } else if (len_c > 3) {
    stop("This function is currently not compatible with >3 confounders.")
  }

  return(final)
}

# bias adjust with multinomial coefs

em_om_multinom <- function(
    data,
    exposure,
    outcome,
    confounders,
    x1y0_model_coefs,
    x0y1_model_coefs,
    x1y1_model_coefs) {

  n <- nrow(data)
  len_c <- length(confounders)
  len_x1y0_coefs <- length(x1y0_model_coefs)
  len_x0y1_coefs <- length(x0y1_model_coefs)
  len_x1y1_coefs <- length(x1y1_model_coefs)

  force_len(
    len_x1y0_coefs,
    3 + len_c,
    paste0(
      "Incorrect length of X1Y0 model coefficients. ",
      "Length should equal 3 + number of confounders."
    )
  )
  force_len(
    len_x0y1_coefs,
    3 + len_c,
    paste0(
      "Incorrect length of X0Y1 model coefficients. ",
      "Length should equal 3 + number of confounders."
    )
  )
  force_len(
    len_x1y1_coefs,
    3 + len_c,
    paste0(
      "Incorrect length of X1Y1 model coefficients. ",
      "Length should equal 3 + number of confounders."
    )
  )

  xstar <- data[, exposure]
  ystar <- data[, outcome]

  x1y0_0 <- x1y0_model_coefs[1]
  x1y0_xstar <- x1y0_model_coefs[2]
  x1y0_ystar <- x1y0_model_coefs[3]

  x0y1_0 <- x0y1_model_coefs[1]
  x0y1_xstar <- x0y1_model_coefs[2]
  x0y1_ystar <- x0y1_model_coefs[3]

  x1y1_0 <- x1y1_model_coefs[1]
  x1y1_xstar <- x1y1_model_coefs[2]
  x1y1_ystar <- x1y1_model_coefs[3]

  if (is.null(confounders)) {
    df <- data.frame(Xstar = xstar, Ystar = ystar)

    p_x1y0 <- exp(x1y0_0 + x1y0_xstar * df$Xstar + x1y0_ystar * df$Ystar)
    p_x0y1 <- exp(x0y1_0 + x0y1_xstar * df$Xstar + x0y1_ystar * df$Ystar)
    p_x1y1 <- exp(x1y1_0 + x1y1_xstar * df$Xstar + x1y1_ystar * df$Ystar)

    denom <- (1 + p_x1y0 + p_x0y1 + p_x1y1)

    x0y0_pred <- 1 / denom
    x1y0_pred <- p_x1y0 / denom
    x0y1_pred <- p_x0y1 / denom
    x1y1_pred <- p_x1y1 / denom

    df_xy_pred <- data.frame(
      X0Y0 = x0y0_pred,
      X1Y0 = x1y0_pred,
      X0Y1 = x0y1_pred,
      X1Y1 = x1y1_pred
    )
    df_xy_pred4 <- bind_rows(df_xy_pred, df_xy_pred, df_xy_pred, df_xy_pred)

    combined <- bind_rows(df, df, df, df) %>%
      bind_cols(df_xy_pred4) %>%
      mutate(
        Xbar = rep(c(1, 0, 1, 0), each = n),
        Ybar = rep(c(1, 1, 0, 0), each = n),
        pXY = case_when(
          Xbar == 0 & Ybar == 0 ~ X0Y0,
          Xbar == 1 & Ybar == 0 ~ X1Y0,
          Xbar == 0 & Ybar == 1 ~ X0Y1,
          Xbar == 1 & Ybar == 1 ~ X1Y1
        )
      )
    suppressWarnings({
      final <- glm(
        Ybar ~ Xbar,
        family = binomial(link = "logit"),
        weights = combined$pXY,
        data = combined
      )
    })
  } else if (len_c == 1) {
    c1 <- data[, confounders]

    df <- data.frame(Xstar = xstar, Ystar = ystar, C1 = c1)

    x1y0_c1 <- x1y0_model_coefs[4]
    x0y1_c1 <- x0y1_model_coefs[4]
    x1y1_c1 <- x1y1_model_coefs[4]

    p_x1y0 <- exp(
      x1y0_0 + x1y0_xstar * df$Xstar + x1y0_ystar * df$Ystar +
        x1y0_c1 * df$C1
    )
    p_x0y1 <- exp(
      x0y1_0 + x0y1_xstar * df$Xstar + x0y1_ystar * df$Ystar +
        x0y1_c1 * df$C1
    )
    p_x1y1 <- exp(
      x1y1_0 + x1y1_xstar * df$Xstar + x1y1_ystar * df$Ystar +
        x1y1_c1 * df$C1
    )

    denom <- (1 + p_x1y0 + p_x0y1 + p_x1y1)

    x0y0_pred <- 1 / denom
    x1y0_pred <- p_x1y0 / denom
    x0y1_pred <- p_x0y1 / denom
    x1y1_pred <- p_x1y1 / denom

    df_xy_pred <- data.frame(
      X0Y0 = x0y0_pred,
      X1Y0 = x1y0_pred,
      X0Y1 = x0y1_pred,
      X1Y1 = x1y1_pred
    )
    df_xy_pred4 <- bind_rows(df_xy_pred, df_xy_pred, df_xy_pred, df_xy_pred)

    combined <- bind_rows(df, df, df, df) %>%
      bind_cols(df_xy_pred4) %>%
      mutate(
        Xbar = rep(c(1, 0, 1, 0), each = n),
        Ybar = rep(c(1, 1, 0, 0), each = n),
        pXY = case_when(
          Xbar == 0 & Ybar == 0 ~ X0Y0,
          Xbar == 1 & Ybar == 0 ~ X1Y0,
          Xbar == 0 & Ybar == 1 ~ X0Y1,
          Xbar == 1 & Ybar == 1 ~ X1Y1
        )
      )

    suppressWarnings({
      final <- glm(
        Ybar ~ Xbar + C1,
        family = binomial(link = "logit"),
        weights = combined$pXY,
        data = combined
      )
    })
  } else if (len_c == 2) {
    c1 <- data[, confounders[1]]
    c2 <- data[, confounders[2]]

    df <- data.frame(Xstar = xstar, Ystar = ystar, C1 = c1, C2 = c2)

    x1y0_c1 <- x1y0_model_coefs[4]
    x1y0_c2 <- x1y0_model_coefs[5]

    x0y1_c1 <- x0y1_model_coefs[4]
    x0y1_c2 <- x0y1_model_coefs[5]

    x1y1_c1 <- x1y1_model_coefs[4]
    x1y1_c2 <- x1y1_model_coefs[5]

    p_x1y0 <- exp(
      x1y0_0 + x1y0_xstar * df$Xstar + x1y0_ystar * df$Ystar +
        x1y0_c1 * df$C1 + x1y0_c2 * df$C2
    )
    p_x0y1 <- exp(
      x0y1_0 + x0y1_xstar * df$Xstar + x0y1_ystar * df$Ystar +
        x0y1_c1 * df$C1 + x0y1_c2 * df$C2
    )
    p_x1y1 <- exp(
      x1y1_0 + x1y1_xstar * df$Xstar + x1y1_ystar * df$Ystar +
        x1y1_c1 * df$C1 + x1y1_c2 * df$C2
    )

    denom <- (1 + p_x1y0 + p_x0y1 + p_x1y1)

    x0y0_pred <- 1 / denom
    x1y0_pred <- p_x1y0 / denom
    x0y1_pred <- p_x0y1 / denom
    x1y1_pred <- p_x1y1 / denom

    df_xy_pred <- data.frame(
      X0Y0 = x0y0_pred,
      X1Y0 = x1y0_pred,
      X0Y1 = x0y1_pred,
      X1Y1 = x1y1_pred
    )
    df_xy_pred4 <- bind_rows(df_xy_pred, df_xy_pred, df_xy_pred, df_xy_pred)

    combined <- bind_rows(df, df, df, df) %>%
      bind_cols(df_xy_pred4) %>%
      mutate(
        Xbar = rep(c(1, 0, 1, 0), each = n),
        Ybar = rep(c(1, 1, 0, 0), each = n),
        pXY = case_when(
          Xbar == 0 & Ybar == 0 ~ X0Y0,
          Xbar == 1 & Ybar == 0 ~ X1Y0,
          Xbar == 0 & Ybar == 1 ~ X0Y1,
          Xbar == 1 & Ybar == 1 ~ X1Y1
        )
      )

    suppressWarnings({
      final <- glm(
        Ybar ~ Xbar + C1 + C2,
        family = binomial(link = "logit"),
        weights = combined$pXY,
        data = combined
      )
    })
  } else if (len_c == 3) {
    c1 <- data[, confounders[1]]
    c2 <- data[, confounders[2]]
    c3 <- data[, confounders[3]]

    df <- data.frame(Xstar = xstar, Ystar = ystar, C1 = c1, C2 = c2, C3 = c3)

    x1y0_c1 <- x1y0_model_coefs[4]
    x1y0_c2 <- x1y0_model_coefs[5]
    x1y0_c3 <- x1y0_model_coefs[6]

    x0y1_c1 <- x0y1_model_coefs[4]
    x0y1_c2 <- x0y1_model_coefs[5]
    x0y1_c3 <- x0y1_model_coefs[6]

    x1y1_c1 <- x1y1_model_coefs[4]
    x1y1_c2 <- x1y1_model_coefs[5]
    x1y1_c3 <- x1y1_model_coefs[6]

    p_x1y0 <- exp(
      x1y0_0 + x1y0_xstar * df$Xstar + x1y0_ystar * df$Ystar +
        x1y0_c1 * df$C1 + x1y0_c2 * df$C2 + x1y0_c3 * df$C3
    )
    p_x0y1 <- exp(
      x0y1_0 + x0y1_xstar * df$Xstar + x0y1_ystar * df$Ystar +
        x0y1_c1 * df$C1 + x0y1_c2 * df$C2 + x0y1_c3 * df$C3
    )
    p_x1y1 <- exp(
      x1y1_0 + x1y1_xstar * df$Xstar + x1y1_ystar * df$Ystar +
        x1y1_c1 * df$C1 + x1y1_c2 * df$C2 + x1y1_c3 * df$C3
    )

    denom <- (1 + p_x1y0 + p_x0y1 + p_x1y1)

    x0y0_pred <- 1 / denom
    x1y0_pred <- p_x1y0 / denom
    x0y1_pred <- p_x0y1 / denom
    x1y1_pred <- p_x1y1 / denom

    df_xy_pred <- data.frame(
      X0Y0 = x0y0_pred,
      X1Y0 = x1y0_pred,
      X0Y1 = x0y1_pred,
      X1Y1 = x1y1_pred
    )
    df_xy_pred4 <- bind_rows(df_xy_pred, df_xy_pred, df_xy_pred, df_xy_pred)

    combined <- bind_rows(df, df, df, df) %>%
      bind_cols(df_xy_pred4) %>%
      mutate(
        Xbar = rep(c(1, 0, 1, 0), each = n),
        Ybar = rep(c(1, 1, 0, 0), each = n),
        pXY = case_when(
          Xbar == 0 & Ybar == 0 ~ X0Y0,
          Xbar == 1 & Ybar == 0 ~ X1Y0,
          Xbar == 0 & Ybar == 1 ~ X0Y1,
          Xbar == 1 & Ybar == 1 ~ X1Y1
        )
      )

    suppressWarnings({
      final <- glm(
        Ybar ~ Xbar + C1 + C2 + C3,
        family = binomial(link = "logit"),
        weights = combined$pXY,
        data = combined
      )
    })
  } else if (len_c > 3) {
    stop("This function is currently not compatible with >3 confounders.")
  }

  return(final)

}

#' Adust for exposure misclassification and outcome misclassification.
#'
#' `adjust_em_om` returns the exposure-outcome odds ratio and confidence
#' interval, adjusted for exposure misclassification and outcome
#' misclassification. Two different options for the bias parameters are
#' available here: 1) parameters from separate models of *X* and *Y*
#' (`x_model_coefs` and `y_model_coefs`) or 2) parameters from
#' a joint model of *X* and *Y* (`x1y0_model_coefs`,
#' `x0y1_model_coefs`, and `x1y1_model_coefs`).
#'
#' Values for the regression coefficients can be applied as
#' fixed values or as single draws from a probability
#' distribution (ex: `rnorm(1, mean = 2, sd = 1)`). The latter has
#' the advantage of allowing the researcher to capture the uncertainty
#' in the bias parameter estimates. To incorporate this uncertainty in the
#' estimate and confidence interval, this function should be run in loop across
#' bootstrap samples of the dataframe for analysis. The estimate and
#' confidence interval would then be obtained from the median and quantiles
#' of the distribution of odds ratio estimates.
#'
#' @inheritParams adjust_uc
#' @param x_model_coefs The regression coefficients corresponding to the model:
#' \ifelse{html}{\out{logit(P(X=1)) = &delta;<sub>0</sub> + &delta;<sub>1</sub>X* + &delta;<sub>2</sub>Y* + &delta;<sub>2+j</sub>C<sub>j</sub>, }}{\eqn{logit(P(X=1)) = \delta_0 + \delta_1 X^* + \delta_2 Y^* + \delta{2+j} C_j, }}
#' where *X* represents the binary true exposure, *X** is the
#' binary misclassified exposure, *Y** is the binary misclassified
#' outcome, *C* represents the vector of
#' measured confounders (if any), and *j* corresponds to the number
#' of measured confounders. The number of parameters is therefore 3 + *j*.
#' @param y_model_coefs The regression coefficients corresponding to the model:
#' \ifelse{html}{\out{logit(P(Y=1)) = &beta;<sub>0</sub> + &beta;<sub>1</sub>X + &beta;<sub>2</sub>Y* + &beta;<sub>2+j</sub>C<sub>j</sub>, }}{\eqn{logit(P(Y=1)) = \beta_0 + \beta_1 X + \beta_2 Y^* + \beta{{2+j}} C_j, }}
#' where *Y* represents the binary true exposure,
#' *X* is the binary exposure, *Y* is the binary
#' misclassified outcome, *C* represents the vector of measured
#' confounders (if any), and *j* corresponds to the number of measured
#' confounders. The number of parameters is therefore 3 + *j*.
#' @param x1y0_model_coefs The regression coefficients corresponding to the
#' model:
#' \ifelse{html}{\out{log(P(X=1,Y=0) / P(X=0,Y=0)) = &gamma;<sub>1,0</sub> + &gamma;<sub>1,1</sub>X* + &gamma;<sub>1,2</sub>Y* + &gamma;<sub>1,2+j</sub>C<sub>j</sub>, }}{\eqn{log(P(X=1,Y=0) / P(X=0,Y=0)) = \gamma_{1,0} + \gamma_{1,1} X^* + \gamma_{1,2} Y^* + \gamma_{1,2+j} C_j, }}
#' where *X* is the binary true exposure, *Y* is the binary
#' true outcome, *X** is the binary misclassified exposure, *Y**
#' is the binary misclassified outcome, *C* represents the vector of
#' measured confounders (if any), and *j* corresponds to the
#' number of measured confounders.
#' @param x0y1_model_coefs The regression coefficients corresponding to the
#' model:
#' \ifelse{html}{\out{log(P(X=0,Y=1) / P(X=0,Y=0)) = &gamma;<sub>2,0</sub> + &gamma;<sub>2,1</sub>X* + &gamma;<sub>2,2</sub>Y* + &gamma;<sub>2,2+j</sub>C<sub>j</sub>, }}{\eqn{log(P(X=0,U=1) / P(X=0,U=0)) = \gamma_{2,0} + \gamma_{2,1} X^* + \gamma_{2,2} Y^* + \gamma_{2,2+j} C_j, }}
#' where *X* is the binary true exposure, *Y* is the binary
#' true outcome, *X** is the binary misclassified exposure, *Y**
#' is the binary misclassified outcome, *C* represents the vector of
#' measured confounders (if any),
#' and *j* corresponds to the number of measured confounders.
#' @param x1y1_model_coefs The regression coefficients corresponding to the
#' model:
#' \ifelse{html}{\out{log(P(X=1,Y=1) / P(X=0,Y=0)) = &gamma;<sub>3,0</sub> + &gamma;<sub>3,1</sub>X* + &gamma;<sub>3,2</sub>Y* + &gamma;<sub>3,2+j</sub>C<sub>j</sub>, }}{\eqn{log(P(X=1,Y=1) / P(X=0,Y=0)) = \gamma_{3,0} + \gamma_{3,1} X^* + \gamma_{3,2} Y^* + \gamma_{3,2+j} C_j, }}
#' where *X* is the binary true exposure, *Y* is the binary
#' true outcome, *X** is the binary misclassified exposure, *Y**
#' is the binary misclassified outcome, *C* represents the vector of
#' measured confounders (if any),
#' and *j* corresponds to the number of measured confounders.
#' @return A list where the first item is the odds ratio estimate of the
#' effect of the exposure on the outcome and the second item is the
#' confidence interval as the vector: (lower bound, upper bound).
#'
#' @examples
#' # Using x_model_coefs and y_model_coefs -------------------------------------
#' adjust_em_om(
#'   df_em_om,
#'   exposure = "Xstar",
#'   outcome = "Ystar",
#'   confounders = "C1",
#'   x_model_coefs = c(-2.15, 1.64, 0.35, 0.38),
#'   y_model_coefs = c(-3.10, 0.63, 1.60, 0.39)
#' )
#'
#' # Using x1y0_model_coefs, x0y1_model_coefs, and x1y1_model_coefs ------------
#' adjust_em_om(
#'   df_em_om,
#'   exposure = "Xstar",
#'   outcome = "Ystar",
#'   confounders = "C1",
#'   x1y0_model_coefs = c(-2.18, 1.63, 0.23, 0.36),
#'   x0y1_model_coefs = c(-3.17, 0.22, 1.60, 0.40),
#'   x1y1_model_coefs = c(-4.76, 1.82, 1.83, 0.72)
#' )
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom stats binomial
#' @importFrom stats glm
#' @importFrom stats qnorm
#' @importFrom stats plogis
#' @importFrom rlang .data
#'
#' @export

adjust_em_om <- function(
    data,
    exposure,
    outcome,
    confounders = NULL,
    x_model_coefs = NULL,
    y_model_coefs = NULL,
    x1y0_model_coefs = NULL,
    x0y1_model_coefs = NULL,
    x1y1_model_coefs = NULL,
    level = 0.95) {

  xstar <- data[, exposure]
  ystar <- data[, outcome]

  force_binary(xstar, "Exposure must be a binary integer.")
  force_binary(ystar, "Outcome must be a binary integer.")

  # check that user correctly specified bias parameters
  if (
    !(
      (is.null(x_model_coefs) && is.null(y_model_coefs)) ||
        ((is.null(x1y0_model_coefs) && is.null(x0y1_model_coefs) &&
          is.null(x1y1_model_coefs))
        )
    )
  ) {
    stop(
      "Bias parameters must be specified for:\n
       1) x_model_coefs and y_model_coefs OR\n
       2) x1y0_model_coefs, x0y1_model_coefs, and x1y1_model_coefs."
    )
  }

  if (!is.null(x_model_coefs)) {
    final <- em_om_single(
      data = data,
      exposure = exposure,
      outcome = outcome,
      confounders = confounders,
      x_model_coefs = x_model_coefs,
      y_model_coefs = y_model_coefs
    )
  } else if (!is.null(x1y0_model_coefs)) {
    final <- em_om_multinom(
      data = data,
      exposure = exposure,
      outcome = outcome,
      confounders = confounders,
      x1y0_model_coefs,
      x0y1_model_coefs,
      x1y1_model_coefs
    )
  }

  est <- summary(final)$coef[2, 1]
  se <- summary(final)$coef[2, 2]
  alpha <- 1 - level

  estimate <- exp(est)
  ci <- c(
    exp(est + se * qnorm(alpha / 2)),
    exp(est + se * qnorm(1 - alpha / 2))
  )

  return(list(estimate = estimate, ci = ci))
}
