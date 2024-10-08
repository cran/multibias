#' Adust for uncontrolled confounding and outcome misclassification.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `adjust_uc_omc_sel()` was renamed to `adjust_uc_om_sel()`
#' @keywords internal
#'
#' @export
adjust_uc_omc <- function(
    data,
    exposure,
    outcome,
    confounders = NULL,
    u_model_coefs = NULL,
    y_model_coefs = NULL,
    u0y1_model_coefs = NULL,
    u1y0_model_coefs = NULL,
    u1y1_model_coefs = NULL,
    level = 0.95) {
  lifecycle::deprecate_warn(
    "1.5.3", "adjust_uc_omc()", "adjust_uc_om()"
  )
  adjust_uc_om(
    data,
    exposure,
    outcome,
    confounders,
    u_model_coefs,
    y_model_coefs,
    u0y1_model_coefs,
    u1y0_model_coefs,
    u1y1_model_coefs,
    level
  )
}


# bias adjust with u_model_coefs and y_model_coefs

uc_om_single <- function(
    data,
    exposure,
    outcome,
    confounders,
    u_model_coefs,
    y_model_coefs) {
  n <- nrow(data)
  len_c <- length(confounders)
  len_u_coefs <- length(u_model_coefs)
  len_y_coefs <- length(y_model_coefs)

  force_len(
    len_u_coefs,
    3,
    paste0(
      "Incorrect length of U model coefficients. ",
      "Length should equal 3."
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

  x <- data[, exposure]
  ystar <- data[, outcome]

  u1_0 <- u_model_coefs[1]
  u1_x <- u_model_coefs[2]
  u1_y <- u_model_coefs[3]

  y1_0 <- y_model_coefs[1]
  y1_x <- y_model_coefs[2]
  y1_ystar <- y_model_coefs[3]

  if (is.null(confounders)) {
    df <- data.frame(X = x, Ystar = ystar)
    df$Ypred <- rbinom(n, 1, plogis(y1_0 + y1_x * df$X + y1_ystar * df$Ystar))
    df$Upred <- rbinom(n, 1, plogis(u1_0 + u1_x * df$X + u1_y * df$Ypred))

    final <- glm(
      Ypred ~ X + Upred,
      family = binomial(link = "logit"),
      data = df
    )
  } else if (len_c == 1) {
    c1 <- data[, confounders]
    df <- data.frame(X = x, Ystar = ystar, C1 = c1)

    y1_c1 <- y_model_coefs[4]

    df$Ypred <- rbinom(
      n, 1, plogis(
        y1_0 + y1_x * df$X + y1_ystar * df$Ystar + y1_c1 * df$C1
      )
    )
    df$Upred <- rbinom(n, 1, plogis(u1_0 + u1_x * df$X + u1_y * df$Ypred))

    final <- glm(
      Ypred ~ X + C1 + Upred,
      family = binomial(link = "logit"),
      data = df
    )
  } else if (len_c == 2) {
    c1 <- data[, confounders[1]]
    c2 <- data[, confounders[2]]

    df <- data.frame(X = x, Ystar = ystar, C1 = c1, C2 = c2)

    y1_c1 <- y_model_coefs[4]
    y1_c2 <- y_model_coefs[5]

    df$Ypred <- rbinom(
      n, 1, plogis(
        y1_0 + y1_x * df$X + y1_ystar * df$Ystar +
          y1_c1 * df$C1 + y1_c2 * df$C2
      )
    )
    df$Upred <- rbinom(n, 1, plogis(u1_0 + u1_x * df$X + u1_y * df$Ypred))

    final <- glm(
      Ypred ~ X + C1 + C2 + Upred,
      family = binomial(link = "logit"),
      data = df
    )
  } else if (len_c == 3) {
    c1 <- data[, confounders[1]]
    c2 <- data[, confounders[2]]
    c3 <- data[, confounders[3]]

    df <- data.frame(X = x, Ystar = ystar, C1 = c1, C2 = c2, C3 = c3)

    y1_c1 <- y_model_coefs[4]
    y1_c2 <- y_model_coefs[5]
    y1_c3 <- y_model_coefs[6]

    df$Ypred <- rbinom(
      n, 1,
      plogis(
        y1_0 + y1_x * df$X + y1_ystar * df$Ystar +
          y1_c1 * df$C1 + y1_c2 * df$C2 + y1_c3 * df$C3
      )
    )
    df$Upred <- rbinom(n, 1, plogis(u1_0 + u1_x * df$X + u1_y * df$Ypred))

    final <- glm(
      Ypred ~ X + C1 + C2 + C3 + Upred,
      family = binomial(link = "logit"),
      data = df
    )
  } else if (len_c > 3) {
    stop("This function is currently not compatible with >3 confounders.")
  }

  return(final)
}


# bias adjust with multinomial coefs

uc_om_multinom <- function(
    data,
    exposure,
    outcome,
    confounders,
    u1y0_model_coefs,
    u0y1_model_coefs,
    u1y1_model_coefs) {
  n <- nrow(data)
  len_c <- length(confounders)
  len_u1y0_coefs <- length(u1y0_model_coefs)
  len_u0y1_coefs <- length(u0y1_model_coefs)
  len_u1y1_coefs <- length(u1y1_model_coefs)

  force_len(
    len_u1y0_coefs,
    3 + len_c,
    paste0(
      "Incorrect length of U1Y0 model coefficients. ",
      "Length should equal 3 + number of confounders."
    )
  )
  force_len(
    len_u0y1_coefs,
    3 + len_c,
    paste0(
      "Incorrect length of U0Y1 model coefficients. ",
      "Length should equal 3 + number of confounders."
    )
  )
  force_len(
    len_u1y1_coefs,
    3 + len_c,
    paste0(
      "Incorrect length of U1Y1 model coefficients. ",
      "Length should equal 3 + number of confounders."
    )
  )

  x <- data[, exposure]
  ystar <- data[, outcome]

  u1y0_0 <- u1y0_model_coefs[1]
  u1y0_x <- u1y0_model_coefs[2]
  u1y0_ystar <- u1y0_model_coefs[3]

  u0y1_0 <- u0y1_model_coefs[1]
  u0y1_x <- u0y1_model_coefs[2]
  u0y1_ystar <- u0y1_model_coefs[3]

  u1y1_0 <- u1y1_model_coefs[1]
  u1y1_x <- u1y1_model_coefs[2]
  u1y1_ystar <- u1y1_model_coefs[3]

  if (is.null(confounders)) {
    df <- data.frame(X = x, Ystar = ystar)

    p_u1y0 <- exp(u1y0_0 + u1y0_x * df$X + u1y0_ystar * df$Ystar)
    p_u0y1 <- exp(u0y1_0 + u0y1_x * df$X + u0y1_ystar * df$Ystar)
    p_u1y1 <- exp(u1y1_0 + u1y1_x * df$X + u1y1_ystar * df$Ystar)

    denom <- (1 + p_u1y0 + p_u0y1 + p_u1y1)

    u0y0_pred <- 1 / denom
    u1y0_pred <- p_u1y0 / denom
    u0y1_pred <- p_u0y1 / denom
    u1y1_pred <- p_u1y1 / denom

    df_uy_pred <- data.frame(
      U0Y0 = u0y0_pred,
      U1Y0 = u1y0_pred,
      U0Y1 = u0y1_pred,
      U1Y1 = u1y1_pred
    )
    df_uy_pred4 <- bind_rows(df_uy_pred, df_uy_pred, df_uy_pred, df_uy_pred)

    combined <- bind_rows(df, df, df, df) %>%
      bind_cols(df_uy_pred4) %>%
      mutate(
        Ubar = rep(c(1, 0, 1, 0), each = n),
        Ybar = rep(c(1, 1, 0, 0), each = n),
        pUY = case_when(
          Ubar == 0 & Ybar == 0 ~ U0Y0,
          Ubar == 1 & Ybar == 0 ~ U1Y0,
          Ubar == 0 & Ybar == 1 ~ U0Y1,
          Ubar == 1 & Ybar == 1 ~ U1Y1
        )
      )
    suppressWarnings({
      final <- glm(
        Ybar ~ X + Ubar,
        family = binomial(link = "logit"),
        weights = combined$pUY,
        data = combined
      )
    })
  } else if (len_c == 1) {
    c1 <- data[, confounders]

    df <- data.frame(X = x, Ystar = ystar, C1 = c1)

    u1y0_c1 <- u1y0_model_coefs[4]
    u0y1_c1 <- u0y1_model_coefs[4]
    u1y1_c1 <- u1y1_model_coefs[4]

    p_u1y0 <- exp(
      u1y0_0 + u1y0_x * df$X + u1y0_ystar * df$Ystar +
        u1y0_c1 * df$C1
    )
    p_u0y1 <- exp(
      u0y1_0 + u0y1_x * df$X + u0y1_ystar * df$Ystar +
        u0y1_c1 * df$C1
    )
    p_u1y1 <- exp(
      u1y1_0 + u1y1_x * df$X + u1y1_ystar * df$Ystar +
        u1y1_c1 * df$C1
    )

    denom <- (1 + p_u1y0 + p_u0y1 + p_u1y1)

    u0y0_pred <- 1 / denom
    u1y0_pred <- p_u1y0 / denom
    u0y1_pred <- p_u0y1 / denom
    u1y1_pred <- p_u1y1 / denom

    df_uy_pred <- data.frame(
      U0Y0 = u0y0_pred,
      U1Y0 = u1y0_pred,
      U0Y1 = u0y1_pred,
      U1Y1 = u1y1_pred
    )
    df_uy_pred4 <- bind_rows(df_uy_pred, df_uy_pred, df_uy_pred, df_uy_pred)

    combined <- bind_rows(df, df, df, df) %>%
      bind_cols(df_uy_pred4) %>%
      mutate(
        Ubar = rep(c(1, 0, 1, 0), each = n),
        Ybar = rep(c(1, 1, 0, 0), each = n),
        pUY = case_when(
          Ubar == 0 & Ybar == 0 ~ U0Y0,
          Ubar == 1 & Ybar == 0 ~ U1Y0,
          Ubar == 0 & Ybar == 1 ~ U0Y1,
          Ubar == 1 & Ybar == 1 ~ U1Y1
        )
      )

    suppressWarnings({
      final <- glm(
        Ybar ~ X + C1 + Ubar,
        family = binomial(link = "logit"),
        weights = combined$pUY,
        data = combined
      )
    })
  } else if (len_c == 2) {
    c1 <- data[, confounders[1]]
    c2 <- data[, confounders[2]]

    df <- data.frame(X = x, Ystar = ystar, C1 = c1, C2 = c2)

    u1y0_c1 <- u1y0_model_coefs[4]
    u1y0_c2 <- u1y0_model_coefs[5]

    u0y1_c1 <- u0y1_model_coefs[4]
    u0y1_c2 <- u0y1_model_coefs[5]

    u1y1_c1 <- u1y1_model_coefs[4]
    u1y1_c2 <- u1y1_model_coefs[5]

    p_u1y0 <- exp(
      u1y0_0 + u1y0_x * df$X + u1y0_ystar * df$Ystar +
        u1y0_c1 * df$C1 + u1y0_c2 * df$C2
    )
    p_u0y1 <- exp(
      u0y1_0 + u0y1_x * df$X + u0y1_ystar * df$Ystar +
        u0y1_c1 * df$C1 + u0y1_c2 * df$C2
    )
    p_u1y1 <- exp(
      u1y1_0 + u1y1_x * df$X + u1y1_ystar * df$Ystar +
        u1y1_c1 * df$C1 + u1y1_c2 * df$C2
    )

    denom <- (1 + p_u1y0 + p_u0y1 + p_u1y1)

    u0y0_pred <- 1 / denom
    u1y0_pred <- p_u1y0 / denom
    u0y1_pred <- p_u0y1 / denom
    u1y1_pred <- p_u1y1 / denom

    df_uy_pred <- data.frame(
      U0Y0 = u0y0_pred,
      U1Y0 = u1y0_pred,
      U0Y1 = u0y1_pred,
      U1Y1 = u1y1_pred
    )
    df_uy_pred4 <- bind_rows(df_uy_pred, df_uy_pred, df_uy_pred, df_uy_pred)

    combined <- bind_rows(df, df, df, df) %>%
      bind_cols(df_uy_pred4) %>%
      mutate(
        Ubar = rep(c(1, 0, 1, 0), each = n),
        Ybar = rep(c(1, 1, 0, 0), each = n),
        pUY = case_when(
          Ubar == 0 & Ybar == 0 ~ U0Y0,
          Ubar == 1 & Ybar == 0 ~ U1Y0,
          Ubar == 0 & Ybar == 1 ~ U0Y1,
          Ubar == 1 & Ybar == 1 ~ U1Y1
        )
      )

    suppressWarnings({
      final <- glm(
        Ybar ~ X + C1 + C2 + Ubar,
        family = binomial(link = "logit"),
        weights = combined$pUY,
        data = combined
      )
    })
  } else if (len_c == 3) {
    c1 <- data[, confounders[1]]
    c2 <- data[, confounders[2]]
    c3 <- data[, confounders[3]]

    df <- data.frame(X = x, Ystar = ystar, C1 = c1, C2 = c2, C3 = c3)

    u1y0_c1 <- u1y0_model_coefs[4]
    u1y0_c2 <- u1y0_model_coefs[5]
    u1y0_c3 <- u1y0_model_coefs[6]

    u0y1_c1 <- u0y1_model_coefs[4]
    u0y1_c2 <- u0y1_model_coefs[5]
    u0y1_c3 <- u0y1_model_coefs[6]

    u1y1_c1 <- u1y1_model_coefs[4]
    u1y1_c2 <- u1y1_model_coefs[5]
    u1y1_c3 <- u1y1_model_coefs[6]

    p_u1y0 <- exp(
      u1y0_0 + u1y0_x * df$X + u1y0_ystar * df$Ystar +
        u1y0_c1 * df$C1 + u1y0_c2 * df$C2 + u1y0_c3 * df$C3
    )
    p_u0y1 <- exp(
      u0y1_0 + u0y1_x * df$X + u0y1_ystar * df$Ystar +
        u0y1_c1 * df$C1 + u0y1_c2 * df$C2 + u0y1_c3 * df$C3
    )
    p_u1y1 <- exp(
      u1y1_0 + u1y1_x * df$X + u1y1_ystar * df$Ystar +
        u1y1_c1 * df$C1 + u1y1_c2 * df$C2 + u1y1_c3 * df$C3
    )

    denom <- (1 + p_u1y0 + p_u0y1 + p_u1y1)

    u0y0_pred <- 1 / denom
    u1y0_pred <- p_u1y0 / denom
    u0y1_pred <- p_u0y1 / denom
    u1y1_pred <- p_u1y1 / denom

    df_uy_pred <- data.frame(
      U0Y0 = u0y0_pred,
      U1Y0 = u1y0_pred,
      U0Y1 = u0y1_pred,
      U1Y1 = u1y1_pred
    )
    df_uy_pred4 <- bind_rows(df_uy_pred, df_uy_pred, df_uy_pred, df_uy_pred)

    combined <- bind_rows(df, df, df, df) %>%
      bind_cols(df_uy_pred4) %>%
      mutate(
        Ubar = rep(c(1, 0, 1, 0), each = n),
        Ybar = rep(c(1, 1, 0, 0), each = n),
        pUY = case_when(
          Ubar == 0 & Ybar == 0 ~ U0Y0,
          Ubar == 1 & Ybar == 0 ~ U1Y0,
          Ubar == 0 & Ybar == 1 ~ U0Y1,
          Ubar == 1 & Ybar == 1 ~ U1Y1
        )
      )

    suppressWarnings({
      final <- glm(
        Ybar ~ X + C1 + C2 + C3 + Ubar,
        family = binomial(link = "logit"),
        weights = combined$pUY,
        data = combined
      )
    })
  } else if (len_c > 3) {
    stop("This function is currently not compatible with >3 confounders.")
  }

  return(final)
}


#' Adust for uncontrolled confounding and outcome misclassification.
#'
#' `adjust_uc_om` returns the exposure-outcome odds ratio and confidence
#' interval, adjusted for uncontrolled confounding and outcome
#' misclassificaiton. Two different options for the bias parameters are
#' available here: 1) parameters from separate models of *U* and *Y*
#' (`u_model_coefs` and `y_model_coefs`) or 2) parameters from
#' a joint model of *U* and *Y* (`u1y0_model_coefs`,
#' `u0y1_model_coefs`, and `u1y1_model_coefs`).
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
#' @inheritParams adjust_em_sel
#' @param u_model_coefs The regression coefficients corresponding to the model:
#' \ifelse{html}{\out{logit(P(U=1)) = &alpha;<sub>0</sub> + &alpha;<sub>1</sub>X + &alpha;<sub>2</sub>Y, }}{\eqn{logit(P(U=1)) = \alpha_0 + \alpha_1 X + \alpha_2 Y, }}
#' where *U* is the binary unmeasured confounder, *X* is the
#' exposure, *Y* is the binary true outcome. The number of parameters
#' therefore equals 3.
#' @param y_model_coefs The regression coefficients corresponding to the model:
#' \ifelse{html}{\out{logit(P(Y=1)) = &delta;<sub>0</sub> + &delta;<sub>1</sub>X + &delta;<sub>2</sub>Y* + &delta;<sub>2+j</sub>C<sub>j</sub>, }}{\eqn{logit(P(Y=1)) = \delta_0 + \delta_1 X + \delta_2 Y^* + \delta_{2+j} C_j, }}
#' where *Y* represents binary true outcome, *X* is the exposure,
#' *Y** is the binary misclassified outcome,
#' *C* represents the vector of measured confounders (if any),
#' and *j* corresponds to the number of measured confounders.
#' The number of parameters therefore equals 3 + *j*.
#' @param u1y0_model_coefs The regression coefficients corresponding to the
#' model:
#' \ifelse{html}{\out{log(P(U=1,Y=0)/P(U=0,Y=0)) = &gamma;<sub>1,0</sub> + &gamma;<sub>1,1</sub>X + &gamma;<sub>1,2</sub>Y* + &gamma;<sub>1,2+j</sub>C<sub>j</sub>, }}{\eqn{log(P(U=1,Y=0)/P(U=0,Y=0)) = \gamma_{1,0} + \gamma_{1,1} X + \gamma_{1,2} Y^* + \gamma_{1,2+j} C_j, }}
#' where *U* is the binary unmeasured confounder, *Y* is the
#' binary true outcome, *X* is the exposure, *Y** is the binary
#' misclassified outcome, *C* represents the vector of measured
#' confounders (if any), and *j* corresponds to the number of
#' measured confounders.
#' @param u0y1_model_coefs The regression coefficients corresponding to the
#' model:
#' \ifelse{html}{\out{log(P(U=0,Y=1)/P(U=0,Y=0)) = &gamma;<sub>2,0</sub> + &gamma;<sub>2,1</sub>X + &gamma;<sub>2,2</sub>Y* + &gamma;<sub>2,2+j</sub>C<sub>j</sub>, }}{\eqn{log(P(U=0,Y=1)/P(U=0,Y=0)) = \gamma_{2,0} + \gamma_{2,1} X + \gamma_{2,2} Y^* + \gamma_{2,2+j} C_j,}}
#' where *U* is the binary unmeasured confounder, *Y* is the
#' binary true outcome, *X* is the exposure, *Y** is the binary
#' misclassified outcome, *C* represents the vector of measured
#' confounders (if any), and *j* corresponds to the number of
#' measured confounders.
#' @param u1y1_model_coefs The regression coefficients corresponding to the
#' model:
#' \ifelse{html}{\out{log(P(U=1,Y=1)/P(U=0,Y=0)) = &gamma;<sub>3,0</sub> + &gamma;<sub>3,1</sub>X + &gamma;<sub>3,2</sub>Y* + &gamma;<sub>3,2+j</sub>C<sub>j</sub>, }}{\eqn{log(P(U=1,Y=1)/P(U=0,Y=0)) = \gamma_{3,0} + \gamma_{3,1} X + \gamma_{3,2} Y^* + \gamma_{3,2+j} C_j,}}
#' where *U* is the binary unmeasured confounder, *Y* is the
#' binary true outcome, *X* is the exposure, *Y** is the binary
#' misclassified outcome, *C* represents the vector of measured
#' confounders (if any), and *j* corresponds to the number of
#' measured confounders.
#' @return A list where the first item is the odds ratio estimate of the
#' effect of the exposure on the outcome and the second item is the
#' confidence interval as the vector: (lower bound, upper bound).
#'
#' @examples
#' # Using u_model_coefs and y_model_coefs -------------------------------------
#' adjust_uc_om(
#'   df_uc_om,
#'   exposure = "X",
#'   outcome = "Ystar",
#'   confounders = "C1",
#'   u_model_coefs = c(-0.22, 0.61, 0.70),
#'   y_model_coefs = c(-2.85, 0.73, 1.60, 0.38)
#' )
#'
#' # Using u1y0_model_coefs, u0y1_model_coefs, u1y1_model_coefs ----------------
#' adjust_uc_om(
#'   df_uc_om,
#'   exposure = "X",
#'   outcome = "Ystar",
#'   confounders = "C1",
#'   u1y0_model_coefs = c(-0.19, 0.61, 0.00, -0.07),
#'   u0y1_model_coefs = c(-3.21, 0.60, 1.60, 0.36),
#'   u1y1_model_coefs = c(-2.72, 1.24, 1.59, 0.34)
#' )
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom stats binomial
#' @importFrom stats glm
#' @importFrom stats qnorm
#' @importFrom stats rbinom
#' @importFrom stats plogis
#' @importFrom rlang .data
#'
#' @export

adjust_uc_om <- function(
    data,
    exposure,
    outcome,
    confounders = NULL,
    u_model_coefs = NULL,
    y_model_coefs = NULL,
    u1y0_model_coefs = NULL,
    u0y1_model_coefs = NULL,
    u1y1_model_coefs = NULL,
    level = 0.95) {
  n <- nrow(data)
  len_c <- length(confounders)

  x <- data[, exposure]
  ystar <- data[, outcome]

  force_binary(ystar, "Outcome must be a binary integer.")

  # check that user correctly specified bias parameters
  if (
    !(
      (is.null(u_model_coefs) && is.null(y_model_coefs)) ||
        ((is.null(u1y0_model_coefs) && is.null(u0y1_model_coefs) &&
          is.null(u1y1_model_coefs)))
    )
  ) {
    stop(
      "Bias parameters must be specified for:\n
       1) u_model_coefs and y_model_coefs OR\n
       2) u1y0_model_coefs, u0y1_model_coefs, and u1y1_model_coefs."
    )
  }

  if (!is.null(u_model_coefs)) {
    final <- uc_om_single(
      data = data,
      exposure = exposure,
      outcome = outcome,
      confounders = confounders,
      u_model_coefs = u_model_coefs,
      y_model_coefs = y_model_coefs
    )
  } else if (!is.null(u1y0_model_coefs)) {
    final <- uc_om_multinom(
      data = data,
      exposure = exposure,
      outcome = outcome,
      confounders = confounders,
      u1y0_model_coefs = u1y0_model_coefs,
      u0y1_model_coefs = u0y1_model_coefs,
      u1y1_model_coefs = u1y1_model_coefs
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
