#' Adust for outcome misclassification.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `adjust_omc()` was renamed to `adjust_om()`
#' @keywords internal
#'
#' @export
adjust_omc <- function(
    data,
    exposure,
    outcome,
    confounders = NULL,
    y_model_coefs,
    level = 0.95) {
  lifecycle::deprecate_warn("1.5.3", "adjust_omc()", "adjust_om()")
  adjust_om(data, exposure, outcome, confounders, y_model_coefs, level)
}


#' Adust for outcome misclassification.
#'
#' `adjust_om` returns the exposure-outcome odds ratio and confidence
#' interval, adjusted for outcome misclassificaiton.
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
#' @param y_model_coefs The regression coefficients corresponding to the model:
#' \ifelse{html}{\out{logit(P(Y=1)) = &delta;<sub>0</sub> + &delta;<sub>1</sub>X + &delta;<sub>2</sub>Y* + &delta;<sub>2+j</sub>C<sub>j</sub>, }}{\eqn{logit(P(Y=1)) = \_delta_0 + \_delta_1 X + \_delta_2 Y^* + \_delta_{2+j} C_j, }}
#' where *Y* represents the binary true outcome, *X* is the exposure,
#' *Y** is the binary misclassified outcome,
#' *C* represents the vector of measured confounders (if any),
#' and *j* corresponds to the number of measured confounders. The number
#' of parameters is therefore 3 + *j*.
#' @return A list where the first item is the odds ratio estimate of the
#' effect of the exposure on the outcome and the second item is the
#' confidence interval as the vector: (lower bound, upper bound).
#'
#' @examples
#' adjust_om(
#'   evans,
#'   exposure = "SMK",
#'   outcome = "CHD",
#'   confounders = "HPT",
#'   y_model_coefs = c(qlogis(0.01), log(1.5), log(5), log(1.5))
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

adjust_om <- function(
    data,
    exposure,
    outcome,
    confounders = NULL,
    y_model_coefs,
    level = 0.95) {
  n <- nrow(data)
  len_c <- length(confounders)
  len_y_coefs <- length(y_model_coefs)

  x <- data[, exposure]
  ystar <- data[, outcome]

  force_binary(ystar, "Outcome must be a binary integer.")
  force_len(
    len_y_coefs,
    3 + len_c,
    paste0(
      "Incorrect length of Y model coefficients. ",
      "Length should equal 3 + number of confounders."
    )
  )

  y1_0 <- y_model_coefs[1]
  y1_x <- y_model_coefs[2]
  y1_ystar <- y_model_coefs[3]

  if (is.null(confounders)) {
    df <- data.frame(X = x, Ystar = ystar)
    df$Ypred <- rbinom(n, 1, plogis(y1_0 + y1_x * df$X + y1_ystar * df$Ystar))

    final <- glm(
      Ypred ~ X,
      family = binomial(link = "logit"),
      data = df
    )
  } else if (len_c == 1) {
    c1 <- data[, confounders]
    df <- data.frame(X = x, Ystar = ystar, C1 = c1)

    y1_c1 <- y_model_coefs[4]

    df$Ypred <- rbinom(
      n, 1, plogis(
        y1_0 + y1_x * df$X + y1_ystar * df$Ystar +
          y1_c1 * df$C1
      )
    )

    final <- glm(
      Ypred ~ X + C1,
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

    final <- glm(
      Ypred ~ X + C1 + C2,
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
      n, 1, plogis(
        y1_0 + y1_x * df$X + y1_ystar * df$Ystar +
          y1_c1 * df$C1 + y1_c2 * df$C2 + y1_c3 * df$C3
      )
    )

    final <- glm(
      Ypred ~ X + C1 + C2 + C3,
      family = binomial(link = "logit"),
      data = df
    )
  } else if (len_c > 3) {
    stop("This function is currently not compatible with >3 confounders.")
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
