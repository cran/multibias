#' Adust for uncontrolled confounding and exposure misclassification.
#'
#' \code{adjust_uc_emc} returns the exposure-outcome odds ratio and confidence
#' interval, adjusted for uncontrolled confounding and exposure
#' misclassificaiton.
#'
#' This function uses two separate logistic regression models to predict the
#' uncontrolled confounder (U) and exposure (X). If a single bias model for
#' jointly modeling X and U is desired use \code{adjust_multinom_uc_emc}.
#'
#' Values for the regression coefficients can be applied as
#' fixed values or as single draws from a probability
#' distribution (ex: \code{rnorm(1, mean = 2, sd = 1)}). The latter has
#' the advantage of allowing the researcher to capture the uncertainty
#' in the bias parameter estimates. To incorporate this uncertainty in the
#' estimate and confidence interval, this function should be run in loop across
#' bootstrap samples of the dataframe for analysis. The estimate and
#' confidence interval would then be obtained from the median and quantiles
#' of the distribution of odds ratio estimates.
#'
#' @inheritParams adjust_emc_sel
#' @param u_model_coefs The regression coefficients corresponding to the model:
#'  \ifelse{html}{\out{logit(P(U=1)) = &alpha;<sub>0</sub> + &alpha;<sub>1</sub>X + &alpha;<sub>2</sub>Y, }}{\eqn{logit(P(U=1)) = \alpha_0 + \alpha_1 X + \alpha_2 Y, }}
#'  where U is the binary unmeasured confounder, X is the binary true
#'  exposure, Y is the binary outcome. The number of parameters therefore
#'  equals 3.
#' @param x_model_coefs The regression coefficients corresponding to the model:
#'  \ifelse{html}{\out{logit(P(X=1)) = &delta;<sub>0</sub> + &delta;<sub>1</sub>X* + &delta;<sub>2</sub>Y + &delta;<sub>2+j</sub>C<sub>j</sub>, }}{\eqn{logit(P(X=1)) = \delta_0 + \delta_1 X^* + \delta_2 Y + \delta_{2+j} C_j, }}
#'  where X represents the binary true exposure,
#'  X* is the binary misclassified exposure, Y is the binary
#'  outcome, C represents the vector of binary measured confounders (if any),
#'  and j corresponds to the number of measured confounders. The number of
#'  parameters therefore equals 3 + j.
#' @return A list where the first item is the odds ratio estimate of the
#'  effect of the exposure on the outcome and the second item is the
#'  confidence interval as the vector: (lower bound, upper bound).
#'
#' @examples
#' adjust_uc_emc(
#'   df_uc_emc,
#'   exposure = "Xstar",
#'   outcome = "Y",
#'   confounders = "C1",
#'   u_model_coefs = c(-0.23, 0.63, 0.66),
#'   x_model_coefs = c(-2.47, 1.62, 0.73, 0.32)
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

adjust_uc_emc <- function(
  data,
  exposure,
  outcome,
  confounders = NULL,
  u_model_coefs,
  x_model_coefs,
  level = 0.95
) {

  n <- nrow(data)
  len_c <- length(confounders)
  len_u_coefs <- length(u_model_coefs)
  len_x_coefs <- length(x_model_coefs)

  xstar <- data[, exposure]
  y <- data[, outcome]

  if (sum(xstar %in% c(0, 1)) != n) {
    stop("Exposure must be a binary integer.")
  }
  if (sum(y %in% c(0, 1)) != n) {
    stop("Outcome must be a binary integer.")
  }
  if (len_u_coefs != 3) {
    stop("Incorrect length of U model coefficients. Length should equal 3.")
  }
  if (len_x_coefs != 3 + len_c) {
    stop(
      paste0(
        "Incorrect length of X model coefficients. ",
        "Length should equal 3 + number of confounders."
      )
    )
  }

  u1_0     <- u_model_coefs[1]
  u1_x     <- u_model_coefs[2]
  u1_y     <- u_model_coefs[3]

  x1_0     <- x_model_coefs[1]
  x1_xstar <- x_model_coefs[2]
  x1_y     <- x_model_coefs[3]

  if (is.null(confounders)) {

    df <- data.frame(Xstar = xstar, Y = y)
    df$Xpred <- rbinom(n, 1, plogis(x1_0 + x1_xstar * df$Xstar + x1_y * df$Y))
    df$Upred <- rbinom(n, 1, plogis(u1_0 + u1_x * df$Xpred + u1_y * df$Y))

    final <- glm(
      Y ~ Xpred + Upred,
      family = binomial(link = "logit"),
      data = df
    )

  } else if (len_c == 1) {

    c1 <- data[, confounders]
    df <- data.frame(Xstar = xstar, Y = y, C1 = c1)

    x1_c1 <- x_model_coefs[4]

    df$Xpred <- rbinom(n, 1, plogis(x1_0 + x1_xstar * df$Xstar +
                                      x1_y * df$Y + x1_c1 * df$C1))
    df$Upred <- rbinom(n, 1, plogis(u1_0 + u1_x * df$Xpred + u1_y * df$Y))

    final <- glm(
      Y ~ Xpred + C1 + Upred,
      family = binomial(link = "logit"),
      data = df
    )

  } else if (len_c == 2) {

    c1 <- data[, confounders[1]]
    c2 <- data[, confounders[2]]

    df <- data.frame(Xstar = xstar, Y = y, C1 = c1, C2 = c2)

    x1_c1 <- x_model_coefs[4]
    x1_c2 <- x_model_coefs[5]

    df$Xpred <- rbinom(n, 1, plogis(x1_0 + x1_xstar * df$Xstar + x1_y * df$Y +
                                      x1_c1 * df$C1 + x1_c2 * df$C2))
    df$Upred <- rbinom(n, 1, plogis(u1_0 + u1_x * df$Xpred + u1_y * df$Y))

    final <- glm(
      Y ~ Xpred + C1 + C2 + Upred,
      family = binomial(link = "logit"),
      data = df
    )

  } else if (len_c == 3) {

    c1 <- data[, confounders[1]]
    c2 <- data[, confounders[2]]
    c3 <- data[, confounders[3]]

    df <- data.frame(Xstar = xstar, Y = y, C1 = c1, C2 = c2, C3 = c3)

    x1_c1 <- x_model_coefs[4]
    x1_c2 <- x_model_coefs[5]
    x1_c3 <- x_model_coefs[6]

    df$Xpred <- rbinom(
      n, 1,
      plogis(
        x1_0 + x1_xstar * df$Xstar + x1_y * df$Y +
          x1_c1 * df$C1 + x1_c2 * df$C2 + x1_c3 * df$C3
      )
    )
    df$Upred <- rbinom(n, 1, plogis(u1_0 + u1_x * df$Xpred + u1_y * df$Y))

    final <- glm(
      Y ~ Xpred + C1 + C2 + C3 + Upred,
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
  ci <- c(exp(est + se * qnorm(alpha / 2)),
          exp(est + se * qnorm(1 - alpha / 2)))
  return(list(estimate = estimate, ci = ci))

}