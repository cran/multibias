#' Simulated data with uncontrolled confounding and outcome misclassification
#'
#' Data containing two sources of bias, three known confounders, and
#' 100,000 observations. This data is obtained from `df_uc_om_source`
#' by removing the columns *Y* and *U*. The resulting data
#' corresponds to what a researcher would see in the real-world: a
#' misclassified outcome, *Ystar*, and missing data on the binary
#' confounder *U*. As seen in `df_uc_omc_source`, the true, unbiased
#' exposure-outcome odds ratio = 2.
#'
#' @format A dataframe with 100,000 rows and 5 columns:
#' \describe{
#'     \item{X}{exposure, 1 = present and 0 = absent}
#'     \item{Ystar}{misclassified outcome, 1 = present and 0 = absent}
#'     \item{C1}{1st confounder, 1 = present and 0 = absent}
#'     \item{C2}{2nd confounder, 1 = present and 0 = absent}
#'     \item{C3}{3rd confounder, 1 = present and 0 = absent}
#' }
"df_uc_om"

#' Data source for `df_uc_om`
#'
#' Data with complete information on the two sources of bias, three known
#' confounders, and 100,000 observations. This data is used to derive
#' `df_uc_om` and can be used to obtain bias parameters for purposes
#' of validating the simultaneous multi-bias adjustment method with
#' `df_uc_om`. With this source data, the fitted regression
#' \ifelse{html}{\out{logit(P(Y=1)) = &alpha;<sub>0</sub> + &alpha;<sub>1</sub>X + &alpha;<sub>2</sub>C1 + &alpha;<sub>3</sub>U}}{\eqn{logit(P(Y=1)) = \alpha_0 + \alpha_1 X + \alpha_2 C1 + \alpha_3 U}}
#' shows that the true, unbiased exposure-outcome odds ratio = 2.
#'
#' @format A dataframe with 100,000 rows and 7 columns:
#' \describe{
#'     \item{X}{exposure, 1 = present and 0 = absent}
#'     \item{Y}{outcome, 1 = present and 0 = absent}
#'     \item{C1}{1st confounder, 1 = present and 0 = absent}
#'     \item{C2}{2nd confounder, 1 = present and 0 = absent}
#'     \item{C3}{3rd confounder, 1 = present and 0 = absent}
#'     \item{U}{unmeasured confounder, 1 = present and 0 = absent}
#'     \item{Ystar}{misclassified outcome, 1 = present and 0 = absent}
#' }
"df_uc_om_source"
