#' Simulated data with uncontrolled confounding
#'
#' Data containing one source of bias, three known confounders, and
#'  100,000 observations. This data is obtained from \code{df_uc_source}
#'  by removing the column U. The resulting data corresponds to
#'  what a researcher would see in the real-world: information on known
#'  confounders (C1, C2, and C3), but nothing for confounder U.
#'  As seen in \code{df_uc_source}, the true, unbiased exposure-outcome
#'  odds ratio = 2.
#'
#' @format A dataframe with 100,000 rows and 5 columns:
#' \describe{
#'     \item{X}{exposure, 1 = present and 0 = absent}
#'     \item{Y}{outcome, 1 = present and 0 = absent}
#'     \item{C1}{1st confounder, 1 = present and 0 = absent}
#'     \item{C2}{2nd confounder, 1 = present and 0 = absent}
#'     \item{C3}{3rd confounder, 1 = present and 0 = absent}
#' }
"df_uc"