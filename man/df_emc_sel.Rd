% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df_emc_sel.R
\docType{data}
\name{df_emc_sel}
\alias{df_emc_sel}
\title{Simulated data with exposure misclassification and selection bias}
\format{
A dataframe with 100,000 rows and 5 columns:
\describe{
    \item{Xstar}{misclassified exposure, 1 = present and 0 = absent}
    \item{Y}{outcome, 1 = present and 0 = absent}
    \item{C1}{1st confounder, 1 = present and 0 = absent}
    \item{C2}{2nd confounder, 1 = present and 0 = absent}
    \item{C3}{3rd confounder, 1 = present and 0 = absent}
}
}
\usage{
df_emc_sel
}
\description{
Data containing two sources of bias, three known confounders, and
 100,000 observations. This data is obtained by sampling with replacement
 with probability = S from \code{df_emc_sel_source} then removing the
 columns X and S. The resulting data corresponds to what a researcher
 would see in the real-world: a misclassified exposure, Xstar,
 and missing data for those not selected into the study
 (S=0). As seen in \code{df_emc_sel_source}, the true, unbiased
 exposure-outcome odds ratio = 2.
}
\keyword{datasets}
