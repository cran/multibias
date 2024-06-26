% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adjust_multinom_uc_emc.R
\name{adjust_multinom_uc_emc}
\alias{adjust_multinom_uc_emc}
\title{Adust for uncontrolled confounding and exposure misclassification.}
\usage{
adjust_multinom_uc_emc(
  data,
  exposure,
  outcome,
  confounders = NULL,
  x1u0_model_coefs,
  x0u1_model_coefs,
  x1u1_model_coefs,
  level = 0.95
)
}
\arguments{
\item{data}{Dataframe for analysis.}

\item{exposure}{String name of the exposure variable.}

\item{outcome}{String name of the outcome variable.}

\item{confounders}{String name(s) of the confounder(s).
A maximum of three confounders are allowed.}

\item{x1u0_model_coefs}{The regression coefficients corresponding to the
model:
\ifelse{html}{\out{log(P(X=1,U=0)/P(X=0,U=0)) = &gamma;<sub>1,0</sub> + &gamma;<sub>1,1</sub>X* + &gamma;<sub>1,2</sub>Y + &gamma;<sub>1,2+j</sub>C<sub>j</sub>, }}{\eqn{log(P(X=1,U=0)/P(X=0,U=0)) = \gamma_{1,0} + \gamma_{1,1} X^* + \gamma_{1,2} Y + \gamma_{1,2+j} C_j, }}
where X is the binary true exposure, U is the binary unmeasured
confounder, X* is the binary misclassified exposure, Y is the binary
outcome, C represents the vector of binary measured confounders (if any),
and j corresponds to the number of measured confounders.}

\item{x0u1_model_coefs}{The regression coefficients corresponding to the
model:
\ifelse{html}{\out{log(P(X=0,U=1)/P(X=0,U=0)) = &gamma;<sub>2,0</sub> + &gamma;<sub>2,1</sub>X* + &gamma;<sub>2,2</sub>Y + &gamma;<sub>2,2+j</sub>C<sub>j</sub>, }}{\eqn{log(P(X=0,U=1)/P(X=0,U=0)) = \gamma_{2,0} + \gamma_{2,1} X^* + \gamma_{2,2} Y + \gamma_{2,2+j} C_j, }}
where X is the binary true exposure, U is the binary unmeasured
confounder, X* is the binary misclassified exposure, Y is the binary
outcome, C represents the vector of binary measured confounders (if any),
and j corresponds to the number of measured confounders.}

\item{x1u1_model_coefs}{The regression coefficients corresponding to the
model:
\ifelse{html}{\out{log(P(X=1,U=1)/P(X=0,U=0)) = &gamma;<sub>3,0</sub> + &gamma;<sub>3,1</sub>X* + &gamma;<sub>3,2</sub>Y + &gamma;<sub>3,2+j</sub>C<sub>j</sub>, }}{\eqn{log(P(X=1,U=1)/P(X=0,U=0)) = \gamma_{3,0} + \gamma_{3,1} X^* + \gamma_{3,2} Y + \gamma_{3,2+j} C_j, }}
where X is the binary true exposure, U is the binary unmeasured
confounder, X* is the binary misclassified exposure, Y is the binary
outcome, C represents the vector of binary measured confounders (if any),
and j corresponds to the number of measured confounders.}

\item{level}{Value from 0-1 representing the full range of the confidence
interval. Default is 0.95.}
}
\value{
A list where the first item is the odds ratio estimate of the
 effect of the exposure on the outcome and the second item is the
 confidence interval as the vector: (lower bound, upper bound).
}
\description{
\code{adjust_multinom_uc_emc} returns the exposure-outcome odds ratio and
confidence interval, adjusted for uncontrolled confounding and exposure
misclassificaiton.
}
\details{
This function uses one bias model, a multinomial logistic regression model,
to predict the uncontrolled confounder (U) and exposure (X). If separate bias
models for X and U are desired, use \code{adjust_uc_emc}.

Values for the regression coefficients can be applied as
fixed values or as single draws from a probability
distribution (ex: \code{rnorm(1, mean = 2, sd = 1)}). The latter has
the advantage of allowing the researcher to capture the uncertainty
in the bias parameter estimates. To incorporate this uncertainty in the
estimate and confidence interval, this function should be run in loop across
bootstrap samples of the dataframe for analysis. The estimate and
confidence interval would then be obtained from the median and quantiles
of the distribution of odds ratio estimates.
}
\examples{
adjust_multinom_uc_emc(
  df_uc_emc,
  exposure = "Xstar",
  outcome = "Y",
  confounders = "C1",
  x1u0_model_coefs = c(-2.82, 1.62, 0.68, -0.06),
  x0u1_model_coefs = c(-0.20, 0.00, 0.68, -0.05),
  x1u1_model_coefs = c(-2.36, 1.62, 1.29, 0.27)
)

}
