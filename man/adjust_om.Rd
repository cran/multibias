% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adjust_om.R
\name{adjust_om}
\alias{adjust_om}
\title{Adust for outcome misclassification.}
\usage{
adjust_om(
  data,
  exposure,
  outcome,
  confounders = NULL,
  y_model_coefs,
  level = 0.95
)
}
\arguments{
\item{data}{Dataframe for analysis.}

\item{exposure}{String name of the exposure variable.}

\item{outcome}{String name of the outcome variable.}

\item{confounders}{String name(s) of the confounder(s).
A maximum of three confounders is allowed.}

\item{y_model_coefs}{The regression coefficients corresponding to the model:
\ifelse{html}{\out{logit(P(Y=1)) = &delta;<sub>0</sub> + &delta;<sub>1</sub>X + &delta;<sub>2</sub>Y* + &delta;<sub>2+j</sub>C<sub>j</sub>, }}{\eqn{logit(P(Y=1)) = \_delta_0 + \_delta_1 X + \_delta_2 Y^* + \_delta_{2+j} C_j, }}
where \emph{Y} represents the binary true outcome, \emph{X} is the exposure,
\emph{Y}* is the binary misclassified outcome,
\emph{C} represents the vector of measured confounders (if any),
and \emph{j} corresponds to the number of measured confounders. The number
of parameters is therefore 3 + \emph{j}.}

\item{level}{Value from 0-1 representing the full range of the confidence
interval. Default is 0.95.}
}
\value{
A list where the first item is the odds ratio estimate of the
effect of the exposure on the outcome and the second item is the
confidence interval as the vector: (lower bound, upper bound).
}
\description{
\code{adjust_om} returns the exposure-outcome odds ratio and confidence
interval, adjusted for outcome misclassificaiton.
}
\details{
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
adjust_om(
  evans,
  exposure = "SMK",
  outcome = "CHD",
  confounders = "HPT",
  y_model_coefs = c(qlogis(0.01), log(1.5), log(5), log(1.5))
)

}
