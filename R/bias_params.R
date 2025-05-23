#' Represent bias parameters
#'
#' @description
#' `bias_params` is one of two different options to represent bias assumptions for bias adjustment. The [multibias_adjust()] function will apply the assumptions from these models and use them to adjust for biases in the observed data. It takes one input, a list, where each item in the list corresponds to the necessary models for bias adjustment. See below for bias models.
#'
#' For each of the following bias models, the variables are defined:
#' \itemize{
#'   \item X = True exposure
#'   \item X* = Misclassified exposure
#'   \item Y = True outcome
#'   \item Y* = Misclassified outcome
#'   \item C = Known confounder
#'   \item j = Number of known confounders
#'   \item U = Uncontrolled confounder
#'   \item S = Selection indicator
#' }
#'
#' \describe{
#'   \item{Uncontrolled confounding}{
#'     \ifelse{html}{\out{logit(P(U=1)) = &alpha;<sub>0</sub> + &alpha;<sub>1</sub>X + &alpha;<sub>2</sub>Y + &alpha;<sub>2+j</sub>C<sub>j</sub> }}{\eqn{logit(P(U=1)) = \alpha_0 + \alpha_1 X + \alpha_2 Y + \alpha_{2+j} C_j }}
#'   }
#'
#'   \item{Exposure misclassification}{
#'     \ifelse{html}{\out{logit(P(X=1)) = &delta;<sub>0</sub> + &delta;<sub>1</sub>X* + &delta;<sub>2</sub>Y + &delta;<sub>2+j</sub>C<sub>j</sub> }}{\eqn{logit(P(X=1)) = \delta_0 + \delta_1 X^* + \delta_2 Y + \delta_{2+j} C_j }}
#'   }
#'
#'   \item{Outcome misclassification}{
#'     \ifelse{html}{\out{logit(P(Y=1)) = &delta;<sub>0</sub> + &delta;<sub>1</sub>X + &delta;<sub>2</sub>Y* + &delta;<sub>2+j</sub>C<sub>j</sub> }}{\eqn{logit(P(Y=1)) = \delta_0 + \delta_1 X + \delta_2 Y^* + \delta_{2+j} C_j }}
#'   }
#'
#'   \item{Selection bias}{
#'     \ifelse{html}{\out{logit(P(S=1)) = &beta;<sub>0</sub> + &beta;<sub>1</sub>X + &beta;<sub>2</sub>Y }}{\eqn{logit(P(S=1)) = \beta_0 + \beta_1 X + \beta_2 Y }}
#'   }
#'
#'   \item{Uncontrolled Confounding & Exposure Misclassification (Option 1)}{
#'     \ifelse{html}{\out{logit(P(U=1)) = &alpha;<sub>0</sub> + &alpha;<sub>1</sub>X + &alpha;<sub>2</sub>Y <br> logit(P(X=1)) = &delta;<sub>0</sub> + &delta;<sub>1</sub>X* + &delta;<sub>2</sub>Y + &delta;<sub>2+j</sub>C<sub>j</sub> }}{\eqn{logit(P(U=1)) = \alpha_0 + \alpha_1 X + \alpha_2 Y \\ logit(P(X=1)) = \delta_0 + \delta_1 X^* + \delta_2 Y + \delta_{2+j} C_j }}
#'   }
#'
#'   \item{Uncontrolled Confounding & Exposure Misclassification (Option 2)}{
#'     \ifelse{html}{\out{log(P(X=1,U=0)/P(X=0,U=0)) = &gamma;<sub>1,0</sub> + &gamma;<sub>1,1</sub>X* + &gamma;<sub>1,2</sub>Y + &gamma;<sub>1,2+j</sub>C<sub>j</sub> <br> log(P(X=0,U=1)/P(X=0,U=0)) = &gamma;<sub>2,0</sub> + &gamma;<sub>2,1</sub>X* + &gamma;<sub>2,2</sub>Y + &gamma;<sub>2,2+j</sub>C<sub>j</sub> <br> log(P(X=1,U=1)/P(X=0,U=0)) = &gamma;<sub>3,0</sub> + &gamma;<sub>3,1</sub>X* + &gamma;<sub>3,2</sub>Y + &gamma;<sub>3,2+j</sub>C<sub>j</sub> }}{\eqn{log(P(X=1,U=0)/P(X=0,U=0)) = \gamma_{1,0} + \gamma_{1,1} X^* + \gamma_{1,2} Y + \gamma_{1,2+j} C_j \\ log(P(X=0,U=1)/P(X=0,U=0)) = \gamma_{2,0} + \gamma_{2,1} X^* + \gamma_{2,2} Y + \gamma_{2,2+j} C_j \\ log(P(X=1,U=1)/P(X=0,U=0)) = \gamma_{3,0} + \gamma_{3,1} X^* + \gamma_{3,2} Y + \gamma_{3,2+j} C_j }}
#'   }
#'
#'   \item{Uncontrolled Confounding & Outcome Misclassification (Option 1)}{
#'     \ifelse{html}{\out{logit(P(U=1)) = &alpha;<sub>0</sub> + &alpha;<sub>1</sub>X + &alpha;<sub>2</sub>Y <br> logit(P(Y=1)) = &delta;<sub>0</sub> + &delta;<sub>1</sub>X + &delta;<sub>2</sub>Y* + &delta;<sub>2+j</sub>C<sub>j</sub> }}{\eqn{logit(P(U=1)) = \alpha_0 + \alpha_1 X + \alpha_2 Y \\ logit(P(Y=1)) = \delta_0 + \delta_1 X + \delta_2 Y^* + \delta_{2+j} C_j }}
#'   }
#'
#'   \item{Uncontrolled Confounding & Outcome Misclassification (Option 2)}{
#'     \ifelse{html}{\out{log(P(U=1,Y=0)/P(U=0,Y=0)) = &gamma;<sub>1,0</sub> + &gamma;<sub>1,1</sub>X + &gamma;<sub>1,2</sub>Y* + &gamma;<sub>1,2+j</sub>C<sub>j</sub> <br> log(P(U=0,Y=1)/P(U=0,Y=0)) = &gamma;<sub>2,0</sub> + &gamma;<sub>2,1</sub>X + &gamma;<sub>2,2</sub>Y* + &gamma;<sub>2,2+j</sub>C<sub>j</sub> <br> log(P(U=1,Y=1)/P(U=0,Y=0)) = &gamma;<sub>3,0</sub> + &gamma;<sub>3,1</sub>X + &gamma;<sub>3,2</sub>Y* + &gamma;<sub>3,2+j</sub>C<sub>j</sub> }}{\eqn{log(P(U=1,Y=0)/P(U=0,Y=0)) = \gamma_{1,0} + \gamma_{1,1} X + \gamma_{1,2} Y^* + \gamma_{1,2+j} C_j \\ log(P(U=0,Y=1)/P(U=0,Y=0)) = \gamma_{2,0} + \gamma_{2,1} X + \gamma_{2,2} Y^* + \gamma_{2,2+j} C_j \\ log(P(U=1,Y=1)/P(U=0,Y=0)) = \gamma_{3,0} + \gamma_{3,1} X + \gamma_{3,2} Y^* + \gamma_{3,2+j} C_j }}
#'   }
#'
#'   \item{Uncontrolled Confounding & Selection Bias}{
#'     \ifelse{html}{\out{logit(P(U=1)) = &alpha;<sub>0</sub> + &alpha;<sub>1</sub>X + &alpha;<sub>2</sub>Y + &alpha;<sub>2+j</sub>C<sub>j</sub> <br> logit(P(S=1)) = &beta;<sub>0</sub> + &beta;<sub>1</sub>X + &beta;<sub>2</sub>Y }}{\eqn{logit(P(U=1)) = \alpha_0 + \alpha_1 X + \alpha_2 Y + \alpha_{2+j} C_j \\ logit(P(S=1)) = \beta_0 + \beta_1 X + \beta_2 Y }}
#'   }
#'
#'  \item{Exposure Misclassification & Outcome Misclassification (Option 1)}{
#'    \ifelse{html}{\out{logit(P(X=1)) = &delta;<sub>0</sub> + &delta;<sub>1</sub>X* + &delta;<sub>2</sub>Y* + &delta;<sub>2+j</sub>C<sub>j</sub> <br> logit(P(Y=1)) = &beta;<sub>0</sub> + &beta;<sub>1</sub>X + &beta;<sub>2</sub>Y* + &beta;<sub>2+j</sub>C<sub>j</sub> }}{\eqn{logit(P(X=1)) = \delta_0 + \delta_1 X^* + \delta_2 Y^* + \delta_{2+j} C_j \\ logit(P(Y=1)) = \beta_0 + \beta_1 X + \beta_2 Y^* + \beta_{2+j} C_j }}
#'  }
#'
#'   \item{Exposure Misclassification & Outcome Misclassification (Option 2)}{
#'     \ifelse{html}{\out{log(P(X=1,Y=0) / P(X=0,Y=0)) = &gamma;<sub>1,0</sub> + &gamma;<sub>1,1</sub>X* + &gamma;<sub>1,2</sub>Y* + &gamma;<sub>1,2+j</sub>C<sub>j</sub> <br> log(P(X=0,Y=1) / P(X=0,Y=0)) = &gamma;<sub>2,0</sub> + &gamma;<sub>2,1</sub>X* + &gamma;<sub>2,2</sub>Y* + &gamma;<sub>2,2+j</sub>C<sub>j</sub> <br> log(P(X=1,Y=1) / P(X=0,Y=0)) = &gamma;<sub>3,0</sub> + &gamma;<sub>3,1</sub>X* + &gamma;<sub>3,2</sub>Y* + &gamma;<sub>3,2+j</sub>C<sub>j</sub> }}{\eqn{log(P(X=1,Y=0) / P(X=0,Y=0)) = \gamma_{1,0} + \gamma_{1,1} X^* + \gamma_{1,2} Y^* + \gamma_{1,2+j} C_j \\ log(P(X=0,Y=1) / P(X=0,Y=0)) = \gamma_{2,0} + \gamma_{2,1} X^* + \gamma_{2,2} Y^* + \gamma_{2,2+j} C_j \\ log(P(X=1,Y=1) / P(X=0,Y=0)) = \gamma_{3,0} + \gamma_{3,1} X^* + \gamma_{3,2} Y^* + \gamma_{3,2+j} C_j }}
#'   }
#'
#'   \item{Exposure Misclassification & Selection Bias}{
#'     \ifelse{html}{\out{logit(P(X=1)) = &delta;<sub>0</sub> + &delta;<sub>1</sub>X* + &delta;<sub>2</sub>Y + &delta;<sub>2+j</sub>C<sub>j</sub> <br> logit(P(S=1)) = &beta;<sub>0</sub> + &beta;<sub>1</sub>X* + &beta;<sub>2</sub>Y + &beta;<sub>2+j</sub>C<sub>j</sub> }}{\eqn{logit(P(X=1)) = \delta_0 + \delta_1 X^* + \delta_2 Y + \delta_{2+j} C_j \\ logit(P(S=1)) = \beta_0 + \beta_1 X^* + \beta_2 Y + \beta_{2+j} C_j }}
#'   }
#'
#'  \item{Outcome Misclassification & Selection Bias}{
#'     \ifelse{html}{\out{logit(P(Y=1)) = &delta;<sub>0</sub> + &delta;<sub>1</sub>X + &delta;<sub>2</sub>Y* + &delta;<sub>2+j</sub>C<sub>j</sub> <br> logit(P(S=1)) = &beta;<sub>0</sub> + &beta;<sub>1</sub>X + &beta;<sub>2</sub>Y* + &beta;<sub>2+j</sub>C<sub>j</sub> }}{\eqn{logit(P(Y=1)) = \delta_0 + \delta_1 X + \delta_2 Y^* + \delta_{2+j} C_j \\ logit(P(S=1)) = \beta_0 + \beta_1 X + \beta_2 Y^* + \beta_{2+j} C_j }}
#'   }
#'   \item{Uncontrolled Confounding, Exposure Misclassification, and Selection Bias (Option 1)}{
#'     \ifelse{html}{\out{logit(P(U=1)) = &alpha;<sub>0</sub> + &alpha;<sub>1</sub>X + &alpha;<sub>2</sub>Y <br> logit(P(X=1)) = &delta;<sub>0</sub> + &delta;<sub>1</sub>X* + &delta;<sub>2</sub>Y + &delta;<sub>2+j</sub>C<sub>j</sub> <br> logit(P(S=1)) = &beta;<sub>0</sub> + &beta;<sub>1</sub>X* + &beta;<sub>2</sub>Y + &beta;<sub>2+j</sub>C<sub>j</sub> }}{\eqn{logit(P(U=1)) = \alpha_0 + \alpha_1 X + \alpha_2 Y \\ logit(P(X=1)) = \delta_0 + \delta_1 X^* + \delta_2 Y + \delta_{2+j} C_j \\ logit(P(S=1)) = \beta_0 + \beta_1 X^* + \beta_2 Y + \beta_{2+j} C_j }}
#'   }
#'   \item{Uncontrolled Confounding, Exposure Misclassification, and Selection Bias (Option 2)}{
#'     \ifelse{html}{\out{log(P(X=1,U=0)/P(X=0,U=0)) = &gamma;<sub>1,0</sub> + &gamma;<sub>1,1</sub>X* + &gamma;<sub>1,2</sub>Y + &gamma;<sub>1,2+j</sub>C<sub>j</sub> <br> log(P(X=0,U=1)/P(X=0,U=0)) = &gamma;<sub>2,0</sub> + &gamma;<sub>2,1</sub>X* + &gamma;<sub>2,2</sub>Y + &gamma;<sub>2,2+j</sub>C<sub>j</sub> <br> log(P(X=1,U=1)/P(X=0,U=0)) = &gamma;<sub>3,0</sub> + &gamma;<sub>3,1</sub>X* + &gamma;<sub>3,2</sub>Y + &gamma;<sub>3,2+j</sub>C<sub>j</sub> <br> logit(P(S=1)) = &beta;<sub>0</sub> + &beta;<sub>1</sub>X* + &beta;<sub>2</sub>Y + &beta;<sub>2+j</sub>C<sub>j</sub> }}{\eqn{log(P(X=1,U=0)/P(X=0,U=0)) = \gamma_{1,0} + \gamma_{1,1} X^* + \gamma_{1,2} Y + \gamma_{1,2+j} C_j \\ log(P(X=0,U=1)/P(X=0,U=0)) = \gamma_{2,0} + \gamma_{2,1} X^* + \gamma_{2,2} Y + \gamma_{2,2+j} C_j \\ log(P(X=1,U=1)/P(X=0,U=0)) = \gamma_{3,0} + \gamma_{3,1} X^* + \gamma_{3,2} Y + \gamma_{3,2+j} C_j \\ logit(P(S=1)) = \beta_0 + \beta_1 X^* + \beta_2 Y + \beta_{2+j} C_j }}
#'   }
#'   \item{Uncontrolled Confounding, Outcome Misclassification, and Selection Bias (Option 1)}{
#'     \ifelse{html}{\out{logit(P(U=1)) = &alpha;<sub>0</sub> + &alpha;<sub>1</sub>X + &alpha;<sub>2</sub>Y <br> logit(P(Y=1)) = &delta;<sub>0</sub> + &delta;<sub>1</sub>X + &delta;<sub>2</sub>Y* + &delta;<sub>2+j</sub>C<sub>j</sub> <br> logit(P(S=1)) = &beta;<sub>0</sub> + &beta;<sub>1</sub>X + &beta;<sub>2</sub>Y* + &beta;<sub>2+j</sub>C<sub>j</sub> }}{\eqn{logit(P(U=1)) = \alpha_0 + \alpha_1 X + \alpha_2 Y \\ logit(P(Y=1)) = \delta_0 + \delta_1 X + \delta_2 Y^* + \delta_{2+j} C_j \\ logit(P(S=1)) = \beta_0 + \beta_1 X + \beta_2 Y^* + \beta_{2+j} C_j }}
#'   }
#'   \item{Uncontrolled Confounding, Outcome Misclassification, and Selection Bias (Option 2)}{
#'     \ifelse{html}{\out{log(P(U=1,Y=0)/P(U=0,Y=0)) = &gamma;<sub>1,0</sub> + &gamma;<sub>1,1</sub>X + &gamma;<sub>1,2</sub>Y* + &gamma;<sub>1,2+j</sub>C<sub>j</sub> <br> log(P(U=0,Y=1)/P(U=0,Y=0)) = &gamma;<sub>2,0</sub> + &gamma;<sub>2,1</sub>X + &gamma;<sub>2,2</sub>Y* + &gamma;<sub>2,2+j</sub>C<sub>j</sub> <br> log(P(U=1,Y=1)/P(U=0,Y=0)) = &gamma;<sub>3,0</sub> + &gamma;<sub>3,1</sub>X + &gamma;<sub>3,2</sub>Y* + &gamma;<sub>3,2+j</sub>C<sub>j</sub> <br> logit(P(S=1)) = &beta;<sub>0</sub> + &beta;<sub>1</sub>X + &beta;<sub>2</sub>Y* + &beta;<sub>2+j</sub>C<sub>j</sub> }}{\eqn{log(P(U=1,Y=0)/P(U=0,Y=0)) = \gamma_{1,0} + \gamma_{1,1} X + \gamma_{1,2} Y^* + \gamma_{1,2+j} C_j \\ log(P(U=0,Y=1)/P(U=0,Y=0)) = \gamma_{2,0} + \gamma_{2,1} X + \gamma_{2,2} Y^* + \gamma_{2,2+j} C_j \\ log(P(U=1,Y=1)/P(U=0,Y=0)) = \gamma_{3,0} + \gamma_{3,1} X + \gamma_{3,2} Y^* + \gamma_{3,2+j} C_j \\ logit(P(S=1)) = \beta_0 + \beta_1 X + \beta_2 Y^* + \beta_{2+j} C_j }}
#'   }
#' }
#'
#' @param coef_list List of coefficient values from the above options of
#' models. Each item of the list is an equation. The left side of the equation
#' identifies the model (i.e., "u" for the model predicting the uncontrolled
#' confounder). For the multinomial models, specify the value here based on the
#' numerator (i.e., "x1u0", "x0u1", "x1u1" for the three multinomial models in
#' Uncontrolled Confounding & Exposure Misclassification, Option 2)
#' The right side of the equation is the vector of values
#' corresponding to the model coefficients (from left to right).
#'
#' @examples
#' list_for_uc <- list(
#'   u = c(-0.19, 0.61, 0.70, -0.09, 0.10, -0.15)
#' )
#'
#' bp_uc <- bias_params(coef_list = list_for_uc)
#'
#' list_for_em_om <- list(
#'   x1y0 = c(-2.18, 1.63, 0.23, 0.36),
#'   x0y1 = c(-3.17, 0.22, 1.60, 0.40),
#'   x1y1 = c(-4.76, 1.82, 1.83, 0.72)
#' )
#'
#' bp_em_om <- bias_params(coef_list = list_for_em_om)
#'
#' @export

bias_params <- function(coef_list) {
  # Check if the input is a list (R's equivalent of a dictionary)
  if (!is.list(coef_list)) {
    stop("Input must be a list (dictionary).")
  }

  # Check if all keys are strings
  if (!all(sapply(names(coef_list), is.character))) {
    stop("All keys in the dictionary must be strings.")
  }

  # Check if all values are vectors
  if (!all(sapply(coef_list, is.vector))) {
    stop("All values in the dictionary must be vectors.")
  }

  # Check if key is an acceptable value
  acceptable_values <- c(
    "u", "x", "y", "s", "x1u0", "x0u1", "x1u1", "u1y0", "u0y1", "u1y1",
    "x1y0", "x0y1", "x1y1"
  )
  for (name in names(coef_list)) {
    if (!(name %in% acceptable_values)) {
      stop(
        paste0(
          "Unacceptable value in coef_list. Acceptable values include: ",
          "u, x, y, s, x1u0, x1u1, x1u1, u1y0, u0y1, u1y1, x1y0, x0y1, x1y1"
        )
      )
    }
  }

  obj <- list(coef_list = coef_list)
  class(obj) <- "bias_params"

  return(obj)
}

#' @export

print.bias_params <- function(x, ...) {
  cat("Bias Parameters\n")
  cat("---------------------------------\n")
  for (key in names(x$coef_list)) {
    cat("Model", key, "\n")
    cat("  Coefs: ", paste(x$coef_list[[key]], collapse = ", "), "\n")
  }
  invisible(x)
}
