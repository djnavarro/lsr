# standardCoefs() computes the standardised regression coefficients (i.e. beta
# weights) for a regression model. It takes an lm object as input, and outputs a
# matrix showing both the raw "b" weights and the standardised "beta" weights.
# Note that, if you're serious about computing the relative importance of the
# predictors, you'd be better off looking at the relaimpo package, which provides
# better tools for relative importance regression. The standardCoefs() function
# is just a handy crutch for beginners: beta weights aren't the best solution to
# the problem.

#' Standardised regression coefficients
#'
#' @description Calculates standardised regression coefficients (beta weights)
#' for a linear model.
#'
#' @param x A linear model, as returned by \code{\link{lm}}.
#'
#' @details Standardised coefficients are the regression coefficients that
#' would result from fitting the model after scaling all predictors and the
#' outcome to have mean 0 and variance 1. They can be useful for comparing
#' the relative magnitude of predictors measured on different scales, though
#' this comparison should be interpreted with care.
#'
#' Note that when a model contains interaction terms, the interaction column
#' is also standardised as a whole, rather than being constructed from
#' standardised versions of the constituent predictors.
#'
#' @return A matrix with one row per predictor (excluding the intercept) and
#' two columns: \code{b} (unstandardised coefficient) and \code{beta}
#' (standardised coefficient).
#'
#' @seealso \code{\link{lm}}, \code{\link{coefficients}}
#'
#' @export
#'
#' @examples
#' X1 <- c(0.69, 0.77, 0.92, 1.72, 1.79, 2.37, 2.64, 2.69, 2.84, 3.41)
#' Y <- c(3.28, 4.23, 3.34, 3.73, 5.33, 6.02, 5.16, 6.49, 6.49, 6.05)
#'
#' # simple linear regression
#' model1 <- lm(Y ~ X1)
#' coefficients(model1) # unstandardised
#' standardCoefs(model1) # unstandardised and standardised side by side
#'
#' # multiple regression
#' X2 <- c(0.19, 0.22, 0.95, 0.43, 0.51, 0.04, 0.12, 0.44, 0.38, 0.33)
#' model2 <- lm(Y ~ X1 + X2)
#' standardCoefs(model2)
#'
#' # model with an interaction term
#' model3 <- lm(Y ~ X1 * X2)
#' standardCoefs(model3)
#'
standardCoefs <- function(x) {
  if (methods::is(x, "aov")) {
    stop('"x" must be a linear model object (aov models are not supported)')
  }
  if (!methods::is(x, "lm")) {
    stop('"x" must be a linear model object')
  }

  # read off the useful info
  term.names <- names(x$coefficients)[-1] # all names except "intercept"
  b <- x$coefficients[-1] # grab coefficients except "intercept"

  # construct the design matrix
  predictors <- stats::model.matrix(x$terms, data = x$model)
  predictors <- predictors[, -1, drop = FALSE] # (we don't want the intercept)
  predictors <- as.data.frame(predictors) # hack!!

  # standard deviations
  sy <- stats::sd(x$model[[1]]) # sd for the outcome
  sx <- sapply(predictors, stats::sd) # sd for the predictors

  # now compute beta
  beta <- b * sx / sy

  # convert to matrix
  coefficients <- cbind(b, beta)
  colnames(coefficients) <- c("b", "beta")
  rownames(coefficients) <- term.names

  return(coefficients)
}
