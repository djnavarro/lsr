

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
#' @description Calculates the standardised regression coefficients for a linear model.
#'
#' @param x A linear model object (i.e. class \code{lm})
#'
#' @details Calculates the standardised regression coefficients (beta-weights),
#' namely the values of the regression coefficients that would have been observed
#' has all regressors and the outcome variable been scaled to have mean 0 and
#' variance 1 before fitting the regression model. Standardised coefficients are
#' sometimes useful in some applied contexts since there is a sense in which all
#' beta values are "on the same scale", though this is not entirely unproblematic.
#'
#' @return A matrix with the regressors as rows, and the two different
#' regression coefficients (unstandardised and standardised) as the two
#' columns. The columns are labeled b (unstandardised) and beta (standardised).
#'
#' @export
#'
#' @examples
#'
#' # Example 1: simple linear regression
#'
#' # data
#' X1 <- c(0.69, 0.77, 0.92, 1.72, 1.79, 2.37, 2.64, 2.69, 2.84, 3.41)
#' Y  <- c(3.28, 4.23, 3.34, 3.73, 5.33, 6.02, 5.16, 6.49, 6.49, 6.05)
#'
#' model1 <- lm( Y ~ X1 )  # run a simple linear regression
#' coefficients( model1 )  # extract the raw regression coefficients
#' standardCoefs( model1 ) # extract standardised coefficients
#'
#'
#' # Example 2: multiple linear regression
#'
#' X2 <- c(0.19, 0.22, 0.95, 0.43, 0.51, 0.04, 0.12, 0.44, 0.38, 0.33)
#' model2 <- lm( Y ~ X1 + X2 )   # new model
#' standardCoefs( model2 )       # standardised coefficients
#'
#' #Example 3: interaction terms
#'
#' model3 <- lm( Y ~ X1 * X2 )
#' coefficients( model3 )
#' standardCoefs( model3 )
#'
#' # Note that these beta values are equivalent to standardising all
#' # three regressors including the interaction term X1:X2, not merely
#' # standardising the two predictors X1 and X2.
#'
standardCoefs <- function( x ) {

  if( !methods::is(x,"lm") ) {stop( '"x" must be a linear model object')}

  # read off the useful info
  term.names <- names(x$coefficients)[-1] # all names except "intercept"
  b <- x$coefficients[-1] # grab coefficients except "intercept"

  # construct the design matrix
  predictors <- stats::model.matrix(x$terms, data = x$model)
  predictors <- predictors[ ,-1, drop=FALSE] # (we don't want the intercept)
  predictors <- as.data.frame(predictors)  # hack!!

  # standard deviations
  sy <- stats::sd(x$model[[1]])       # sd for the outcome
  sx <- sapply(predictors, stats::sd)  # sd for the predictors

  # now compute beta
  beta <- b * sx / sy

  # convert to matrix
  coefficients <- cbind(b,beta)
  colnames(coefficients) <- c("b","beta")
  rownames(coefficients) <- term.names

  return(coefficients)


}
