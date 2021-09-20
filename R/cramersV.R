

# cramersV() calculates the Cramer's V measure of effect size for chi-square tests.
# I haven't thought about this function in a while: it might not be the right way
# to go about this one.


#' Cramer's V
#'
#' @description Calculate the Cramer's V measure of association
#'
#' @param ... Arguments to be passed to the \code{chisq.test} function.
#'
#' @details Calculates the Cramer's V measure of effect size for chi-square
#' tests of association and goodness of fit. The arguments to the
#' \code{cramersV} function are all passed straight to the \code{chisq.test}
#' function, and should have the same format.
#'
#' @return A numeric variable with a single element corresponding to the
#' value of V.
#'
#' @export
#'
#' @examples
#'
#' # Consider an experiment with two conditions, each with 100
#' # participants. Each participant chooses between one of three
#' # options. Possible data for this experiment:
#'
#' condition1 <- c(30, 20, 50)
#' condition2 <- c(35, 30, 35)
#' X <- cbind( condition1, condition2 )
#' rownames(X) <- c( 'choice1', 'choice2', 'choice3' )
#' print(X)
#'
#' # To test the null hypothesis that the distribution of choices
#' # is identical in the two conditions, we would run a chi-square
#' # test:
#' chisq.test(X)
#'
#' # To estimate the effect size we can use Cramer's V:
#' cramersV( X )  # returns a value of 0.159
#'
cramersV <- function (...) {

  test <- stats::chisq.test(...)
  chi2 <- test$statistic
  N <- sum(test$observed)

  if (test$method =="Chi-squared test for given probabilities"){
    # for GOF test, calculate max chi-square value
    ind <- which.min(test$expected)
    max.dev <- test$expected
    max.dev[ind] <- N-max.dev[ind]
    max.chi2 <- sum( max.dev ^2 / test$expected )
    V <- sqrt( chi2 / max.chi2 )
  }
  else {
    # for test of association, use analytic expression
    k <- min(dim(test$observed))
    V <- sqrt( chi2 / (N*(k-1)) )
  }
  names(V) <- NULL
  return(V)

}
