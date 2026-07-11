# cramersV() calculates the Cramer's V measure of effect size for chi-square tests.
# I haven't thought about this function in a while: it might not be the right way
# to go about this one.


#' Cramer's V
#'
#' @description Calculates Cramer's V, a measure of the strength of association
#' for chi-square tests.
#'
#' @param ... Arguments passed to \code{\link{chisq.test}}, in the same format
#'   accepted by that function.
#'
#' @details Cramer's V summarises the strength of association from a chi-square
#' test. It is appropriate for both tests of association (two categorical
#' variables) and goodness of fit tests (one variable versus hypothesised
#' probabilities). Values range from 0 (no association) to 1 (perfect
#' association). The arguments are passed directly to \code{\link{chisq.test}},
#' so the input format is the same.
#'
#' @return A single number giving the value of Cramer's V.
#'
#' @seealso
#' \code{\link{chisq.test}},
#' \code{\link{associationTest}},
#' \code{\link{goodnessOfFitTest}}
#'
#' @export
#'
#' @examples
#' # frequency table for two groups, each choosing from three options
#' condition1 <- c(30, 20, 50)
#' condition2 <- c(35, 30, 35)
#' X <- cbind(condition1, condition2)
#' rownames(X) <- c("choice1", "choice2", "choice3")
#'
#' # chi-square test of association
#' chisq.test(X)
#'
#' # effect size estimate
#' cramersV(X)
#'
cramersV <- function(...) {
  # Pearson chi-squared (correct = FALSE) is required so that V is on the
  # correct 0-to-1 scale. Yates' continuity correction reduces chi-squared
  # below its Pearson value, which causes V to be less than 1 even for
  # perfect 2x2 association.
  test <- stats::chisq.test(..., correct = FALSE)
  chi2 <- test$statistic
  N <- sum(test$observed)

  if (test$method == "Chi-squared test for given probabilities") {
    # for GOF test, calculate max chi-square value
    ind <- which.min(test$expected)
    max.dev <- test$expected
    max.dev[ind] <- N - max.dev[ind]
    max.chi2 <- sum(max.dev^2 / test$expected)
    V <- sqrt(chi2 / max.chi2)
  } else {
    # for test of association, use analytic expression
    k <- min(dim(test$observed))
    V <- sqrt(chi2 / (N * (k - 1)))
  }
  names(V) <- NULL
  return(V)
}
