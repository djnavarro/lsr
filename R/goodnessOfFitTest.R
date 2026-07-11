#' Chi-square goodness of fit test
#'
#' @description Runs a chi-square goodness of fit test to check whether the
#' observed frequencies in a categorical variable match a set of hypothesised
#' probabilities.
#'
#' @param x A factor variable containing the observed outcomes.
#' @param p A numeric vector of hypothesised probabilities, one per level of
#'   \code{x}. The values must sum to 1. If named, the names must match the
#'   levels of \code{x} (order does not matter). If omitted, all outcomes are
#'   assumed to be equally likely.
#'
#' @details The test checks whether the observed frequencies for a categorical
#' variable are consistent with the probabilities specified in \code{p}.
#'
#' Missing values in \code{x} are removed before the test is run, and a
#' warning is issued if any cases are dropped. If the probabilities in
#' \code{p} do not sum exactly to 1, they are rescaled with a warning.
#'
#' If \code{x} has unused factor levels (levels with zero observed cases), a
#' warning is issued. Those levels are included in the test with zero observed
#' cases, which changes the degrees of freedom and may give misleading results.
#' Call \code{\link{droplevels}} on the data first if this is not intended.
#'
#' @return Prints a summary of the test showing the variable name, null and
#' alternative hypotheses, a table of observed frequencies, expected
#' frequencies, and hypothesised probabilities, and the test results
#' (chi-square statistic, degrees of freedom, p-value). The underlying results
#' are also returned as a list, so the output can be assigned to a variable
#' and inspected if needed.
#'
#' @export
#'
#' @seealso
#' \code{\link{chisq.test}},
#' \code{\link{associationTest}},
#' \code{\link{cramersV}}
#'
#' @examples
#' # raw data
#' gender <- factor(
#'   c(
#'     "male", "male", "male", "male", "female", "female",
#'     "female", "male", "male", "male"
#'   )
#' )
#'
#' # goodness of fit test against the hypothesis that males and
#' # females occur with equal frequency
#' goodnessOfFitTest(gender)
#'
#' # goodness of fit test against the hypothesis that males appear
#' # with probability .6 and females with probability .4.
#' goodnessOfFitTest(gender, p = c(.4, .6))
#' goodnessOfFitTest(gender, p = c(female = .4, male = .6))
#' goodnessOfFitTest(gender, p = c(male = .6, female = .4))
#'
goodnessOfFitTest <- function(x, p = NULL) {
  # check if x is missing
  if (missing(x)) {
    stop('"x" argument is missing, with no default')
  }

  # make sure x is a factor
  if (!methods::is(x, "factor")) {
    stop("input argument 'x' must be a factor")
  }

  # check for missing data & print warning if needed
  missingData <- is.na(x)
  x <- x[!missingData]
  if (sum(missingData) > 0) {
    warning(paste(sum(missingData)), " case(s) removed due to missingness")
  }

  # make sure x has at least two levels
  if (nlevels(x) <= 1) {
    stop("factor 'x' must be have at least two levels")
  }

  # warn if any factor levels have zero observed cases
  unused_levels <- levels(x)[tabulate(x, nbins = nlevels(x)) == 0]
  if (length(unused_levels) > 0) {
    warning(
      "Factor 'x' has unused level(s): ",
      paste(unused_levels, collapse = ", "),
      ". These levels are included in the test with zero observed cases, ",
      "which changes the degrees of freedom and may give misleading results. ",
      "Call droplevels() on your data first if this is not intended."
    )
  }

  # either set the default probabilities...
  if (is.null(p)) {
    p <- rep.int(1, nlevels(x)) / nlevels(x)
    names(p) <- levels(x)
  } else { # or check that the user supplied probabilites make sense...
    if (!methods::is(p, "numeric") | any(is.na(p))) stop("'p' must be a numeric vector of probabilities")
    if (length(p) != nlevels(x)) stop("'p' contains the wrong number of elements")
    if (abs(sum(p) - 1) > 10^-10) {
      warning("probabilities in 'p' do not add up to 1. rescaled values used")
      p <- p / sum(p)
    }
    if (!is.null(names(p))) { # if p has names, better check
      if (all(sort(names(p)) == sort(levels(x)))) { # if they match
        p <- p[levels(x)] # sort them to match level ordering
      } else {
        warning("'p' has names that differ from the levels of 'x'")
      }
    }
  }

  # tabulate x
  f <- table(x)

  # run the corresponding chi-square test: suppressing the warning,
  # replacing it with our own if it exists
  old.warn <- options(warn = 2) # convert warnings to errors
  on.exit(options(old.warn)) # always restore, even if an error is thrown
  htest <- try(stats::chisq.test(x = f, p = p), silent = TRUE)
  need.warning <- FALSE

  if (inherits(htest, "try-error")) {
    need.warning <- length(grep("Chi-squared approximation may be incorrect", htest)) > 0
    if (need.warning) {
      options(warn = -1) # suppress warnings for the retry
    } else {
      stop(conditionMessage(attr(htest, "condition")))
    }
    htest <- stats::chisq.test(x = f, p = p)
  }

  # get the variable name if it exists
  outcome <- match.call()[2] # get the x argument from the call
  outcome <- as.character(outcome)

  # create output structure
  out <- list(
    outcome = outcome,
    p = p,
    observed = htest$observed,
    expected = htest$expected,
    difference = htest$observed - htest$expected,
    statistic = htest$statistic,
    df = htest$parameter,
    p.value = htest$p.value,
    warn = need.warning
  )
  class(out) <- "gofTest"

  # throw the warning if needed
  if (need.warning) {
    warning("Expected frequencies too small: chi-squared approximation may be incorrect")
  }

  return(out)
}


#' Print goodness of fit test results
#'
#' @description Prints the results of a chi-square goodness of fit test in a
#' readable format. This function is called automatically whenever a result
#' from \code{\link{goodnessOfFitTest}} is displayed.
#'
#' @param x A goodness of fit test result, as returned by
#'   \code{\link{goodnessOfFitTest}}.
#' @param ... Additional arguments (unused, included for compatibility).
#'
#' @return Invisibly returns \code{x} unchanged.
#' @export
print.gofTest <- function(x, ...) {
  # print the name of the test
  cat("\n     Chi-square test against specified probabilities\n\n")

  # print the data variable
  cat("Data variable:  ", x$outcome, "\n\n")

  # print the hypotheses being tested
  cat("Hypotheses: \n")
  cat("   null:        true probabilities are as specified\n")
  cat("   alternative: true probabilities differ from those specified\n")
  cat("\n")

  # print observed against expected
  descriptives <- cbind(x$observed, x$expected, x$p)
  colnames(descriptives) <- c("observed freq.", "expected freq.", "specified prob.")
  cat("Descriptives: \n")
  print(descriptives)
  cat("\n")

  # print test statistics
  nDigits <- 3
  cat("Test results: \n")
  cat("   X-squared statistic: ", round(x$statistic, nDigits), "\n")
  cat("   degrees of freedom: ", round(x$df, nDigits), "\n")
  pp <- ifelse(x$p.value < .001, "<.001", round(x$p.value, nDigits))
  cat("   p-value: ", pp, "\n")
  if (x$warn) cat("   warning: expected frequencies too small, results may be inaccurate\n")
  cat("\n")


  return(invisible(x))
}
