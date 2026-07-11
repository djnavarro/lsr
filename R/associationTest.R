#' Chi-square test of association / independence
#'
#' @description Runs a chi-square test to check whether two categorical
#' variables are independent of one another.
#'
#' @param formula A one-sided formula of the form \code{~var1 + var2},
#'   specifying the two variables to be tested. Both variables must be factors.
#' @param data An optional data frame containing the variables named in
#'   \code{formula}. If omitted, the variables are looked up in the workspace.
#'
#' @details The test checks whether two categorical variables are statistically
#' independent. Both variables must be factors, and the formula must be
#' one-sided with exactly two variables, e.g. \code{~gender + answer}.
#'
#' Missing values are removed before the test is run, and a warning is issued
#' if any cases are dropped. When both variables have only two levels, Yates'
#' continuity correction is applied automatically to the chi-squared statistic
#' (though not to the Cramer's V effect size).
#'
#' If either variable has unused factor levels (levels with zero observed
#' cases), a warning is issued. Those levels are included in the contingency
#' table with zero observed cases, which may give misleading results. Call
#' \code{\link{droplevels}} on the data first if this is not intended.
#'
#' @return Prints a summary of the test showing the variable names, null and
#' alternative hypotheses, observed and expected frequency tables, test results
#' (chi-square statistic, degrees of freedom, p-value), and Cramer's V as a
#' measure of effect size. The underlying results are also returned as a list,
#' so the output can be assigned to a variable and inspected if needed.
#'
#' @seealso
#' \code{\link{chisq.test}},
#' \code{\link{goodnessOfFitTest}},
#' \code{\link{cramersV}}
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   gender = factor(c("male", "male", "male", "male", "female", "female", "female")),
#'   answer = factor(c("heads", "heads", "heads", "heads", "tails", "tails", "heads"))
#' )
#'
#' associationTest(~ gender + answer, df)
#'
associationTest <- function(formula, data = NULL) {
  ############ check  formula ############

  # check that the user has input a formula
  if (missing(formula)) {
    stop('"formula" argument is missing, with no default')
  }
  if (!methods::is(formula, "formula")) {
    stop('"formula" argument must be a formula')
  }

  # the formula must be of the form ~ VAR1 + VAR2
  if (length(formula) != 2) stop('"formula" argument must be one-sided with two variables')
  vars <- all.vars(formula)
  if (length(vars) != 2) stop('"formula" argument must be one-sided with two variables')

  ############ check data frame ############

  if (!missing(data)) {
    # it needs to be data frame, because a matrix can't
    # contain actors
    if (!methods::is(data, "data.frame")) stop("'data' argument must be a data frame")
    data <- as.data.frame(data)

    # check that both variables are in the data frame
    if (!(vars[1] %in% names(data))) {
      stop(paste0("'", vars[1], "' is not the name of a variable in '", deparse(substitute(data)), "'"))
    }
    if (!(vars[2] %in% names(data))) {
      stop(paste0("'", vars[2], "' is not the name of a variable in '", deparse(substitute(data)), "'"))
    }
  } else {
    # check that all variables exist in the workspace
    workspace <- objects(parent.frame())

    # check that both variables are in the workspace
    if (!(vars[1] %in% workspace)) {
      stop(paste0("'", vars[1], "' is not the name of a variable in the workspace"))
    }
    if (!(vars[2] %in% workspace)) {
      stop(paste0("'", vars[2], "' is not the name of a variable in the workspace"))
    }

    # copy variables into a data frame if none is specified, and
    # check that the variables are appropriate for a data frame.
    # need to retain the missing values for later
    data <- try(eval(stats::model.frame(formula = formula, na.action = stats::na.pass),
      envir = parent.frame()
    ), silent = TRUE)
    if (methods::is(data, "try-error")) {
      stop("specified variables cannot be coerced to data frame")
    }
  }

  # subset the data frame
  data <- data[, vars]

  ############ check data frame ############

  # check that both variables are factors
  if (!methods::is(data[, vars[1]], "factor")) {
    stop(paste0("'", vars[1], "' is not a factor"))
  }
  if (!methods::is(data[, vars[2]], "factor")) {
    stop(paste0("'", vars[2], "' is not a factor"))
  }


  # check for missing data & print warning if needed
  missingData <- is.na(data[, vars[1]]) | is.na(data[, vars[2]])
  data <- data[!missingData, ]
  if (sum(missingData) > 0) {
    warning(paste(sum(missingData)), " case(s) removed due to missingness")
  }

  # warn if any factor levels have zero observed cases
  unused1 <- levels(data[, vars[1]])[tabulate(data[, vars[1]], nbins = nlevels(data[, vars[1]])) == 0]
  unused2 <- levels(data[, vars[2]])[tabulate(data[, vars[2]], nbins = nlevels(data[, vars[2]])) == 0]
  if (length(unused1) > 0 || length(unused2) > 0) {
    warning(
      "One or more variables have unused factor levels. ",
      "These levels are included in the contingency table with zero observed cases, ",
      "which may give misleading results. ",
      "Call droplevels() on your data first if this is not intended."
    )
  }


  ############ do the test ############

  # get the variable names
  f <- stats::xtabs(formula, data)

  # run the corresponding chi-square test, suppressing the warning,
  # replacing it with our own if it exists
  old.warn <- options(warn = 2) # convert warnings to errors
  on.exit(options(old.warn)) # always restore, even if an error is thrown
  htest <- try(stats::chisq.test(x = f), silent = TRUE)
  need.warning <- FALSE

  if (inherits(htest, "try-error")) {
    need.warning <- length(grep("Chi-squared approximation may be incorrect", htest)) > 0
    if (need.warning) {
      options(warn = -1) # suppress warnings for the retry
    } else {
      stop(conditionMessage(attr(htest, "condition")))
    }
    htest <- stats::chisq.test(x = f)
  }
  effectsize <- cramersV(f)

  # make a note of whether yates' correction has been applied
  yates <- FALSE
  if (nrow(f) == 2 & ncol(f) == 2) yates <- TRUE


  ############ output ############

  # create output structure
  out <- list(
    variables = vars,
    observed = htest$observed,
    expected = htest$expected,
    difference = htest$observed - htest$expected,
    statistic = htest$statistic,
    df = htest$parameter,
    p.value = htest$p.value,
    effect.size = effectsize,
    yates = yates,
    warn = need.warning
  )
  class(out) <- "assocTest"

  # throw the warning
  if (need.warning) {
    warning("Expected frequencies too small: chi-squared approximation may be incorrect")
  }

  return(out)
}


# print method

#' Print chi-square association test results
#'
#' @description Prints the results of a chi-square test of association in a
#' readable format. This function is called automatically whenever a result
#' from \code{\link{associationTest}} is displayed.
#'
#' @param x An association test result, as returned by
#'   \code{\link{associationTest}}.
#' @param ... Additional arguments (unused, included for compatibility).
#'
#' @return Invisibly returns \code{x} unchanged.
#' @export
print.assocTest <- function(x, ...) {
  nDigits <- 3

  # print the name of the test
  cat("\n     Chi-square test of categorical association\n\n")

  # print the data variable
  cat("Variables:  ", paste(x$variables, collapse = ", "), "\n\n")

  # print the hypotheses being tested
  cat("Hypotheses: \n")
  cat("   null:        variables are independent of one another\n")
  cat("   alternative: some contingency exists between variables\n")
  cat("\n")

  # print out the observed
  cat("Observed contingency table:\n")
  print(x$observed)
  cat("\n")

  # print out the expected
  cat("Expected contingency table under the null hypothesis:\n")
  print(x$expected, digits = nDigits)
  cat("\n")

  # print test statistics
  cat("Test results: \n")
  cat("   X-squared statistic: ", round(x$statistic, nDigits), "\n")
  cat("   degrees of freedom: ", round(x$df, nDigits), "\n")
  pp <- ifelse(x$p.value < .001, "<.001", round(x$p.value, nDigits))
  cat("   p-value: ", pp, "\n")
  cat("\n")

  # print other things
  cat("Other information: \n")
  cat("   estimated effect size (Cramer's v): ", round(x$effect.size, nDigits), "\n")
  if (x$yates) cat("   Yates' continuity correction has been applied\n")
  if (x$warn) cat("   warning: expected frequencies too small, results may be inaccurate\n")
  cat("\n")


  return(invisible(x))
}
