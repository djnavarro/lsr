#' Independent samples t-test
#'
#' @description Runs an independent-samples t-test and prints the results in
#' a readable format.
#'
#' @param formula A formula of the form \code{outcome ~ group}, where
#'   \code{outcome} is the numeric variable being measured and \code{group}
#'   is a factor with exactly two levels.
#' @param data An optional data frame containing the variables named in
#'   \code{formula}. Tibbles are accepted and converted automatically. If
#'   \code{data} is omitted the variables are looked up in the workspace.
#' @param var.equal Set to \code{TRUE} to run Student's t-test, which assumes
#'   equal group variances. The default (\code{FALSE}) runs Welch's t-test,
#'   which is safer when variances may differ between groups.
#' @param one.sided Set to \code{FALSE} (default) for a two-sided test. Set
#'   to the name of the group expected to have the larger mean for a one-sided
#'   test (e.g., \code{one.sided = "group2"}).
#' @param conf.level The confidence level for the confidence interval.
#'   The default is \code{0.95} for a 95\% interval.
#'
#' @details Runs an independent-samples t-test comparing the means of two
#' groups, and prints the results in a beginner-friendly format. The
#' calculations are done by \code{\link{t.test}} and \code{\link{cohensD}}.
#' When \code{var.equal = TRUE}, Cohen's d uses a pooled standard deviation;
#' when \code{var.equal = FALSE} (Welch's test), it uses the "unequal" method.
#' Cases with missing values are removed with a warning.
#'
#' @return Prints a summary showing the outcome and grouping variable names,
#' group means and standard deviations, null and alternative hypotheses, test
#' results (t-statistic, degrees of freedom, p-value), a confidence interval,
#' and Cohen's d as a measure of effect size. The underlying results are also
#' returned as a list, so the output can be assigned to a variable and
#' inspected if needed.
#'
#' @seealso
#' \code{\link{t.test}},
#' \code{\link{oneSampleTTest}},
#' \code{\link{pairedSamplesTTest}},
#' \code{\link{cohensD}}
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   rt = c(451, 562, 704, 324, 505, 600, 829),
#'   cond = factor(x = c(1, 1, 1, 2, 2, 2, 2), labels = c("group1", "group2"))
#' )
#'
#' # Welch's t-test (the default, does not assume equal variances)
#' independentSamplesTTest(rt ~ cond, df)
#'
#' # Student's t-test (assumes equal variances)
#' independentSamplesTTest(rt ~ cond, df, var.equal = TRUE)
#'
#' # one-sided test: is group1 larger?
#' independentSamplesTTest(rt ~ cond, df, one.sided = "group1")
#'
#' # missing values are removed with a warning
#' df$rt[1] <- NA
#' df$cond[7] <- NA
#' independentSamplesTTest(rt ~ cond, df)
#'
independentSamplesTTest <- function(
  formula,
  data = NULL,
  var.equal = FALSE,
  one.sided = FALSE,
  conf.level = .95
) {
  ############ check  formula ############

  # check that the user has input a formula
  if (missing(formula)) {
    stop('"formula" argument is missing, with no default')
  }
  if (!methods::is(formula, "formula")) {
    stop('"formula" argument must be a formula')
  }

  # the formula must be of the form DV ~ IV
  if (length(formula) != 3) stop('invalid value for "formula" argument')
  vars <- all.vars(formula)
  if (length(vars) != 2) stop('invalid value for "formula" argument')

  # read off the names of the variables
  outcome <- vars[1]
  group <- vars[2]


  ############ check data frame ############

  if (!missing(data)) {
    # it needs to be data frame, because a matrix can't
    # contain both factors and numeric variables
    if (!methods::is(data, "data.frame")) stop("'data' is not a data frame")
    data <- as.data.frame(data)

    # check that all three variables are in the data frame
    if (!(outcome %in% names(data))) {
      stop(paste0("'", outcome, "' is not the name of a variable in '", deparse(substitute(data)), "'"))
    }
    if (!(group %in% names(data))) {
      stop(paste0("'", group, "' is not the name of a variable in '", deparse(substitute(data)), "'"))
    }
  } else {
    # check that all variables exist in the workspace
    workspace <- objects(parent.frame())

    # check that all three variables are in the data frame
    if (!(outcome %in% workspace)) {
      stop(paste0("'", outcome, "' is not the name of a variable in the workspace"))
    }
    if (!(group %in% workspace)) {
      stop(paste0("'", group, "' is not the name of a variable in the workspace"))
    }

    # copy variables into a data frame if none is specified, and
    # check that the variables are appropriate for a data frame
    data <- try(eval(stats::model.frame(formula = formula, na.action = stats::na.pass),
      envir = parent.frame()
    ), silent = TRUE)
    if (methods::is(data, "try-error")) {
      stop("specified variables cannot be coerced to data frame")
    }
  }

  # subset the data frame
  data <- data[, c(outcome, group)]

  ############ check classes for outcome, group and id ############

  # at this point we have a data frame that is known to contain
  # outcome and group. Now check that they are of the appropriate
  # type to run a t-test

  # outcome must be numeric
  if (!methods::is(data[, outcome], "numeric")) stop("outcome variable must be numeric")

  # group should be a factor with two-levels. issue warnings if it only
  # has two unique values but isn't a factor, or is a factor with more
  # than two levels but only uses two of them.

  if (methods::is(data[, group], "factor")) { # it's a factor

    if (nlevels(data[, group]) < 2) { # fewer than two levels
      stop("grouping variable does not have two distinct levels")
    }

    if (nlevels(data[, group]) > 2) { # more than two levels
      if (length(unique(data[, group])) == 2) { # but only two of them are used...
        warning("grouping variable has unused factor levels")
        data[, group] <- droplevels(data[, group])
      } else { # too many levels in use
        stop("grouping variable has more than two distinct values")
      }
    }
  } else { # it's not a factor

    if (length(unique(data[, group])) == 2) { # if it happens to have 2 unique values...
      warning("group variable is not a factor") # warn the user
      data[, group] <- as.factor(data[, group]) # coerce and continue...
    } else {
      stop("grouping variable must contain only two unique values (and should be a factor)")
    }
  }


  ############ check other inputs ############

  # group names
  gp.names <- levels(data[, group])

  # check alternative
  if (length(one.sided) != 1) stop("invalid value for 'one.sided'")
  if (one.sided == FALSE) { # two sided
    alternative <- "two.sided"
  } else {
    if (one.sided == gp.names[1]) { # first factor level
      alternative <- "greater"
    } else {
      if (one.sided == gp.names[2]) { # second factor level
        alternative <- "less"
      } else {
        stop("invalid value for 'one.sided'")
      }
    }
  }


  # check conf.level
  if (!methods::is(conf.level, "numeric") ||
    length(conf.level) != 1 ||
    is.na(conf.level) ||
    conf.level < 0 ||
    conf.level > 1
  ) {
    stop('"conf.level" must be a number between 0 and 1')
  }


  ############ do the statistical calculations ############

  # find cases with missing data
  missing <- apply(is.na(data), 1, any)
  if (any(missing)) warning(paste(sum(missing)), " case(s) removed due to missingness")
  data <- data[!missing, ]

  # pass to t.test
  htest <- stats::t.test(formula,
    data = data, var.equal = var.equal,
    alternative = alternative, conf.level = conf.level
  )

  # group means
  gp.means <- htest$estimate
  names(gp.means) <- gp.names

  # group standard deviations
  gp.sd <- stats::aggregate(formula, data, FUN = stats::sd)[[2]]

  # pass to cohens d
  if (var.equal) {
    var.method <- "pooled"
  } else {
    var.method <- "unequal"
  }
  d <- cohensD(formula = formula, data = data, method = var.method)


  ############ output ############

  # create output structure
  TT <- list(
    t.statistic = htest$statistic,
    df = htest$parameter,
    p.value = htest$p.value,
    conf.int = htest$conf.int,
    conf = conf.level,
    mean = gp.means,
    sd = gp.sd,
    outcome = outcome,
    group = group,
    group.names = gp.names,
    id = NULL,
    mu = NULL,
    alternative = alternative,
    method = ifelse(var.equal,
      yes = "Student's independent samples t-test",
      no = "Welch's independent samples t-test"
    ),
    effect.size = d
  )

  # specify the class and ouput
  class(TT) <- "TTest"
  return(TT)
}
