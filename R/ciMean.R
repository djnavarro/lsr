# ciMean() computes a confidence interval around the sample mean, under the
# usual assumption of normality.

#' Confidence interval around the mean
#'
#' @description Calculates a confidence interval for the mean of a numeric
#' variable (or each numeric variable in a data frame or matrix).
#'
#' @param x A numeric vector, matrix, or data frame.
#' @param conf The confidence level. Defaults to \code{0.95} for a 95\%
#'   interval.
#' @param na.rm Set to \code{TRUE} to remove missing values before computing
#'   the interval. Defaults to \code{FALSE}.
#'
#' @details Calculates a confidence interval for the mean under the standard
#' assumption that the data are normally distributed. When \code{x} is a
#' matrix or data frame, a separate interval is computed for each column.
#' Non-numeric columns in a data frame produce \code{NA} rows in the output.
#'
#' @return A matrix with one row per variable and two columns giving the lower
#' and upper bounds of the confidence interval. Column names reflect the
#' confidence level (e.g., \code{"2.5\%"} and \code{"97.5\%"} for a 95\%
#' interval).
#'
#' @seealso
#' \code{\link{t.test}},
#' \code{\link{mean}}
#'
#' @export
#'
#' @examples
#' x <- c(1, 3, 6)
#' ciMean(x) # 95% confidence interval
#' ciMean(x, conf = 0.80) # 80% confidence interval
#'
#' # for comparison: equivalent result via lm
#' confint(lm(x ~ 1))
#'
#' # missing values
#' x <- c(1, 3, NA, 6)
#' ciMean(x, na.rm = TRUE)
#'
ciMean <- function(x, conf = .95, na.rm = FALSE) {
  ############  check input  ############

  # check x
  if (missing(x)) {
    stop('argument "x" is missing, with no default')
  }
  if (!methods::is(x, "integer") & !methods::is(x, "numeric") & !methods::is(x, "matrix") & !methods::is(x, "data.frame")) {
    stop('"x" must be numeric, matrix or data frame')
  }

  # check conf
  if (!methods::is(conf, "numeric")) {
    stop('"conf" must be numeric')
  }
  if (length(conf) != 1) {
    stop('"conf" must be a single number')
  }
  if (conf < 0 | conf > 1) {
    stop('"conf" must be between 0 and 1')
  }

  # check na.rm
  if (!methods::is(na.rm, "logical")) {
    stop('"na.rm" must be logical')
  }
  if (length(na.rm) != 1) {
    stop('"na.rm" must be of length 1')
  }
  if (!(na.rm %in% c(TRUE, FALSE))) {
    stop('"na.rm" must be TRUE or FALSE')
  } # no NA!


  ############  function for a single ci  ############

  getNames <- function(quantiles) {
    paste(100 * quantiles, "%", sep = "")
  }

  computeCI <- function(x, conf, na.rm) {
    # remove missing data if requested
    if (na.rm) {
      x <- x[!is.na(x)]
    }

    # key quantities
    quantiles <- c((1 - conf) / 2, (1 + conf) / 2) # quantiles of t
    n <- length(x) # sample size

    # calculate CI
    if (length(x) < 2 | any(is.na(x))) {
      CI <- c(NA, NA) # undefined ci
    } else {
      CI <- mean(x) + stats::qt(p = quantiles, df = n - 1) * stats::sd(x) / sqrt(n) # normal CI
      if (stats::sd(x) == 0) warning("data have zero variance")
    }

    # assign names and return
    names(CI) <- getNames(quantiles)
    return(CI)
  }

  ############  handle different types of input  ############


  # handle numeric input
  if (methods::is(x, "integer") | methods::is(x, "numeric")) {
    ci <- computeCI(x, conf, na.rm) # compute the ci
    out <- matrix(NA, 1, 2) # set up output matrix
    out[1, ] <- ci # insert data
    colnames(out) <- names(ci) # column names
    nn <- match.call()[[2]] # get the x argument from the call
    if (is.name(nn)) { # if there is a variable name...
      rownames(out) <- as.character(nn) # ... add it as a row
    }
    return(out)
  }

  # handle matrix input
  if (methods::is(x, "matrix")) {
    if (mode(x) != "numeric") {
      stop("matrix input must be numeric")
    }
    d <- dim(x)
    out <- matrix(NA, nrow = d[2], ncol = 2)
    for (v in 1:d[2]) {
      ci <- computeCI(x[, v], conf, na.rm)
      out[v, ] <- ci
    }
    rownames(out) <- colnames(x)
    colnames(out) <- names(ci)
    return(out)
  }

  # handle data frame input
  if (methods::is(x, "data.frame")) {
    d <- dim(x)
    out <- matrix(NA, nrow = d[2], ncol = 2)
    nn <- names(x)
    for (v in 1:d[2]) {
      if (is.numeric(x[[v]])) {
        ci <- computeCI(x[[v]], conf, na.rm)
        out[v, ] <- ci
      } else {
        nn[v] <- paste0(nn[v], "*")
      }
    }
    rownames(out) <- nn
    quantiles <- c((1 - conf) / 2, (1 + conf) / 2)
    colnames(out) <- getNames(quantiles)
    return(out)
  }

  # throw error
  stop("hey, how did I get here?")
}
