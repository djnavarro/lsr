#' Mean absolute deviation
#'
#' @description Calculates the mean absolute deviation from the sample mean.
#'
#' @param x A numeric vector containing the observations.
#' @param na.rm Set to \code{TRUE} to remove missing values before computing
#'   the deviation. Defaults to \code{FALSE}.
#'
#' @details Computes the average of the absolute differences between each
#' observation and the sample mean of \code{x}, i.e.
#' \code{mean(abs(x - mean(x)))}.
#'
#' @return A single number giving the mean absolute deviation.
#'
#' @seealso \code{\link{mean}}, \code{\link{sd}}, \code{\link{var}}
#'
#' @export
#'
#' @examples
#' x <- c(1, 3, 6)
#' aad(x)
#'
#' # missing values
#' x <- c(1, 3, NA, 6)
#' aad(x) # returns NA
#' aad(x, na.rm = TRUE) # ignores the missing value
#'
aad <- function(x, na.rm = FALSE) {
  if (!methods::is(x, "numeric") & !methods::is(x, "integer")) {
    stop('"x" must be numeric')
  }
  if (!methods::is(na.rm, "logical") || length(na.rm) != 1 || is.na(na.rm)) {
    stop('"na.rm" must be a single logical value')
  }
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  y <- mean(abs(x - mean(x)))
  return(y)
}
