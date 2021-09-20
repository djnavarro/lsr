

#' Mean (average) absolute deviation from the mean
#'
#' @description Calculates the mean absolute deviation from the sample mean
#'
#' @param x A vector containing the observations.
#' @param na.rm A logical value indicating whether or not missing values should be removed. Defaults to \code{FALSE}
#'
#' @details The \code{aad} function calculates the average (i.e. mean)
#' absolute deviation from the mean value of \code{x}, removing \code{NA}
#' values if requested by the user. It exists primarily to simplify the
#' discussion of descriptive statistics during an introductory stats class.
#'
#' @return Numeric
#' @export
#'
#' @examples
#' # basic usage
#' X <- c(1, 3, 6)  # data
#' aad(X)           # returns a value of 1.777
#'
#' # removing missing data
#' X <- c(1, 3, NA, 6)   # data
#' aad(X)                # returns NA
#' aad(X, na.rm = TRUE)  # returns 1.777
#'
aad <- function(x, na.rm = FALSE) {
  if ( !methods::is(x,"numeric") & !methods::is(x,"integer") ) {
    stop( '"x" must be numeric')
  }
  if( !methods::is(na.rm,"logical") | length(na.rm) !=1 ) {
    stop( '"na.rm" must be a single logical value')
  }
  if (na.rm) { x <- x[!is.na(x)] }
  y <- mean( abs(x - mean(x)) )
  return(y)
}

