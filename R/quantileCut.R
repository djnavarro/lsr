

# quantileCut() splits the data x into n equally sized groups. Use with care:
# it is very frequently the case that people want to break a variable into several
# equally sized groups in order to force a continuous variable into the ANOVA
# framework. Much as I recognise the intuitive appeal of this, I'm not convinced
# it's actually a sensible data analysis strategy *unless* the groups that emerge
# from this automatic-grouping strategy are actually meaningful. I've included the
# function because it's something that many students have requested, but as I say,
# it should be used with care.

#' Cut by quantiles
#'
#' @description Divides a numeric variable into \code{n} categories that each
#' contain approximately the same number of observations.
#'
#' @param x A numeric vector.
#' @param n The number of categories to create.
#' @param ... Additional arguments passed to \code{\link{cut}}, such as
#'   \code{labels}.
#'
#' @details Unlike \code{\link{cut}}, which creates categories of equal width,
#' \code{quantileCut} uses \code{\link{quantile}} to find breakpoints that
#' produce roughly equal-sized groups. This can be useful in exploratory
#' analysis, but the resulting categories are data-driven and may not have
#' a clear interpretation. Using them as grouping variables in an ANOVA is
#' generally not recommended, as the breakpoints are arbitrary and the groups
#' will typically not have equal variances.
#'
#' @return A factor with \code{n} levels. Level labels follow the same
#' convention as \code{\link{cut}} and can be overridden with the
#' \code{labels} argument.
#'
#' @seealso \code{\link{cut}}, \code{\link{quantile}}
#'
#' @export
#'
#' @examples
#' # the data are unevenly spread, so equal-width bins would be unbalanced
#' x <- c(0, 1, 2, 3, 4, 5, 7, 10, 15)
#'
#' # quantileCut creates equal-frequency bins
#' bins_eq_freq <- quantileCut(x, 3)
#' table(bins_eq_freq)
#'
#' # compare to cut(), which creates equal-width bins
#' bins_eq_width <- cut(x, 3)
#' table(bins_eq_width)
#'
quantileCut <- function(x, n, ...) {

  if( !is.vector(x) | !methods::is(x,"numeric")) stop( '"x" must be a numeric vector')
  if( length(n) !=1 | !methods::is(n,"numeric")) stop( 'number of bins "n" must be a single number')

  p <- seq(0,1,1/n)
  breaks <- stats::quantile( x, p, na.rm=TRUE )
  eps <- (max(x, na.rm=TRUE)-min(x, na.rm=TRUE)) / 1000
  breaks[1] <- breaks[1] - eps
  breaks[n+1] <- breaks[n+1] + eps
  out <- cut(x, breaks, ...)
  return( out )
}
