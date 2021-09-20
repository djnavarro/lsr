

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
#' @description Cuts a variable into equal sized categories
#'
#' @param x A vector containing the observations.
#' @param n Number of categories
#' @param ... Additional arguments to cut
#'
#' @description  It is sometimes convenient (though not always wise) to split a
#' continuous numeric variable \code{x} into a set of \code{n} discrete
#' categories that contain an approximately equal number of cases. The
#' \code{quantileCut} function does exactly this. The actual categorisation
#' is done by the \code{\link{cut}} function. However, instead of selecting
#' ranges of equal sizes (the default behaviour in \code{cut}), the
#' \code{quantileCut} function uses the \code{\link{quantile}} function to
#' select  unequal sized ranges so as to ensure that each of the categories
#' contains the same number of observations. The intended purpose of the
#' function is to assist in exploratory data analysis; it is not generally
#' a good idea to use the output of \code{quantileCut} function as a factor
#' in an analysis of variance, for instance, since the factor levels are not
#' interpretable and will almost certainly violate homogeneity of variance.
#'
#' @return A factor containing \code{n} levels. The factor levels are
#' determined in the same way as for the \code{cut} function, and can be
#' specified manually using the \code{labels} argument, which is passed to
#' the \code{cut} function.
#'
#' @export
#'
#' @seealso \code{\link{cut}}, \code{\link{quantile}}
#'
#' @examples
#' # An example illustrating why care is needed
#'
#' dataset <- c( 0,1,2, 3,4,5, 7,10,15 )       # note the uneven spread of data
#' x <- quantileCut( dataset, 3 )              # cut into 3 equally frequent bins
#' table(x)                                    # tabulate
#'
#' # For comparison purposes, here is the behaviour of the more standard cut
#' # function when applied to the same data:
#' y <- cut( dataset, 3 )
#' table(y)
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
