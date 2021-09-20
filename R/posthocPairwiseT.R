
# posthocPairwiseT() is actually just a wrapper to pairwise.t.test() that
# takes an aov object as input, rather than requiring the user to specify
# the variables

#' Post-hoc pairwise t-tests for ANOVA
#'
#' @description Performs pairwise t-tests for an analysis of variance,
#' making corrections for multiple comparisons.
#'
#' @param x An \code{aov} object
#' @param ... Arguments to be passed to \code{pairwise.t.test}
#'
#' @details The intention behind this function is to allow users to use simple
#' tools for multiple corrections (e.g., Bonferroni, Holm) as post hoc
#' corrections in an ANOVA context, using the fitted model object (i.e., an
#' \code{aov} object) as the input. The reason for including this function is
#' that Tukey / Scheffe methods for constructing simultaneous confidence
#' intervals (as per \code{\link{TukeyHSD}}) are not often discussed in the
#' context of an introductory class, and the more powerful tools provided by
#' the \code{multcomp} package are not appropriate for students just beginning
#' to learn statistics.
#'
#' This function is currently just a wrapper function for
#' \code{\link{pairwise.t.test}}, and it only works for one-way ANOVA, but
#' this may change in future versions.
#'
#' @return As per \code{pairwise.t.test}
#' @export
#'
#' @seealso
#' \code{\link{pairwise.t.test}},
#' \code{\link{TukeyHSD}}
#'
#' @examples
#' # create the data set to analyse:
#' dataset <- data.frame(
#'   outcome = c( 1,2,3, 2,3,4, 5,6,7 ),
#'   group = factor(c( "a","a","a", "b","b","b","c","c","c"))
#' )
#'
#' # run the ANOVA and print out the ANOVA table:
#' anova1 <- aov( outcome ~ group, data = dataset )
#' summary(anova1)
#'
#' # Currently, the following two commands are equivalent:
#' posthocPairwiseT( anova1 )
#' pairwise.t.test( dataset$outcome, dataset$group )
#'
posthocPairwiseT <- function(x,...) {

  if( !methods::is(x,"aov") ) {stop( '"x" must be an aov object')}
  if( length(x$model)>2 ) stop( '"posthocPairwiseT" only supports one-way ANOVA' )

  # only works for a one-way anova!
  outcome <- x$model[[1]]
  group <- x$model[[2]]

  # run pairwise t.test
  out <- stats::pairwise.t.test(outcome, group, ...)

  # alter the names to match the original and return
  var.names <- names(x$model)
  out$data.name <- paste(var.names[1],"and",var.names[2])
  return(out)
}
