
# posthocPairwiseT() is actually just a wrapper to pairwise.t.test() that
# takes an aov object as input, rather than requiring the user to specify
# the variables

#' Post-hoc pairwise t-tests for ANOVA
#'
#' @description Runs pairwise t-tests for a one-way analysis of variance,
#' with corrections for multiple comparisons.
#'
#' @param x An \code{aov} object, as returned by \code{\link{aov}}. Only
#'   one-way ANOVA models are supported.
#' @param ... Additional arguments passed to \code{\link{pairwise.t.test}},
#'   such as \code{p.adjust.method}.
#'
#' @details Takes a fitted one-way ANOVA object and runs pairwise t-tests for
#' all pairs of groups, applying a correction for multiple comparisons. This
#' is a simpler alternative to \code{\link{TukeyHSD}} that uses the same
#' correction methods (e.g., Holm, Bonferroni) as
#' \code{\link{pairwise.t.test}}.
#'
#' @return Prints a table of p-values for all pairwise group comparisons. The
#' underlying result is also returned as a list (with the same structure as
#' \code{\link{pairwise.t.test}}) so it can be assigned to a variable and
#' inspected if needed.
#'
#' @seealso
#' \code{\link{pairwise.t.test}},
#' \code{\link{TukeyHSD}},
#' \code{\link{aov}}
#'
#' @export
#'
#' @examples
#' dataset <- data.frame(
#'   outcome = c(1, 2, 3, 2, 3, 4, 5, 6, 7),
#'   group = factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c"))
#' )
#'
#' anova1 <- aov(outcome ~ group, data = dataset)
#' summary(anova1)
#'
#' # post-hoc pairwise comparisons with Holm correction (the default)
#' posthocPairwiseT(anova1)
#'
#' # Bonferroni correction instead
#' posthocPairwiseT(anova1, p.adjust.method = "bonferroni")
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
