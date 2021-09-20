
# posthocPairwiseT() is actually just a wrapper to pairwise.t.test() that
# takes an aov object as input, rather than requiring the user to specify
# the variables
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
