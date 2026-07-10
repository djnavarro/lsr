
#' One sample t-test
#'
#' @description Runs a one-sample t-test and prints the results in a
#' readable format.
#'
#' @param x A numeric vector containing the data to be tested.
#' @param mu The hypothesised population mean to test against.
#' @param one.sided Set to \code{FALSE} (default) for a two-sided test. Set
#'   to \code{"greater"} if you expect the population mean to be above
#'   \code{mu}, or \code{"less"} if you expect it to be below.
#' @param conf.level The confidence level for the confidence interval.
#'   The default is \code{0.95} for a 95\% interval.
#'
#' @details Runs a one-sample t-test comparing the mean of \code{x} to the
#' hypothesised value \code{mu}, and prints the results in a beginner-friendly
#' format. The calculations are done by \code{\link{t.test}} and
#' \code{\link{cohensD}}. Missing values in \code{x} are removed with a
#' warning.
#'
#' @return Prints a summary showing the variable name, descriptive statistics,
#' null and alternative hypotheses, test results (t-statistic, degrees of
#' freedom, p-value), a confidence interval, and Cohen's d as a measure of
#' effect size. The underlying results are also returned as a list, so the
#' output can be assigned to a variable and inspected if needed.
#'
#' @export
#'
#' @seealso
#' \code{\link{t.test}},
#' \code{\link{pairedSamplesTTest}},
#' \code{\link{independentSamplesTTest}},
#' \code{\link{cohensD}}
#'
#' @examples
#'
#' likert <- c(3,1,4,1,4,6,7,2,6,6,7)
#'
#' oneSampleTTest( x = likert, mu = 4 )
#' oneSampleTTest( x = likert, mu = 4, one.sided = "greater" )
#' oneSampleTTest( x = likert, mu = 4, conf.level=.99 )
#'
#' likert <- c(3,NA,4,NA,4,6,7,NA,6,6,7)
#' oneSampleTTest( x = likert, mu = 4 )
#'
oneSampleTTest <- function(
  x,
  mu,
  one.sided = FALSE,
  conf.level=.95
){

  ############ check x and mu ############

  # check that the user has specified x and mu
  if( missing(x) ) { stop( '"x" argument is missing, with no default')}
  if( missing(mu) ) { stop( '"mu" argument is missing, with no default')}

  # check x
  if( !methods::is(x,"numeric")) {
    stop( '"x" must be numeric')
  }

  # check mu
  if( !methods::is(mu,"numeric") | length(mu) !=1 ) {
    stop( '"mu" must be a single number')
  }

  ############ check other input arguments ############

  # check alternative
  if( length(one.sided) !=1 ) stop( "invalid value for 'one.sided'" )
  if( one.sided != FALSE && !(one.sided %in% c("less","greater")) ) {
    stop( "invalid value for 'one.sided'" )
  }
  if( one.sided == FALSE ) { one.sided = "two.sided" }

  # check conf.level
  if( !methods::is(conf.level,"numeric") ||
      length( conf.level) != 1 ||
      is.na(conf.level) ||
      conf.level < 0 ||
      conf.level > 1
  ) {
    stop( '"conf.level" must be a number between 0 and 1')
  }

  ############ do the statistical calculations ############

  # check for missing data
  if( any( is.na(x))) {warning( paste( sum(is.na(x))), " case(s) removed due to missingness" ) }

  # run the ttest
  htest <- stats::t.test( x=x, mu=mu, alternative=one.sided, conf.level=conf.level )

  # get cohensD
  d <- cohensD( x=x, mu=mu )


  ############ output ############

  # get the variable name if it exists
  outcome <- match.call()[2] # get the x argument from the call
  outcome <- as.character( outcome )

  # create output structure
  TT <- list(
    t.statistic = htest$statistic,
    df = htest$parameter,
    p.value = htest$p.value,
    conf.int = htest$conf.int,
    conf = conf.level,
    mean = mean(x, na.rm=TRUE),
    sd = stats::sd(x, na.rm=TRUE),
    outcome = outcome,
    group = NULL,
    group.names = NULL,
    id = NULL,
    mu = mu,
    alternative = one.sided,
    method = "One sample t-test",
    effect.size = d
  )

  # specify the class and ouput
  class(TT) <- "TTest"
  return(TT)


}
