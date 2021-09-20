
#' One sample t-test
#'
#' @description Convenience function that runs a one sample t-test. This is
#' a wrapper function intended to be used for pedagogical purposes only.
#'
#' @param x The variable to be tested (required).
#' @param mu The value against which the mean should be tested (required).
#' @param one.sided One sided or two sided hypothesis test (default = \code{FALSE})
#' @param conf.level The confidence level for the confidence interval (default = .95).
#'
#' @details The \code{oneSampleTTest} function runs a one-sample t-test on
#' the data in \code{x}, and prints the results in a format that is easier
#' for novices to handle than the output of \code{t.test}. All the actual
#' calculations are done by the \code{t.test} and \code{cohensD} functions.
#'
#' As with the \code{t.test} function, the default test is two sided,
#' corresponding to a default value of \code{one.sided = FALSE}. To specify
#' a one sided test in which the alternative hypothesis is that \code{x} is
#' larger than \code{mu}, the input must be \code{one.sided = "greater"}.
#' Similarly, if \code{one.sided="less"}, then the alternative hypothesis
#' is that the mean of \code{x} is smaller than \code{mu}.
#'
#' @return An object of class 'TTest'. When printed, the output is organised
#' into five short sections. The first section lists the name of the test and
#' the variables included. The second provides means and standard deviations.
#' The third states explicitly what the null and alternative hypotheses were.
#' The fourth contains the test results: t-statistic, degrees of freedom and
#' p-value. The final section includes the relevant confidence interval and an
#' estimate of the effect size (i.e., Cohen's d).
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
  if( !methods::is(conf.level,"numeric") |
      length( conf.level) != 1 |
      conf.level < 0 |
      conf.level > 1
  ) {
    stop( '"conf.level" must be a number between 0 and 1')
  }

  ############ do the statistical calculations ############

  # check for missing data
  if( any( is.na(x))) {warning( paste( sum(is.na(x))), " case(s) removed due to missingness" ) }

  # run the ttest
  htest <- stats::t.test( x=x, mu=mu, alternative=one.sided )

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
