
#' Cohen's d
#'
#' @description Calculates the Cohen's d measure of effect size.
#'
#' @param x A numeric variable containing the data for group 1, or possibly a formula of the form \code{outcome ~ group}
#' @param y If \code{x} is a numeric variable, the \code{y} argument should be a numeric variable containing the data for group 2. If a one-sample calculation is desired, then no value for \code{y} should be specified.
#' @param data If \code{x} is a formula, then \code{data} is an optional argument specifying data frame containing the variables in the formula.
#' @param method Which version of the d statistic should we calculate? Possible values are \code{"pooled"} (the default), \code{"x.sd"}, \code{"y.sd"}, \code{"corrected"}, \code{"raw"}, \code{"paired"} and \code{"unequal"}. See below for specifics.
#' @param mu The "null" value against which the effect size should be measured. This is almost always 0 (the default), so this argument is rarely specified.
#' @param formula An alias for \code{x} if a formula input is used. Included for the sake of consistency with the \code{t.test} function.
#'
#' @details The \code{cohensD} function calculates the Cohen's d measure of
#' effect size in one of several different formats. The function is intended
#' to be called in one of two different ways, mirroring the \code{t.test}
#' function. That is, the first input argument \code{x} is a formula, then a
#' command of the form \code{cohensD(x = outcome~group, data = data.frame)}
#' is expected, whereas if \code{x} is a numeric variable, then a command of
#' the form \code{cohensD(x = group1, y = group2)} is expected.
#'
#' The \code{method} argument allows the user to select one of several
#' different variants of Cohen's d. Assuming that the original t-test for
#' which an effect size is desired was an independent samples t-test (i.e.,
#' not one sample or paired samples t-test), then there are several
#' possibilities for how the normalising term (i.e., the standard deviation
#' estimate) in Cohen's d should be calculated. The most commonly used
#' method is to use the same pooled standard deviation estimate that is
#' used in a Student t-test (\code{method = "pooled"}, the default). If
#' \code{method = "raw"} is used, then the same pooled standard deviation
#' estimate is used, except that the sample standard deviation is used
#' (divide by N) rather than the unbiased estimate of the population
#' standard deviation (divide by N-2). Alternatively, there may be reasons
#' to use only one of the two groups to estimate the standard deviation. To
#' do so, use \code{method = "x.sd"} to select the \code{x} variable, or
#' the first group listed in the grouping factor; and \code{method = "y.sd"}
#' to normalise by \code{y}, or the second group listed in the grouping
#' factor. The last of the "Student t-test" based measures is the unbiased
#' estimator of d (\code{method = "corrected"}), which multiplies the "pooled"
#' version by (N-3)/(N-2.25).
#'
#' For other versions of the t-test, there are two possibilities implemented.
#' If the original t-test did not make a homogeneity of variance assumption,
#' as per the Welch test, the normalising term should mirror the Welch test
#' (\code{method = "unequal"}). Or, if the original t-test was a paired samples
#' t-test, and the effect size desired is intended to be based on the standard
#' deviation of the differences, then \code{method = "paired"} should be used.
#'
#' The last argument to \code{cohensD} is \code{mu}, which represents the mean
#' against which one sample Cohen's d calculation should be assessed. Note that
#' this is a slightly narrower usage of \code{mu} than the \code{t.test}
#' function allows. \code{cohensD} does not currently support the use of a
#' non-zero \code{mu} value for a paired-samples calculation.
#'
#' @return Numeric variable containing the effect size, d. Note that it does
#' not show the direction of the effect, only the magnitude. That is, the value
#' of d returned by the function is always positive or zero.
#'
#' @references
#' Cohen, J. (1988). Statistical power analysis for the behavioral sciences
#' (2nd ed.). Hillsdale, NJ: Lawrence Erlbaum Associates.
#'
#' @export
#'
#' @examples
#' # calculate Cohen's d for two independent samples:
#' gradesA <- c(55, 65, 65, 68, 70) # 5 students with teacher A
#' gradesB <- c(56, 60, 62, 66)     # 4 students with teacher B
#' cohensD(gradesA, gradesB)
#'
#' # calculate Cohen's d for the same data, described differently:
#' grade <- c(55, 65, 65, 68, 70, 56, 60, 62, 66) # grades for all students
#' teacher <- c("A", "A", "A", "A", "A", "B", "B", "B", "B") # teacher for each student
#' cohensD(grade ~ teacher)
#'
#' # calculate Cohen's d for two paired samples:
#' pre  <- c(100, 122, 97, 25, 274) # a pre-treatment measure for 5 cases
#' post <- c(104, 125, 99, 29, 277) # the post-treatment measure for the same 5 cases
#' cohensD(pre, post, method = "paired") # ... explicitly indicate that it's paired, or else
#' cohensD(post - pre)  # ... do a "single-sample" calculation on the difference
#'
#' # support for data frames:
#' exams <- data.frame(grade, teacher)
#' cohensD(exams$grade ~ exams$teacher)    # using $
#' cohensD(grade ~ teacher, data = exams)  # using the 'data' argument
#'
cohensD <- function(x = NULL, y = NULL, data = NULL, method = "pooled",  mu = 0, formula=NULL ) {

  #### there's only a limited number of meaningful input patterns. ENFORCE this ###

  # determine which variables the user has input
  userInput <- !(c( x = missing(x),
                    y = missing(y),
                    data = missing(data),
                    method = missing(method),
                    mu = missing(mu),
                    formula = missing(formula)
                ))
  scenario <- "bad"

  # user inputs x
  if(all(  userInput == c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE)  )) {

    if( methods::is(x,"numeric")) {
      # good case 1: x=numeric -> single sample cohensD with mu=0
      scenario <- "one.sample"
    }

    if( methods::is(x,"formula")) {
      # good case 2: x=formula -> two sample cohensD with variables in workspace
      scenario <- "two.sample.formula"

      # since no y is specified, create it now
      y <- try( eval( stats::model.frame(formula = x), envir=parent.frame() ), silent=TRUE)
      if( methods::is(y,"try-error") ) {
        stop( "variables specified in the formula do not exist or are different lengths")
      }
    }
  }


  # good case 3: x=numeric, mu=numeric -> single sample cohensD
  if(all(  userInput == c(TRUE,FALSE,FALSE,FALSE,TRUE,FALSE)  )) { # user inputs x,mu
    if( methods::is(x,"numeric") ) { # check input type for x
      if( methods::is(mu,"numeric") & length(mu)==1 ) { # check input for mu
        scenario <- "one.sample"
      }
    }
  }


  # user inputs x,y
  if(all(  userInput == c(TRUE,TRUE,FALSE,FALSE,FALSE,FALSE)  )) {

    # good case 4: x=numeric, y=numeric -> two sample cohensD
    if( methods::is(x,"numeric") & methods::is(y,"numeric") ) {
      scenario <- "two.sample.grouped"
    }

    # good case 5: x=formula, y=df -> two sample cohensD from formula input
    if( methods::is(x,"formula") & methods::is(y,"data.frame") ) {
      scenario <- "two.sample.formula"
    }

  }

  # good case 6: x=formula, data=df -> two sample cohensD
  if(all(  userInput == c(TRUE,FALSE,TRUE,FALSE,FALSE,FALSE)  )) { # user inputs x,data
    if( methods::is(x,"formula") & methods::is(data,"data.frame") ) {
      y <- data
      data <- NULL
      scenario <- "two.sample.formula"
    }
  }

  # good case 7: formula=formula, data=df -> two sample cohensD
  if(all(  userInput == c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE)  )) { # user inputs formula,data
    if( methods::is(formula,"formula") & methods::is(data,"data.frame") ) {
      y <- data
      data <- NULL
      x <- formula
      formula <- NULL
      scenario <- "two.sample.formula"
    }
  }

  # good case 8: x=formula, method=character
  if(all(  userInput == c(TRUE,FALSE,FALSE,TRUE,FALSE,FALSE)  )) {

    if( methods::is(x,"formula") & methods::is(method,"character") & length(method)==1 ) {
      scenario <- "two.sample.formula"
      if( method=="paired") { # issue warning about case ordering...
        warning( "calculating paired samples Cohen's d using formula input. Results will be incorrect if cases do not appear in the same order for both levels of the grouping factor")
      }

      # since no y is specified, create it now
      y <- try( eval( stats::model.frame(formula = x), envir=parent.frame() ), silent=TRUE)
      if( methods::is(y,"try-error") ) {
        stop( "variables specified in the formula do not exist or are different lengths")
      }
    }
  }

  # user inputs x,y,method
  if(all(  userInput == c(TRUE,TRUE,FALSE,TRUE,FALSE,FALSE)  )) {

    # good case 9: x=numeric, y=numeric, method=character -> two sample cohensD
    if( methods::is(x,"numeric") & methods::is(y,"numeric") & methods::is(method,"character") & length(method)==1) {
      scenario <- "two.sample.grouped"
    }

    # good case 10: x=formula, y=df, method=character -> two sample cohensD from formula input
    if( methods::is(x,"formula") & methods::is(y,"data.frame") & methods::is(method,"character") & length(method)==1) {
      scenario <- "two.sample.formula"
      if( method=="paired") { # issue warning about case ordering...
        warning( "calculating paired samples Cohen's d using formula input. Results will be incorrect if cases do not appear in the same order for both levels of the grouping factor")
      }
    }

  }

  # good case 11: x=formula, data=df, method=character -> two sample cohensD
  if(all(  userInput == c(TRUE,FALSE,TRUE,TRUE,FALSE,FALSE)  )) { # user inputs x,data,method
    if( methods::is(x,"formula") & methods::is(data,"data.frame") & methods::is(method,"character") & length(method)==1 ) {
      y <- data
      data <- NULL
      scenario <- "two.sample.formula"
      if( method=="paired") { # issue warning about case ordering...
        warning( "calculating paired samples Cohen's d using formula input. Results will be incorrect if cases do not appear in the same order for both levels of the grouping factor")
      }
    }
  }

  # good case 12: formula=formula, data=df, method=character -> two sample cohensD
  if(all(  userInput == c(FALSE,FALSE,TRUE,TRUE,FALSE,TRUE)  )) { # user inputs formula,data,method
    if( methods::is(formula,"formula") & methods::is(data,"data.frame") & methods::is(method,"character") & length(method)==1 ) {
      y <- data
      data <- NULL
      x <- formula
      formula <- NULL
      scenario <- "two.sample.formula"
      if( method=="paired") { # issue warning about case ordering...
        warning( "calculating paired samples Cohen's d using formula input. Results will be incorrect if cases do not appear in the same order for both levels of the grouping factor")
      }
    }
  }

  # good case 13: formula=formula, method=character
  if(all(  userInput == c(FALSE,FALSE,FALSE,TRUE,FALSE,TRUE)  )) {

    if( methods::is(formula,"formula") & methods::is(method,"character") & length(method)==1 ) {
      x <- formula
      formula <- NULL
      scenario <- "two.sample.formula"
      if( method=="paired") { # issue warning about case ordering...
        warning( "calculating paired samples Cohen's d using formula input. Results will be incorrect if cases do not appear in the same order for both levels of the grouping factor")
      }

      # since no y is specified, create it now
      y <- try( eval( stats::model.frame(formula = x), envir=parent.frame() ), silent=TRUE)
      if( methods::is(y,"try-error") ) {
        stop( "variables specified in the formula do not exist or are different lengths")
      }
    }
  }

  # good case 14: formula=formula
  if(all(  userInput == c(FALSE,FALSE,FALSE,FALSE,FALSE,TRUE)  )) {

    if( methods::is(formula,"formula") ) {
      x <- formula
      formula <- NULL
      scenario <- "two.sample.formula"

      # since no y is specified, create it now
      y <- try( eval( stats::model.frame(formula = x), envir=parent.frame() ), silent=TRUE)
      if( methods::is(y,"try-error") ) {
        stop( "variables specified in the formula do not exist or are different lengths")
      }
    }
  }

  # good case 15: formula=formula, x=df -> two sample cohensD
  if(all(  userInput == c(TRUE,FALSE,FALSE,FALSE,FALSE,TRUE)  )) { # user inputs formula,x
    if( methods::is(formula,"formula") & methods::is(x,"data.frame") ) {
      y <- x
      x <- formula
      formula <- NULL
      scenario <- "two.sample.formula"
    }
  }

  # good case 16: formula=formula, x=df, method=character -> two sample cohensD
  if(all(  userInput == c(TRUE,FALSE,FALSE,TRUE,FALSE,TRUE)  )) { # user inputs formula,x,method
    if( methods::is(formula,"formula") & methods::is(x,"data.frame") & methods::is(method,"character") & length(method)==1 ) {
      y <- x
      x <- formula
      formula <- NULL
      scenario <- "two.sample.formula"
      if( method=="paired") { # issue warning about case ordering...
        warning( "calculating paired samples Cohen's d using formula input. Results will be incorrect if cases do not appear in the same order for both levels of the grouping factor")
      }
    }
  }

  ####### throw error if scenario is bad  #######
  if( scenario == "bad" ) {
    stop( "arguments specified do not appear to correspond to a meaningful cohen's d calculation")
  }


  ####### run one-sample calculation if scenario is one sample  #######
  if ( scenario == "one.sample" ) {
    x <- x[!is.na(x)]
    d <- abs(mean(x) - mu) / stats::sd(x)
    return( d )
  }

  ####### convert to grouped data if scenario is two sample formula  #######
  if( scenario == "two.sample.formula" ) {

    outcome <- eval( x[[2]], y ) # not super happy about using eval here...
    group <- eval( x[[3]], y )
    group <- as.factor(group)
    if (nlevels(group) != 2L) {
      stop("grouping factor must have exactly 2 levels")
    }
    x <- split(outcome,group)
    y <- x[[2]]
    x <- x[[1]]
    scenario <- "two.sample.grouped"

  }

  ######## do a two sample calculation ########
  if( scenario == "two.sample.grouped" ) {

    # check method
    if( !( method %in% c("x.sd","y.sd","pooled","corrected","raw","paired","unequal" ))) {
      stop( '"method" must be "x.sd","y.sd","pooled","corrected","raw","paired" or "unequal"')
    }

    # remove missing data
    if( method == "paired" ) { # remove complete cases for paired
      if( length(x) != length(y) ) {
        stop( "paired samples cohen's d requires samples of the same size")
      }
      ind <- !is.na(x) & !is.na(y)
      x <- x[ind]
      y <- y[ind]
    } else { # remove individual cases for non-paired
      x <- x[!is.na(x)]
      y <- y[!is.na(y)]
    }


    # function to compute a pooled standard deviation
    pooledSD <- function(x,y,debias = TRUE) {
      sq.devs <- (c( x-mean(x), y-mean(y) ))^2
      n <- length(sq.devs)
      if(debias) { psd <- sqrt(sum(sq.devs)/(n-2)) }
      else { psd <- sqrt(sum(sq.devs)/n)}
      return(psd)
    }

    # calculate cohens d
    mean.diff <- mean(x) - mean(y)
    sd.est <- switch( EXPR = method,
                      "x.sd" = stats::sd(x),
                      "y.sd" = stats::sd(y),
                      "pooled" = pooledSD(x,y),
                      "corrected" = pooledSD(x,y),
                      "raw" = pooledSD(x,y,FALSE),
                      "paired" = stats::sd(x-y),
                      "unequal" = sqrt( (stats::var(x)+stats::var(y))/2 )
    )
    d <- mean.diff / sd.est
    if( method == "corrected") {
      n <- length(x) + length(y)
      d <- d * (n - 3)/(n-2.25)
    }

    return(abs(d))
  }


  # check
  stop( "how did I get here?")

}
