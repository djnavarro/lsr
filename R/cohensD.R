
#' Cohen's d
#'
#' @description Calculates the Cohen's d measure of effect size.
#'
#' @param x A numeric vector of data for group 1, or a formula of the form
#'   \code{outcome ~ group} (in which case \code{data} can be used to supply
#'   a data frame).
#' @param y A numeric vector of data for group 2. Omit for a one-sample
#'   calculation.
#' @param data An optional data frame containing the variables in \code{x}
#'   when \code{x} is a formula.
#' @param method Which version of Cohen's d to calculate. Options are
#'   \code{"pooled"} (default), \code{"x.sd"}, \code{"y.sd"},
#'   \code{"corrected"}, \code{"raw"}, \code{"paired"}, and \code{"unequal"}.
#'   See Details.
#' @param mu The null value for a one-sample calculation. Almost always 0
#'   (the default).
#' @param formula A formula of the form \code{outcome ~ group}. This is an
#'   alternative way to supply the formula instead of using \code{x}.
#'
#' @details The function can be used in two main ways. For two separate
#' numeric vectors, call \code{cohensD(x = group1, y = group2)}. For data in
#' a data frame with a grouping variable, use a formula:
#' \code{cohensD(outcome ~ group, data = mydata)}.
#'
#' The \code{method} argument controls how the standard deviation is estimated:
#' \describe{
#'   \item{\code{"pooled"}}{Pooled SD from both groups (matches Student's
#'     t-test). This is the default.}
#'   \item{\code{"corrected"}}{Bias-corrected version of \code{"pooled"},
#'     multiplied by \code{(N-3)/(N-2.25)}.}
#'   \item{\code{"raw"}}{Like \code{"pooled"} but divides by N rather than
#'     N-2.}
#'   \item{\code{"x.sd"}}{SD of the first group only.}
#'   \item{\code{"y.sd"}}{SD of the second group only.}
#'   \item{\code{"unequal"}}{Square root of the average of the two group
#'     variances (matches Welch's t-test).}
#'   \item{\code{"paired"}}{SD of the within-person differences (matches a
#'     paired-samples t-test).}
#' }
#'
#' For a one-sample calculation, supply only \code{x} (and optionally
#' \code{mu}). The result is \code{abs(mean(x) - mu) / sd(x)}.
#'
#' @return A single positive number: the magnitude of the effect size d.
#' The sign of the mean difference is dropped, so the value is always
#' zero or greater.
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
