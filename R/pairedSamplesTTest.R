
# Removed from earlier version...
#
# \section{Note}{\code{pairedSamplesTTest} also supports an even more
# "lme4"-like method for specifying the model in the \code{formula} argument.
# That is, \code{outcome ~ group + (1|id)} is deemed to be equivalent to
# \code{outcome ~ group + (id)}. This may be removed in future versions.}


#' Paired samples t-test
#'
#' @description Convenience function that runs a paired samples t-test. This
#' is a wrapper function intended to be used for pedagogical purposes only.
#'
#' @param formula Formula specifying the outcome and the groups (required).
#' @param data Optional data frame containing the variables.
#' @param id The name of the id variable (must be a character string).
#' @param one.sided One sided or two sided hypothesis test (default = \code{FALSE})
#' @param conf.level The confidence level for the confidence interval (default = .95).
#'
#' @details The \code{pairedSamplesTTest} function runs a paired-sample t-test,
#' and prints the results in a format that is easier for novices to handle than
#' the output of \code{t.test}. All the actual calculations are done by the
#' \code{t.test} and \code{cohensD} functions.
#'
#' There are two different ways of specifying the formula, depending on whether
#' the data are in wide form or long form. If the data are in wide form, then
#' the input should be a one-sided formula of the form
#' \code{~ variable1 + variable2}. The \code{id} variable is not required: the
#' first element of \code{variable1} is paired with the first element of
#' \code{variable2} and so on. Both \code{variable1} and \code{variable2} must
#' be numeric.
#'
#' If the data are in long form, a two sided formula is required. The simplest
#' way to specify the test is to input a formula of the form
#' \code{outcome ~ group + (id)}. The term in parentheses is assumed to be
#' the \code{id} variable, and must be a factor. The \code{group} variable
#' must be a factor with two levels (if there are more than two levels but
#' only two are used in the data, a warning is given). The \code{outcome}
#' variable must be numeric.
#'
#' The reason for using the \code{outcome ~ group + (id)} format is that it is
#' broadly consistent with the way repeated measures analyses are specified
#' in the \code{lme4} package. However, this format may not appeal to some
#' people for teaching purposes. Given this, the \code{pairedSamplesTTest}
#' also supports a simpler formula of the form \code{outcome ~ group}, so
#' long as the user specifies the \code{id} argument: this must be a
#' character vector specifying the name of the id variable
#'
#' As with the \code{t.test} function, the default test is two sided,
#' corresponding to a default value of \code{one.sided = FALSE}. To specify
#' a one sided test, the \code{one.sided} argument must specify the name of
#' the factor level (long form data) or variable (wide form data) that is
#' hypothesised (under the alternative) to have the larger mean. For instance,
#' if the outcome at "time2" is expected to be higher than at "time1", then
#' the corresponding one sided test is specified by \code{one.sided = "time2"}.
#'
#' @return An object of class 'TTest'. When printed, the output is organised
#' into five short sections. The first section lists the name of the test
#' and the variables included. The second provides means and standard
#' deviations. The third states explicitly what the null and alternative
#' hypotheses were. The fourth contains the test results: t-statistic,
#' degrees of freedom and p-value. The final section includes the relevant
#' confidence interval and an estimate of the effect size (i.e., Cohen's d)
#'
#' @seealso
#' \code{\link{t.test}},
#' \code{\link{oneSampleTTest}},
#' \code{\link{independentSamplesTTest}},
#' \code{\link{cohensD}}
#'
#' @export
#'
#' @examples
#' # long form data frame
#' df <- data.frame(
#'   id = factor( x=c(1, 1, 2, 2, 3, 3, 4, 4),
#'                labels=c("alice","bob","chris","diana") ),
#'   time = factor( x=c(1,2,1,2,1,2,1,2),
#'                  labels=c("time1","time2")),
#'   wm = c(3, 4, 6, 6, 9, 12,7,9)
#' )
#'
#' # wide form
#' df2 <- longToWide( df, wm ~ time )
#'
#' # basic test, run from long form or wide form data
#' pairedSamplesTTest( formula= wm ~ time, data=df, id="id" )
#' pairedSamplesTTest( formula= wm ~ time + (id), data=df )
#' pairedSamplesTTest( formula= ~wm_time1 + wm_time2, data=df2 )
#'
#' # one sided test
#' pairedSamplesTTest( formula= wm~time, data=df, id="id", one.sided="time2" )
#'
#' # missing data because of NA values
#' df$wm[1] <- NA
#' pairedSamplesTTest( formula= wm~time, data=df, id="id" )
#'
#' # missing data because of missing cases from the long form data frame
#' df <- df[-1,]
#' pairedSamplesTTest( formula= wm~time, data=df, id="id" )
#'
pairedSamplesTTest <- function(
  formula,
  data=NULL,
  id=NULL,
  one.sided = FALSE,
  conf.level=.95
) {



  # check that the user has input a formula
  if( missing(formula) ) { stop( '"formula" argument is missing, with no default')}
  if( !methods::is( formula, "formula")) { stop( '"formula" argument must be a formula')}


  if( length(formula)==2) { ############ ONE-SIDED FORMULA ############

    # read off the formula
    vars <- all.vars( formula )
    if( length(vars) != 2 ) stop( "one-sided 'formula' must contain exactly two variables" )
    outcome <- vars
    gp.names <- vars
    group <- NA
    id <- NA

    # check the data
    if( !missing(data) ) { # is there a data frame?

      # it needs to be data frame, because a matrix can't
      # contain both factors and numeric variables
      if( !methods::is(data,"data.frame") ) stop ( "'data' is not a data frame")

      # check that all three variables are in the data frame
      if( !( vars[1] %in% names(data)) ) {
        stop( paste0( "'", vars[1], "' is not the name of a variable in '", deparse(substitute(data)), "'" ))
      }
      if( !( vars[2] %in% names(data)) ) {
        stop( paste0( "'",vars[2],"' is not the name of a variable in '", deparse(substitute(data)), "'" ))
      }

      # truncate the data frame
      data <- data[,vars]

    } else {

      # check that all variables exist in the workspace
      workspace <- objects( parent.frame())

      # check that all three variables are in the data frame
      if( !( vars[1] %in% workspace) ) {
        stop( paste0( "'", vars[1], "' is not the name of a variable in the workspace" ))
      }
      if( !( vars[2] %in% workspace) ) {
        stop( paste0( "'", vars[2],"' is not the name of a variable in the workspace" ))
      }

      # copy variables into a data frame if none is specified, and
      # check that the variables are appropriate for a data frame
      ff <- stats::as.formula( paste( "~", vars[1], "+", vars[2]))
      data <- try( eval( stats::model.frame( formula = ff, na.action = stats::na.pass ),
                         envir=parent.frame() ), silent=TRUE)
      if( methods::is(data,"try-error") ) {
        stop( "specified variables cannot be coerced to data frame")
      }

    }

    # check classes of the variables
    if( !methods::is( data[,vars[1]], "numeric" ) ) stop( paste0( "'", vars[1], "' is not numeric" ) )
    if( !methods::is( data[,vars[2]], "numeric" ) ) stop( paste0( "'", vars[2], "' is not numeric" ) )

    # remove missing data
    exclude.id <- is.na( data[,vars[1]]) | is.na( data[,vars[2]])
    if( sum(exclude.id) > 0){
      warning( paste( sum(exclude.id), "case(s) removed due to missingness" ) )
    }
    data <- data[!exclude.id,,drop=FALSE]

    # create data matrix for later with dummy column
    WF <- cbind(rep.int(NA,nrow(data)),data)

    ############ check the one-sided option ############
    if( length(one.sided) !=1 ) stop( "invalid value for 'one.sided'" )
    if( one.sided == FALSE ) { # two sided
      alternative <- "two.sided"
    } else {
      if( one.sided == vars[1] ) { # first variable is the bigger one
        alternative <- "greater"
      } else {
        if( one.sided == vars[2] ) { # second variable is the bigger one
          alternative <- "less"
        } else {
          stop( "invalid value for 'one.sided'" )
        }
      }
    }



  } else {  ############ TWO-SIDED FORMULA ############

    ############ check formula / id combination ############

    # check that the user has specified an id that might map onto a variable name
    if( !missing(id) ) { # yes, there's an id...

      # is it a character of length one?
      if( !methods::is(id,"character") | length(id) !=1 ) {
        stop( '"id" argument does not specify the name of a valid id variable')
      }

      # if there's an id, then the formula must be of the form DV ~ IV
      if( length( formula ) !=3 ) stop( 'invalid value for "formula" argument' )
      vars <- all.vars( formula )
      if( length( vars) !=2 ) stop( 'invalid value for "formula" argument' )
      outcome <- vars[1]
      group <- vars[2]


    } else { # no, there isn't...

      # if there's no id, then the formula must specify the id variable in a
      # lme4-like fashion... either this:  DV ~ IV + (id) or DV ~ IV + (1|id).
      # this functionality is not properly, but my own sense of elegance
      # makes me want to be able to specify the full model via the formula

      meets.sneaky.case <- FALSE

      if( length( formula)==3 ) { # must be a two sided formula...
        outcome <- all.vars(formula[[2]]) # pull the outcome variable [to be checked later]
        if( length( outcome)==1 ) { # must contain only one outcome variable...

          rhs <- formula[[3]] # grab the right hand side of the formula
          if( methods::is( rhs, "call" ) &&  # RHS must be a call
                length( rhs)==3 &&  # must be a binary operation
                deparse( rhs[[1]]) == "+"  # that operation must be +
          ) {
            terms <- strsplit( deparse(rhs), split="+", fixed=TRUE)[[1]] # split by +
            if( length(terms) == 2 ) { # must have only two terms...
              terms <- gsub(" ","",terms) # deblank

              id.candidate <- grep("\\(.*\\)",terms) # id variable must have (.*) in it
              if( length( id.candidate) == 1) { # there can be only 1
                id <- terms[id.candidate] # grab that term
                group <- terms[3-id.candidate] # assume the other one is the group [it is checked later]

                # does it match lme4-like (1|id) ?
                if( length(grep( "^\\(1\\|.*\\)$", id ))==1 ) {
                  id <- gsub( "^\\(1\\|", "", id ) # delete the front bit
                  id <- gsub( "\\)$", "", id ) # delete the back bit
                  formula <- stats::as.formula( paste(outcome, "~", group) ) # truncated formula
                  meets.sneaky.case <- TRUE

                } else {

                  # alternatively, does it match (id) ?
                  if( length(grep( "^\\(.*\\)$", id ))==1  ) {
                    id <- gsub( "^\\(", "", id ) # delete the front bit
                    id <- gsub( "\\)$", "", id ) # delete the back bit
                    formula <- stats::as.formula( paste(outcome, "~", group) ) # truncated formula
                    meets.sneaky.case <- TRUE

                  }
                }
              }
            }
          }
        }
      }

      if( !meets.sneaky.case ) stop( "no 'id' variable specified")

    }


    ############ check data frame ############

    # at this point we know that outcome, vars and id are all character
    # vectors that are supposed to map onto variables either in the workspace
    # or the data frame

    # if the user has specified 'data', check that it is a data frame that
    # contains the outcome, group and id variables.
    if( !missing(data) ) {

      # it needs to be data frame, because a matrix can't
      # contain both factors and numeric variables
      if( !methods::is(data,"data.frame") ) stop ( "'data' is not a data frame")

      # check that all three variables are in the data frame
      if( !( outcome %in% names(data)) ) {
        stop( paste0( "'", outcome, "' is not the name of a variable in '", deparse(substitute(data)), "'" ))
      }
      if( !( group %in% names(data)) ) {
        stop( paste0( "'",group,"' is not the name of a variable in '", deparse(substitute(data)), "'" ))
      }
      if( !( id %in% names(data)) ) {
        stop( paste0( "'",id,"' is not the name of a variable in '", deparse(substitute(data)), "'" ))
      }


    } else {

      # check that all variables exist in the workspace
      workspace <- objects( parent.frame())

      # check that all three variables are in the data frame
      if( !( outcome %in% workspace) ) {
        stop( paste0( "'", outcome, "' is not the name of a variable in the workspace" ))
      }
      if( !( group %in% workspace) ) {
        stop( paste0( "'",group,"' is not the name of a variable in the workspace" ))
      }
      if( !( id %in% workspace) ) {
        stop( paste0( "'",id,"' is not the name of a variable in the workspace" ))
      }

      # copy variables into a data frame if none is specified, and
      # check that the variables are appropriate for a data frame
      ff <- stats::as.formula( paste( outcome, "~", group, "+", id))
      data <- try( eval( stats::model.frame( formula = ff, na.action = stats::na.pass ),
                         envir=parent.frame() ), silent=TRUE)
      if( methods::is(data,"try-error") ) {
        stop( "specified variables cannot be coerced to data frame")
      }

    }

    # subset the data frame
    data <- data[, c(outcome,group,id) ]


    ############ check classes for outcome, group and id ############

    # at this point we have a data frame that is known to contain
    # outcome, group and id. Now check that they are of the appropriate
    # type to run a t-test

    # outcome must be numeric
    if( !methods::is(data[,outcome],"numeric") ) stop( "outcome variable must be numeric")

    # group should be a factor with two-levels. issue warnings if it only
    # has two unique values but isn't a factor, or is a factor with more
    # than two levels but only uses two of them.

    if( methods::is(data[,group], "factor") ) { # it's a factor

      if( nlevels( data[,group]) <2 ) { # fewer than two levels
        stop( "grouping variable does not have two distinct levels")
      }

      if( nlevels( data[,group]) >2 ) { # more than two levels
        if( length( unique( data[,group] ))==2 ) { # but only two of them are used...
          warning( "grouping variable has unused factor levels")
          data[,group] <- droplevels( data[,group])

        } else { # too many levels in use
          stop( "grouping variable has more than two distinct values")
        }
      }

    } else { # it's not a factor

      if( length( unique( data[,group] ))==2 ) { # if it happens to have 2 unique values...
        warning( "group variable is not a factor" ) # warn the user
        data[,group] <- as.factor( data[,group]) # coerce and continue...

      } else {
        stop( "grouping variable must contain only two unique values (and should be a factor)")
      }

    }


    # id should be a factor. issue a warning if it isn't
    if( !methods::is( data[,id], "factor" )) warning( "id variable is not a factor")



    ############ check the one-sided option ############

    # group names
    gp.names <- levels(data[,group])

    # check alternative
    if( length(one.sided) !=1 ) stop( "invalid value for 'one.sided'" )
    if( one.sided == FALSE ) { # two sided
      alternative <- "two.sided"
    } else {
      if( one.sided == gp.names[1] ) { # first factor level
        alternative <- "greater"
      } else {
        if( one.sided == gp.names[2] ) { # second factor level
          alternative <- "less"
        } else {
          stop( "invalid value for 'one.sided'" )
        }
      }
    }


    ############ check cases and restructure ############

    # check that we have the right number of cases?
    tt <- table( data[,id], data[,group] )
    if( any(tt > 1) ) stop( "there too many observations for some cases" )

    # find cases to remove
    exclude.id <- tt[,1] !=1 | tt[,2] != 1   # exclude if the relevant row is missing...
    more.bad <- as.character(unique(data[apply( is.na(data[,c(outcome,group)]), 1, any ),id])) # or if it has NA
    exclude.id[ more.bad ] <- TRUE
    if( sum(exclude.id) > 0){
      warning( paste( sum(exclude.id), "case(s) removed due to missingness" ) )
    }
    exclude.id <- rownames(tt)[exclude.id]

    # remove bad cases if they exist
    bad.cases <- data[,id] %in% exclude.id
    data <- data[ !bad.cases,, drop=FALSE ]

    # Convert to wide form for later
    WF <- longToWide( data, formula )

  }


  ############ check the confidence level ############

  if( !methods::is(conf.level,"numeric") |
        length( conf.level) != 1 |
        conf.level < 0 |
        conf.level > 1
  ) {
    stop( '"conf.level" must be a number between 0 and 1')
  }

  ############ do the statistical calculations ############

  # pass to t.test
  htest <- stats::t.test( x = WF[,2], y=WF[,3], paired=TRUE,
                   alternative=alternative, conf.level=conf.level )

  # cohens D
  d <- cohensD( x= WF[,2], y=WF[,3], method="paired" )

  # descriptives
  gp.means <- sapply( WF[,-1], mean )
  gp.sd <- sapply( WF[,-1], stats::sd )

  # add the difference scores to the descriptives
  gp.means <- c( gp.means, mean(WF[,2]-WF[,3]))
  gp.sd <- c( gp.sd, stats::sd(WF[,3]-WF[,2]))

  ############ output ############

  # create output structure
  TT <- list(
    t.statistic = htest$statistic,
    df = htest$parameter,
    p.value = htest$p.value,
    conf.int = htest$conf.int,
    conf = conf.level,
    mean = gp.means,
    sd = gp.sd,
    outcome = outcome,
    group = group,
    group.names = gp.names,
    id = id,
    mu = NULL,
    alternative = alternative,
    method = "Paired samples t-test",
    effect.size = d
  )

  # specify the class and return
  class(TT) <- "TTest"
  return(TT)


}


