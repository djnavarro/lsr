
# Removed from earlier version...
#
# \section{Note}{\code{pairedSamplesTTest} also supports an even more
# "lme4"-like method for specifying the model in the \code{formula} argument.
# That is, \code{outcome ~ group + (1|id)} is deemed to be equivalent to
# \code{outcome ~ group + (id)}. This may be removed in future versions.}


#' Paired samples t-test
#'
#' @description Runs a paired-samples t-test and prints the results in a
#' readable format.
#'
#' @param formula A formula describing the data. For wide-format data use a
#'   one-sided formula such as \code{~ time1 + time2}. For long-format data
#'   use \code{outcome ~ group + (id)}, or \code{outcome ~ group} together
#'   with the \code{id} argument.
#' @param data An optional data frame containing the variables named in
#'   \code{formula}. Tibbles are accepted and converted automatically. If
#'   \code{data} is omitted the variables are looked up in the workspace.
#' @param id The name of the participant ID variable as a character string
#'   (e.g., \code{id = "subject"}). Required when using long-format data
#'   with a plain \code{outcome ~ group} formula instead of
#'   \code{outcome ~ group + (id)}.
#' @param one.sided Set to \code{FALSE} (default) for a two-sided test. Set
#'   to the name of the group or variable expected to have the larger mean
#'   for a one-sided test (e.g., \code{one.sided = "time2"}).
#' @param conf.level The confidence level for the confidence interval.
#'   The default is \code{0.95} for a 95\% interval.
#'
#' @details Runs a paired-samples t-test and prints the results in a
#' beginner-friendly format. The calculations are done by \code{\link{t.test}}
#' and \code{\link{cohensD}}.
#'
#' There are two ways to supply data. If the data are in \strong{wide format}
#' (one row per participant, with the two measurements in separate columns),
#' use a one-sided formula such as \code{~ time1 + time2}. The first row of
#' \code{time1} is paired with the first row of \code{time2}, and so on.
#'
#' If the data are in \strong{long format} (two rows per participant), use a
#' two-sided formula. The recommended style is \code{outcome ~ group + (id)},
#' where the participant ID variable is enclosed in parentheses. Alternatively,
#' use the plain formula \code{outcome ~ group} and supply the ID variable name
#' via the \code{id} argument. The lme4-style notation
#' \code{outcome ~ group + (1|id)} is also accepted as equivalent to
#' \code{outcome ~ group + (id)}.
#'
#' Participants with missing measurements are removed with a warning.
#'
#' @return Prints a summary showing the variable names, descriptive statistics
#' (including the mean and standard deviation of the differences), null and
#' alternative hypotheses, test results (t-statistic, degrees of freedom,
#' p-value), a confidence interval, and Cohen's d as a measure of effect size.
#' The underlying results are also returned as a list, so the output can be
#' assigned to a variable and inspected if needed.
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
#' # long-format data: one row per participant per time point
#' df <- data.frame(
#'   id = factor(
#'     x = c(1, 1, 2, 2, 3, 3, 4, 4),
#'     labels = c("alice", "bob", "chris", "diana")
#'   ),
#'   time = factor(
#'     x = c(1, 2, 1, 2, 1, 2, 1, 2),
#'     labels = c("time1", "time2")
#'   ),
#'   wm = c(3, 4, 6, 6, 9, 12, 7, 9)
#' )
#'
#' # wide-format data: one row per participant
#' df2 <- longToWide(df, wm ~ time)
#'
#' # three equivalent ways to run the same test
#' pairedSamplesTTest(formula = wm ~ time, data = df, id = "id")
#' pairedSamplesTTest(formula = wm ~ time + (id), data = df)
#' pairedSamplesTTest(formula = ~wm_time1 + wm_time2, data = df2)
#'
#' # one-sided test: is time2 larger than time1?
#' pairedSamplesTTest(formula = wm ~ time, data = df, id = "id", one.sided = "time2")
#'
#' # missing value: that participant is removed with a warning
#' df$wm[1] <- NA
#' pairedSamplesTTest(formula = wm ~ time, data = df, id = "id")
#'
#' # missing row: that participant is also removed with a warning
#' df <- df[-1, ]
#' pairedSamplesTTest(formula = wm ~ time, data = df, id = "id")
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
      data <- as.data.frame(data)

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
      data <- as.data.frame(data)

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

  if( !methods::is(conf.level,"numeric") ||
        length( conf.level) != 1 ||
        is.na(conf.level) ||
        conf.level < 0 ||
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


