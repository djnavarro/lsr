
# wideToLong() takes a wide-form data frame and converts it to a long-form data frame.
# Like it's companion function longToWide() it's not as flexible as cast() and melt(),
# because it relies on variable names following a very specific naming scheme, but as
# long as the user does adhere to this scheme it's a much easier function to use.


#' Reshape from wide to long
#'
#' @description Reshape a data frame from wide form to long form using the variable names
#'
#' @param data The data frame.
#' @param within Name to give to the long-form within-subject factor(s)
#' @param sep Separator string used in wide-form variable names
#' @param split Should multiple within-subject factors be split into multiple variables?
#'
#' @details The \code{wideToLong} function is the companion function to \code{longToWide}.
#' The \code{data} argument is a "wide form" data frame, in which each row corresponds to
#' a single experimental unit (e.g., a single subject).  The output is a "long form" data
#' frame, in which each row corresponds to a single observation.
#'
#' The \code{wideToLong} function relies on the variable names to determine how the data
#' should be reshaped. The naming scheme for these variables places the name of the
#' measured variable first, followed by the levels of the within-subjects variable(s),
#' separated by the separator string \code{sep} (default is \code{_}) The separator
#' string cannot appear anywhere else in the variable names: variables without
#' the separator string are assumed to be between-subject variables.
#'
#' If the experiment measured the \code{accuracy} of participants at some task
#' at two different points in time, then the wide form data frame would contain
#' variables of the form \code{accuracy_t1} and \code{accuracy_t2}. After
#' reshaping, the long form data frame would contain one measured variable
#' called \code{accuracy}, and a within-subjects factor with levels \code{t1}
#' and \code{t2}. The name of the within-subjects factor is the \code{within}
#' argument.
#'
#' The function supports experimental designs with multiple within-subjects
#' factors and multi-variable observations. For example, suppose each
#' experimental subject is tested in two \code{conditions} (\code{cond1} and
#' \code{cond2}), on each of two \code{days} (\code{day1} and \code{day2}),
#' yielding an experimental design in which four observations are made for
#' each subject. For each such observation, we record the mean response time
#' \code{MRT} for and proportion of correct responses \code{PC} for the
#' participant.  The variable names needed for a design such as this one would
#' be \code{MRT_cond1_day1}, \code{MRT_cond1_day2}, \code{PC_cond1_day1}, etc.
#' The \code{within} argument should be a vector of names for the
#' within-subject factors: in this case, \code{within = c("condition","day")}.
#'
#' By default, if there are multiple within-subject factors implied by the
#' existence of multiple separators, the output will keep these as distinct
#' variables in the long form data frame (\code{split=FALSE}). If
#' \code{split=TRUE}, the within-subject factors will be collapsed into a
#' single variable.
#'
#' @return A data frame containing the reshaped data
#'
#' @seealso \code{\link{longToWide}}, \code{\link{reshape}}
#'
#' @export
#'
#' @examples
#' # Outcome measure is mean response time (MRT), measured in two conditions
#' # with 4 participants. All participants participate in both conditions.
#'
#' wide <- data.frame( accuracy_t1 = c( .15,.50,.78,.55 ),  # accuracy at time point 1
#'                     accuracy_t2 = c( .55,.32,.99,.60 ),  # accuracy at time point 2
#'                     id = 1:4 )                           # id variable
#'
#' # convert to long form
#' wideToLong( wide, "time" )
#'
#'
#' # A more complex design with multiple within-subject factors. Again, we have only
#' # four participants, but now we have two different outcome measures, mean response
#' # time (MRT) and the proportion of correct responses (PC). Additionally, we have two
#' # different repeated measures variables. As before, we have the experimental condition
#' # (cond1, cond2), but this time each participant does both conditions on two different
#' # days (day1, day2). Finally, we have multiple between-subject variables too, namely
#' # id and gender.
#'
#' wide2 <- data.frame( id = 1:4,
#'                      gender = factor( c("male","male","female","female") ),
#'                      MRT_cond1_day1 = c( 415,500,478,550 ),
#'                      MRT_cond2_day1 = c( 455,532,499,602 ),
#'                      MRT_cond1_day2 = c( 400,490,468,502 ),
#'                      MRT_cond2_day2 = c( 450,518,474,588 ),
#'                      PC_cond1_day1 = c( 79,83,91,75 ),
#'                      PC_cond2_day1 = c( 82,86,90,78 ),
#'                      PC_cond1_day2 = c( 88,92,98,89 ),
#'                      PC_cond2_day2 = c( 93,97,100,95 ) )
#'
#' # conversion to long form:
#' wideToLong( wide2 )
#' wideToLong( wide2, within = c("condition","day") )
#'
#' # treat "condition x day" as a single repeated measures variable:
#' wideToLong( wide2, split = FALSE)
#'
wideToLong <- function( data, within="within", sep="_", split=TRUE) {

  ind <- grep(sep,names(data),fixed=TRUE) # indices of variables that are repeated
  idvar <- names(data)[-ind] # names of id variables

  # make sure that the id variables do uniquely specify cases
  n.profiles <- dim( unique( as.matrix( data[,idvar,drop=FALSE] ), margin=1 ) )[1] # number of unique id-var profiles
  if( n.profiles < dim(data)[1] ) { # if id variables don't uniquely specify cases
    warning( "Between-subject variables must uniquely specify cases: a case 'id' variable has been added")
    id <- 1:dim(data)[1]
    data <- cbind(data,id)
    names(data) <- make.unique(names(data))
    ind <- grep(sep,names(data),fixed=TRUE) # indices of variables that are repeated
    idvar <- names(data)[-ind] # names of id variables
  }

  tmp <- t(as.data.frame(strsplit( names(data[ind]), sep, fixed=TRUE ))) # matrix with split var names
  v.names <- unique(tmp[,1]) # grab the measure var names
  times <- unique(apply( tmp[,-1,drop=FALSE],1,paste,collapse=sep)) # measure 'time' names
  varying <- list()
  for( i in seq_along(v.names) ) varying[[i]] <- names(data)[ind][tmp[,1]==v.names[i]]

  tmp <-make.unique(c(names(data),"withintmp"))
  within.tmp <- tmp[length(tmp)]

  x<-stats::reshape( data, idvar=idvar, varying=varying, direction="long",
              times=times, v.names=v.names, timevar=within.tmp,
              new.row.names = paste0("case.",1:(dim(data)[1]*length(times))) )


  if( split==TRUE & length( grep(sep,times,fixed=TRUE))>0 ) { # split multiple treatments into two factors?
    split.treatments <- t(as.data.frame(strsplit(x[,within.tmp],sep,fixed=TRUE)))
    rownames(split.treatments)<-NULL
    split.treatments <- as.data.frame(split.treatments)
    if( length(within)==1) {
      names(split.treatments) <- paste(within,1:length(split.treatments),sep="")
    } else {
      if( length(within) == length(split.treatments)) {
        names(split.treatments) <- within
      } else { stop( "length of 'within' is incorrect" )}
    }
    x <- x[,setdiff(names(x),within.tmp)] # delete collapsed treatment
    x <- cbind(x,split.treatments) # append split treatment
  } else {
    x[,within.tmp]<- factor(x[,within.tmp])
    names(x)[grep(within.tmp,names(x))] <- within
  }
  rownames(x) <- NULL
  names(x) <- make.unique(names(x))
  return(x)

}

