

# longToWide() takes a long-form data frame and converts it to a wide-form data frame.
# Like it's companion function wideToLong() it's not as flexible as cast() and melt(),
# but it is easier to use.


#' Reshape from long to wide
#'
#' @description Reshape a data frame from long form to wide form
#'
#' @param data The data frame.
#' @param formula A two-sided formula specifying measure variables and within-subject variables
#' @param sep Separator string used in wide-form variable names
#'
#' @details The \code{longToWide} function is the companion function to
#' \code{wideToLong}. The \code{data} argument is a "long form" data frame,
#' in which each row corresponds to a single observation. The output is a
#' "wide form" data frame, in which each row corresponds to a single
#' experimental unit (e.g., a single subject).
#'
#' The reshaping formula should list all of the measure variables on the
#' left hand side, and all of the within-subject variables on the right
#' hand side. All other variables are assumed to be between-subject variables.
#' For example, if the \code{accuracy} of a participant's performance is
#' measured at multiple \code{time} points, then the formula would be
#' \code{accuracy ~ time}.
#'
#' Multiple variables are supported on both sides of the formula. For example,
#' suppose we measured the response time \code{rt} and \code{accuracy} of
#' participants, across three separate \code{days}, and across three separate
#' \code{sessions} within each day. In this case the formula would be
#' \code{rt + accuracy ~ days + sessions}.
#'
#' @return The output is a "wide form" data frame in containing one row per
#' subject (or experimental unit, more generally), with each observation of
#' that subject corresponding to a separate variable. The naming scheme for
#' these variables places the name of the measured variable first, followed
#' by the levels of within-subjects variable(s), separated by the separator
#' string \code{sep}. In the example above where the reshaping formula was
#' \code{accuracy ~ time}, if the default separator of \code{sep="_"} was
#' used, and the levels of the \code{time} variable are \code{t1}, \code{t2}
#' and \code{t3}, then the output would include the variables
#' \code{accuracy_t1}, \code{accuracy_t2} and \code{accuracy_t3}.
#'
#' In the second example listed above, where the reshaping formula was
#' \code{rt + accuracy ~ days + sessions}, the output variables would refer
#' to levels of both within-subjects variables. For instance,
#' \code{rt_day1_session1}, and \code{accuracy_day2_session1} might be the
#' names of two of the variables in the wide form data frame.
#'
#' @export
#'
#' @seealso \code{\link{wideToLong}}
#'
#' @examples
#' long <- data.frame(
#'   id = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
#'   time = c("t1", "t1", "t1", "t2", "t2", "t2", "t3", "t3", "t3"),
#'   accuracy = c(.50, .03, .72, .94, .63, .49, .78, .71, .16)
#' )
#'
#' longToWide(long, accuracy ~ time)
#'
longToWide <- function( data, formula, sep="_") {

  within <- all.vars(formula[-2])
  v.names <- all.vars(formula[-3])
  idvar <- setdiff(names(data),c(within,v.names))

  if( length(within)>1 ) {
    collapsed.treatments <- apply(as.matrix(data[,within]),1,paste,collapse=sep)
    data <- data[,setdiff(names(data),within)] # delete split treatments
    data$within <- collapsed.treatments # append collapsed treatment
    within <- "within"
  }
  times <- unique( data[,within]) # measure 'time' names
  varying <- list()
  for( i in seq_along(v.names) ) varying[[i]] <- paste(v.names[i],times, sep=sep)

  x<-stats::reshape( data, idvar=idvar, varying=varying, direction="wide", times=times, v.names=v.names, timevar=within)
  rownames(x) <- NULL
  return(x)

}
