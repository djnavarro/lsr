

# longToWide() takes a long-form data frame and converts it to a wide-form data frame.
# Like it's companion function wideToLong() it's not as flexible as cast() and melt(),
# but it is easier to use.


#' Reshape from long to wide
#'
#' @description Reshapes a data frame from long form (one row per observation)
#' to wide form (one row per subject), using a formula to specify the structure.
#'
#' @param data A long-form data frame with one row per observation.
#' @param formula A two-sided formula of the form \code{measure ~ within},
#'   listing the measured variable(s) on the left and the within-subject
#'   variable(s) on the right. All other variables in \code{data} are treated
#'   as between-subject variables. Multiple variables are supported on each
#'   side, e.g. \code{rt + accuracy ~ day + session}.
#' @param sep The separator string used to construct wide-form variable names.
#'   Defaults to \code{"_"}. For example, with \code{sep = "_"} and a measure
#'   called \code{accuracy} at levels \code{t1} and \code{t2}, the output
#'   columns are named \code{accuracy_t1} and \code{accuracy_t2}.
#'
#' @details This function is the companion to \code{\link{wideToLong}}. It
#' reshapes a long-form data frame into wide form by spreading the within-subject
#' observations across columns, with column names constructed from the measure
#' name and factor level(s) joined by \code{sep}.
#'
#' @return A wide-form data frame with one row per subject (or experimental
#' unit). Column names for the repeated measures follow the naming convention
#' used by \code{\link{wideToLong}}: the measure name followed by the
#' within-subject factor level(s), separated by \code{sep}.
#'
#' @seealso \code{\link{wideToLong}}, \code{\link{reshape}}
#'
#' @export
#'
#' @examples
#' long <- data.frame(
#'   id       = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
#'   time     = c("t1", "t1", "t1", "t2", "t2", "t2", "t3", "t3", "t3"),
#'   accuracy = c(.50, .03, .72, .94, .63, .49, .78, .71, .16)
#' )
#'
#' longToWide(long, accuracy ~ time)
#'
longToWide <- function( data, formula, sep="_") {

  if( !methods::is(data, "data.frame") ) stop( '"data" must be a data frame')
  if( missing(formula) ) stop( '"formula" argument is missing, with no default')
  if( !methods::is(formula, "formula") ) stop( '"formula" must be a formula')
  if( length(formula) != 3 ) stop( '"formula" must be a two-sided formula')
  if( !methods::is(sep, "character") || length(sep) != 1 ) {
    stop( '"sep" must be a single character string')
  }

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
