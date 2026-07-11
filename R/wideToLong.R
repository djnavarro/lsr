# wideToLong() takes a wide-form data frame and converts it to a long-form data frame.
# Like it's companion function longToWide() it's not as flexible as cast() and melt(),
# because it relies on variable names following a very specific naming scheme, but as
# long as the user does adhere to this scheme it's a much easier function to use.


#' Reshape from wide to long
#'
#' @description Reshapes a data frame from wide form (one row per subject) to
#' long form (one row per observation), using variable names to determine the
#' structure.
#'
#' @param data A wide-form data frame with one row per subject (or experimental
#'   unit). Variables whose names contain \code{sep} are treated as repeated
#'   measures; all others are treated as between-subject variables.
#' @param within A character string, or vector of strings, giving the name(s)
#'   to use for the within-subject factor column(s) in the output. Defaults to
#'   \code{"within"}.
#' @param sep The separator string used in the wide-form variable names to
#'   separate the measure name from the factor level(s). Defaults to \code{"_"}.
#'   The separator must not appear anywhere else in the variable names.
#' @param split Set to \code{TRUE} (the default) to split multiple within-subject
#'   factors into separate columns in the output. Set to \code{FALSE} to keep
#'   them combined into a single column.
#'
#' @details This function is the companion to \code{\link{longToWide}}. It
#' determines the reshape structure from the variable names rather than
#' requiring an explicit formula.
#'
#' The naming scheme for repeated-measures variables places the measure name
#' first, followed by the factor level(s), all joined by \code{sep}. For
#' example, variables named \code{accuracy_t1} and \code{accuracy_t2} indicate
#' a measure called \code{accuracy} recorded at two time points (\code{t1} and
#' \code{t2}). After reshaping, the long-form output contains one column called
#' \code{accuracy} and a factor column (named by the \code{within} argument)
#' with levels \code{t1} and \code{t2}.
#'
#' Designs with multiple within-subject factors are supported. For example,
#' \code{MRT_cond1_day1} encodes measure \code{MRT} at level \code{cond1} of
#' one factor and \code{day1} of another. Supply \code{within = c("condition",
#' "day")} to name both output columns. Multiple measured variables per
#' observation (e.g., both \code{MRT} and \code{PC}) are also supported.
#'
#' @return A long-form data frame with one row per observation.
#'
#' @seealso \code{\link{longToWide}}, \code{\link{reshape}}
#'
#' @export
#'
#' @examples
#' # simple design: accuracy measured at two time points for 4 participants
#' wide <- data.frame(
#'   id          = 1:4,
#'   accuracy_t1 = c(.15, .50, .78, .55),
#'   accuracy_t2 = c(.55, .32, .99, .60)
#' )
#' wideToLong(wide, "time")
#'
#' # complex design: two measures (MRT, PC), two conditions, two days
#' wide2 <- data.frame(
#'   id             = 1:4,
#'   gender         = factor(c("male", "male", "female", "female")),
#'   MRT_cond1_day1 = c(415, 500, 478, 550),
#'   MRT_cond2_day1 = c(455, 532, 499, 602),
#'   MRT_cond1_day2 = c(400, 490, 468, 502),
#'   MRT_cond2_day2 = c(450, 518, 474, 588),
#'   PC_cond1_day1  = c(79, 83, 91, 75),
#'   PC_cond2_day1  = c(82, 86, 90, 78),
#'   PC_cond1_day2  = c(88, 92, 98, 89),
#'   PC_cond2_day2  = c(93, 97, 100, 95)
#' )
#'
#' # default: condition and day become separate columns
#' wideToLong(wide2, within = c("condition", "day"))
#'
#' # alternative: keep condition and day as one combined column
#' wideToLong(wide2, split = FALSE)
#'
wideToLong <- function(data, within = "within", sep = "_", split = TRUE) {
  if (!methods::is(data, "data.frame")) stop('"data" must be a data frame')
  if (!methods::is(within, "character") || length(within) == 0) {
    stop('"within" must be a non-empty character vector')
  }
  if (!methods::is(sep, "character") || length(sep) != 1) {
    stop('"sep" must be a single character string')
  }
  if (!methods::is(split, "logical") || length(split) != 1 || is.na(split)) {
    stop('"split" must be a single logical value')
  }

  ind <- grep(sep, names(data), fixed = TRUE) # indices of variables that are repeated

  # check that the separator appears in at least some column names
  if (length(ind) == 0) {
    stop(
      'No column names contain the separator "', sep, '". ',
      'Repeated-measures column names must follow the pattern ',
      'MEASURE', sep, 'CONDITION (e.g. "score', sep, 't1", "score', sep, 't2"). ',
      'Check that your column names use "', sep, '" as the separator, ',
      'or set sep = "..." to match a different separator.'
    )
  }

  idvar <- names(data)[-ind] # names of id variables

  # make sure that the id variables do uniquely specify cases
  n.profiles <- dim(unique(as.matrix(data[, idvar, drop = FALSE]), margin = 1))[1] # number of unique id-var profiles
  if (n.profiles < dim(data)[1]) { # if id variables don't uniquely specify cases
    warning("Between-subject variables must uniquely specify cases: a case 'id' variable has been added")
    id <- 1:dim(data)[1]
    data <- cbind(data, id)
    names(data) <- make.unique(names(data))
    ind <- grep(sep, names(data), fixed = TRUE) # indices of variables that are repeated
    idvar <- names(data)[-ind] # names of id variables
  }

  tmp <- t(as.data.frame(strsplit(names(data[ind]), sep, fixed = TRUE))) # matrix with split var names
  v.names <- unique(tmp[, 1]) # grab the measure var names
  times <- unique(apply(tmp[, -1, drop = FALSE], 1, paste, collapse = sep)) # measure 'time' names
  varying <- list()
  for (i in seq_along(v.names)) varying[[i]] <- names(data)[ind][tmp[, 1] == v.names[i]]

  tmp <- make.unique(c(names(data), "withintmp"))
  within.tmp <- tmp[length(tmp)]

  x <- stats::reshape(data,
    idvar = idvar, varying = varying, direction = "long",
    times = times, v.names = v.names, timevar = within.tmp,
    new.row.names = paste0("case.", 1:(dim(data)[1] * length(times)))
  )


  if (split == TRUE & length(grep(sep, times, fixed = TRUE)) > 0) { # split multiple treatments into two factors?
    split.treatments <- t(as.data.frame(strsplit(x[, within.tmp], sep, fixed = TRUE)))
    rownames(split.treatments) <- NULL
    split.treatments <- as.data.frame(split.treatments)
    if (length(within) == 1) {
      names(split.treatments) <- paste(within, 1:length(split.treatments), sep = "")
    } else {
      if (length(within) == length(split.treatments)) {
        names(split.treatments) <- within
      } else {
        stop("length of 'within' is incorrect")
      }
    }
    x <- x[, setdiff(names(x), within.tmp)] # delete collapsed treatment
    x <- cbind(x, split.treatments) # append split treatment
  } else {
    x[, within.tmp] <- factor(x[, within.tmp])
    names(x)[grep(within.tmp, names(x))] <- within
  }
  rownames(x) <- NULL
  names(x) <- make.unique(names(x))
  return(x)
}
