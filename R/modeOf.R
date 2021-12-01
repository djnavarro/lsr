
# modeOf() returns the sample mode: the value that has highest observed frequency

#' Sample mode
#'
#' @description Calculate the mode of a sample: both modal value(s) and
#' the corresponding frequency
#'
#' @param x A vector containing the observations.
#' @param na.rm Logical value indicating whether NA values should be removed.
#'
#' @details These two functions can be used to calculate the mode (most
#' frequently observed value) of a sample, and the actual frequency of the
#' modal value. The only complication is in respect to missing data. If
#' \code{na.rm = FALSE}, then there are multiple possibilities for how to
#' calculate the mode. One possibility is to treat \code{NA} as another
#' possible value for the elements of \code{x}, and therefore if \code{NA}
#' is more frequent than any other value, then \code{NA} is the mode; and
#' the modal frequency is equal to the number of missing values. This is
#' the version that is currently implemented.
#'
#' Another possibility is to treat \code{NA} as meaning "true value unknown",
#' and to the mode of \code{x} is itself known only if the number of missing
#' values is small enough that -- regardless of what value they have -- they
#' cannot alter the sample mode. For instance, if \code{x} were
#' \code{c(1,1,1,1,2,2,NA)}, we know that the mode of \code{x} is \code{1}
#' regardless of what the true value is for the one missing datum; and we
#' know that the modal frequency is between 4 and 5. This is also a valid
#' interpretation, depending on what precisely it is the user wants, but
#' is not currently implemented.
#'
#' Because of the ambiguity of how \code{na.rm = FALSE} should be interpreted,
#' the default value has been set to \code{na.rm = TRUE}, which differs from
#' the default value used elsewhere in the package.
#'
#' @return The \code{modeOf} function returns the mode of \code{x}. If there
#' are ties, it returns a vector containing all values of \code{x} that have
#' the modal frequency. The \code{maxFreq} function returns the modal
#' frequency as a numeric value.
#'
#' @name mode
#'
#' @seealso
#' \code{\link{mean}},
#' \code{\link{median}},
#' \code{\link{table}}
#'
#' @examples
#' # simple example
#' eyes <- c("green","green","brown","brown","blue")
#' modeOf(eyes)
#' maxFreq(eyes)
#'
#' # vector with missing data
#' eyes <- c("green","green","brown","brown","blue",NA,NA,NA)
#'
#' # returns NA as the modal value.
#' modeOf(eyes, na.rm = FALSE)
#' maxFreq(eyes, na.rm = FALSE)
#'
#' # returns c("green", "brown") as the modes, as before
#' modeOf(eyes, na.rm = TRUE)
#' maxFreq(eyes, na.rm = TRUE)
NULL

#' @rdname mode
#' @export
modeOf <- function(x, na.rm = TRUE) {

  if( !is.vector(x) & !is.factor(x) ) {
    stop( '"x" must be a vector or a factor')
  }
  if( !methods::is(na.rm,"logical") | length(na.rm) !=1 ) {
    stop( '"na.rm" must be a single logical value')
  }

  na.freq <- 0
  if (na.rm == FALSE) { na.freq <- sum( is.na(x) ) }  # count the NAs if needed
  x <- x[!is.na(x)]                                   # delete NAs
  obs.val <- unique(x)                                # find unique values
  valFreq <- function(x, y){ sum(y == x) }
  freq <- unlist((lapply( obs.val, valFreq, x )))     # apply for all unique values
  max.freq <- max(freq)                               # modal frequency
  if (na.rm == FALSE & na.freq > max.freq) {
    modal.values <- NA                                # mode is NA if appropriate...
  } else {
    modal.cases <- freq == max.freq                   # otherwise find modal cases
    modal.values <- obs.val[modal.cases]              # and corresponding values
  }
  if(class(x)=="factor") modal.values <- as.character(modal.values)
  return( modal.values )

}


# maxFreq() returns the frequency of the sample mode.

#' @rdname mode
#' @export
maxFreq <- function(x, na.rm = TRUE) {

  if( !is.vector(x) & !is.factor(x) ) {
    stop( '"x" must be a vector or a factor')
  }
  if( !methods::is(na.rm,"logical") | length(na.rm) !=1 ) {
    stop( '"na.rm" must be a single logical value')
  }

  na.freq <- 0
  if (na.rm == FALSE) { na.freq <- sum( is.na(x) ) }  # count the NAs if needed
  x <- x[!is.na(x)]                                   # delete NAs
  obs.val <- unique(x)                                # find unique values
  valFreq <- function(x, y){ sum(y == x) }
  freq <- unlist((lapply( obs.val, valFreq, x )))     # apply for all unique values
  max.freq <- max(freq, na.freq)                      # modal frequency
  return( max.freq )

}


