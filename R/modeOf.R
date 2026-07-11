# modeOf() returns the sample mode: the value that has highest observed frequency

#' Sample mode
#'
#' @description Calculate the most frequently occurring value(s) in a sample
#' (\code{modeOf}) or the frequency of the most common value (\code{maxFreq}).
#'
#' @param x A vector or factor containing the observations.
#' @param na.rm Set to \code{TRUE} (the default) to remove missing values
#'   before computing the mode. Set to \code{FALSE} to treat \code{NA} as a
#'   possible modal value (see Details).
#'
#' @details When \code{na.rm = FALSE}, missing values are treated as a
#' distinct value that can itself be the mode. If the number of \code{NA}s
#' exceeds the frequency of every other value, \code{modeOf} returns
#' \code{NA} and \code{maxFreq} returns the count of missing values.
#'
#' Because of this ambiguity, the default is \code{na.rm = TRUE}, unlike
#' most other functions in this package.
#'
#' @return \code{modeOf} returns the most frequently observed value. If
#' multiple values are tied for the highest frequency, all of them are
#' returned as a vector. \code{maxFreq} returns the modal frequency as a
#' single number.
#'
#' @name mode
#'
#' @seealso
#' \code{\link{mean}},
#' \code{\link{median}},
#' \code{\link{table}}
#'
#' @examples
#' eyes <- c("green", "green", "brown", "brown", "blue")
#' modeOf(eyes) # returns c("green", "brown") -- a tie
#' maxFreq(eyes) # returns 2
#'
#' # with missing data
#' eyes <- c("green", "green", "brown", "brown", "blue", NA, NA, NA)
#'
#' # na.rm = FALSE: NA is the most frequent "value"
#' modeOf(eyes, na.rm = FALSE)
#' maxFreq(eyes, na.rm = FALSE)
#'
#' # na.rm = TRUE: missing values ignored
#' modeOf(eyes, na.rm = TRUE)
#' maxFreq(eyes, na.rm = TRUE)
#' NULL
#' @rdname mode
#' @export
modeOf <- function(x, na.rm = TRUE) {
  if ((!is.vector(x) | is.list(x)) & !is.factor(x)) {
    stop('"x" must be a vector or a factor')
  }
  if (!methods::is(na.rm, "logical") || length(na.rm) != 1 || is.na(na.rm)) {
    stop('"na.rm" must be a single logical value')
  }

  na.freq <- 0
  if (na.rm == FALSE) {
    na.freq <- sum(is.na(x))
  } # count the NAs if needed
  x <- x[!is.na(x)] # delete NAs
  obs.val <- unique(x) # find unique values
  valFreq <- function(x, y) {
    sum(y == x)
  }
  freq <- unlist((lapply(obs.val, valFreq, x))) # apply for all unique values
  max.freq <- max(freq) # modal frequency
  if (na.rm == FALSE & na.freq > max.freq) {
    modal.values <- NA # mode is NA if appropriate...
  } else {
    modal.cases <- freq == max.freq # otherwise find modal cases
    modal.values <- obs.val[modal.cases] # and corresponding values
  }
  if (is.factor(x)) modal.values <- as.character(modal.values)
  return(modal.values)
}


# maxFreq() returns the frequency of the sample mode.

#' @rdname mode
#' @export
maxFreq <- function(x, na.rm = TRUE) {
  if ((!is.vector(x) | is.list(x)) & !is.factor(x)) {
    stop('"x" must be a vector or a factor')
  }
  if (!methods::is(na.rm, "logical") || length(na.rm) != 1 || is.na(na.rm)) {
    stop('"na.rm" must be a single logical value')
  }

  na.freq <- 0
  if (na.rm == FALSE) {
    na.freq <- sum(is.na(x))
  } # count the NAs if needed
  x <- x[!is.na(x)] # delete NAs
  obs.val <- unique(x) # find unique values
  valFreq <- function(x, y) {
    sum(y == x)
  }
  freq <- unlist((lapply(obs.val, valFreq, x))) # apply for all unique values
  max.freq <- max(freq, na.freq) # modal frequency
  return(max.freq)
}
