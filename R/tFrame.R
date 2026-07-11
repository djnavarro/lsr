# tFrame() transposes a data frame. This isn't usually a very sensible thing to do,
# except in those cases where the data frame would actually make sense as a matrix,
# in which case you could coerce to a matrx and then transpose using t(). However,
# there are some cases where novices (who haven't yet grasped coercion) might want
# to transpose a data frame, and it can be handy to use the tFrame() function for
# teaching purposes rather than stop to teach coercion & data types on the spot.

#' Transpose a data frame
#'
#' @description Transposes a data frame, swapping rows and columns, and returns
#' the result as a data frame.
#'
#' @param x A data frame to be transposed.
#'
#' @details Equivalent to \code{as.data.frame(t(x))}. Unlike applying
#' \code{\link{t}} directly, \code{tFrame} ensures the result is a data frame
#' rather than a matrix. This makes sense when the rows of \code{x} can be
#' meaningfully treated as variables — for example, when each row represents a
#' measurement type and each column represents a participant.
#'
#' @return The transposed data frame.
#'
#' @seealso \code{\link{t}}
#'
#' @export
#'
#' @examples
#' dataset <- data.frame(
#'   Gf = c(105, 119, 121, 98), # fluid intelligence
#'   Gc = c(110, 115, 119, 103), # crystallised intelligence
#'   Gs = c(112, 102, 108, 99) # processing speed
#' )
#' rownames(dataset) <- paste0("person", 1:4)
#' dataset
#'
#' tFrame(dataset)
#'
tFrame <- function(x) {
  if (!methods::is(x, "data.frame")) {
    stop("'tFrame' is intended to apply to data frames only")
  }
  x <- t(x) # coerce to matrix and transpose
  x <- as.data.frame(x)
  return(x)
}
