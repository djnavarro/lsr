

# tFrame() transposes a data frame. This isn't usually a very sensible thing to do,
# except in those cases where the data frame would actually make sense as a matrix,
# in which case you could coerce to a matrx and then transpose using t(). However,
# there are some cases where novices (who haven't yet grasped coercion) might want
# to transpose a data frame, and it can be handy to use the tFrame() function for
# teaching purposes rather than stop to teach coercion & data types on the spot.

#' Transpose a data frame
#'
#' @description Transposes a data frame, converting variables to cases and vice versa
#'
#' @param x The data frame to be transposed.
#'
#' @details The \code{tFrame} function is a convenience function that simply
#' transposes the input data frame and coerces the result back to a data frame.
#' Apart from a very small amount of exception handling, it is equivalent to
#' \code{as.data.frame(t(x))}. It exists simply because I sometimes find it
#' convenient when teaching statistics to discuss simple data handling before
#' going into details regarding coercion; similarly, since I generally have
#' students work with data frames before exposing them to matrices, it is
#' convenient to have a transpose function that returns a data frame as output.
#'
#' Naturally, the \code{tFrame} function should only be used when it is
#' actually sensible to think of the cases of \code{x} as variables in their
#' own right. In real life I expect that this maps almost perfectly onto those
#' cases where \code{x} could be a matrix just as easily as a data frame, so
#' I don't believe that \code{tFrame} is useful in real world data analysis. It
#' is intended as a teaching tool.
#'
#' @return The transposed data frame
#'
#' @seealso \code{\link{t}}
#'
#' @export
#'
#' @examples
#' # Create a data frame that could sensibly be transposed...
#' Gf <- c(105, 119, 121, 98)   # fluid intelligence for 4 people
#' Gc <- c(110, 115, 119, 103)  # crystallised intelligence
#' Gs <- c(112, 102, 108, 99)   # speed of processing
#' dataset <- data.frame( Gf, Gc, Gs )
#' rownames(dataset) <- paste( "person", 1:4, sep="" )
#' print(dataset)
#'
#' # Now transpose it...
#' tFrame( dataset )
#'
tFrame <- function(x) {
  if (!methods::is(x,"data.frame")) {
    stop("'tFrame' is intended to apply to data frames only")
  }
  x <- t(x) # coerce to matrix and transpose
  x <- as.data.frame(x)
  return( x )
}
