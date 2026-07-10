
#' Copy a vector into a matrix
#'
#' @description Creates a matrix by stacking multiple copies of a vector as
#' rows (\code{rowCopy}) or as columns (\code{colCopy}).
#'
#' @param x A vector to be copied.
#' @param times The number of copies to stack together.
#' @param dimnames An optional list specifying row and column names for the
#'   output matrix.
#'
#' @details These functions are shortcuts for building a matrix where every
#' row (or every column) is the same vector. They are equivalent to calling
#' \code{\link{matrix}} with appropriate \code{byrow} and dimension arguments.
#'
#' @return For \code{rowCopy}, a matrix with \code{times} rows and
#' \code{length(x)} columns, where each row is \code{x}. For \code{colCopy},
#' a matrix with \code{length(x)} rows and \code{times} columns, where each
#' column is \code{x}.
#'
#' @seealso \code{\link{matrix}}, \code{\link{rbind}}, \code{\link{cbind}}
#'
#' @name copy
#' @examples
#' x <- c(3, 1, 4, 1, 5)
#'
#' # stack x as rows
#' rowCopy(x, 4)
#'
#' # stack x as columns
#' colCopy(x, 4)
#'
#' # with custom dimension names
#' dnames <- list(rows = c("r1", "r2", "r3"), cols = c("c1", "c2", "c3", "c4", "c5"))
#' rowCopy(x, 3, dnames)
NULL

#' @rdname copy
#' @export
colCopy <- function(x,times, dimnames=NULL ) {
  if( !is.vector(x) | is.list(x) ) stop( '"x" must be a vector')
  if( length(times) !=1 | !methods::is(times,"numeric")) stop( '"times" must be a single number')
  if( is.null(dimnames) ) dimnames<-list(names(x),character(0))
  matrix( x, length(x), times, byrow=FALSE, dimnames )

  # alternative code:
  # replicate(times,x)
}

