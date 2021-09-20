
#' @title Copies a vector into a matrix
#'
#' @param x The vector to be copied
#' @param times Number of copies of the vector to bind together
#' @param dimnames List specifying row and column names
#'
#' @details This is a convenience function for binding together multiple copies
#' of the same vector. The intended usage is for situations where one might
#' ordinarily use \code{rbind} or \code{cbind}, but the work is done by the
#' \code{matrix} function. Instead of needing to input multiple copies of the
#' input vector \code{x} (as one would for \code{rbind}), one only needs to
#' specify the number of \code{times} that the vector should be copied.
#'
#' @return For \code{rowCopy}, the output is a matrix with \code{times} rows
#' and \code{length(x)} columns, in which each row contains the vector \code{x}.
#' For \code{colCopy}, each column corresponds to the vector \code{x}.
#'
#' @name copy
#' @examples
#'
#' #Example 1: basic usage
#' data <- c(3,1,4,1,5)
#' rowCopy( data, 4 )
#' colCopy( data, 4 )
#'
#' #Example 2: attach dimension names
#' dnames <- list( rows = c("r1","r2","r3"), cols = c("c1","c2","c3","c4","c5") )
#' rowCopy( data,3,dnames )
NULL

#' @rdname copy
#' @export
colCopy <- function(x,times, dimnames=NULL ) {
  if( !is.vector(x) ) stop( '"x" must be a vector')
  if( length(times) !=1 | !methods::is(times,"numeric")) stop( '"times" must be a single number')
  if( is.null(dimnames) ) dimnames<-list(names(x),character(0))
  matrix( x, length(x), times, byrow=FALSE, dimnames )

  # alternative code:
  # replicate(times,x)
}

