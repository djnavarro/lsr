

# permuteLevels() allows the user to reorder the levels of a factor according
# to some arbitrary permutation. It's much like relevel() but more general.


#' Permute the levels of a factor
#'
#' @description Apply an arbitrary permutation to the ordering of levels
#' within a factor
#'
#' @param x The factor to be permuted
#' @param perm A vector specifying the permutation
#' @param ordered Should the output be an ordered factor?
#' @param invert Use the inverse of \code{perm} to specify the permutation
#'
#' @details This is a convenience function used to shuffle the order in
#' which the levels of a factor are specified. It is similar in spirit to
#' the \code{relevel} function, but more general. The \code{relevel}
#' function only changes the first level of the factor, whereas
#' \code{permuteLevels} can apply an arbitrary permutation. This can be
#' useful for plotting data, because some plotting functions will display
#' the factor levels in the same order that they appear within the factor.
#'
#' The \code{perm} argument is a vector of the same length as \code{levels(x)},
#' such that \code{perm[k]} is an integer that indicates which of the old
#' levels should be moved to position k. However, if \code{invert=TRUE}, the
#' inverse permutation is applied: that is, \code{perm[k]} is an integer
#' specifying where to move the k-th level of the original factor. See the
#' examples for more details.
#'
#' @return Returns a factor with identical values, but with the ordering
#' of the factor levels shuffled.
#'
#' @export
#'
#' @seealso
#' \code{\link{factor}},
#' \code{\link{order}},
#' \code{\link{relevel}}
#'
#' @examples
#'
#' # original factor specifies the levels in order: a,b,c,d,e,f
#' x <- factor( c(1,4,2,2,3,3,5,5,6,6), labels=letters[1:6] )
#' print(x)
#'
#' # apply permutation (5 3 2 1 4 6)... i.e., move 5th factor level (e)
#' # into position 1, move 3rd factor level (c) into position 2, etc
#' permuteLevels(x,perm = c(5,3,2,1,4,6))
#'
#' # apply the inverse of permutation (5 3 2 1 4 6)... i.e., move 1st
#' # level (a) into position 5, move 2nd level (b) into position 3, etc
#' permuteLevels(x,perm = c(5,3,2,1,4,6),invert=TRUE)
#'
permuteLevels <- function(x,perm,ordered = is.ordered(x),invert=FALSE) {

  if( !methods::is(x,"factor") ) stop( '"x" must be a factor')
  if( !methods::is(perm,"numeric") ) stop('"perm" must be numeric')
  if( length(perm) != nlevels(x)) stop('length of "perm" must equal the number of levels of "x"')
  if( any( (sort(perm) - 1:nlevels(x)) != 0)) stop( '"perm" is not a valid permutation')
  if( !methods::is(ordered,"logical") | length(ordered) !=1 ) {
    stop( '"ordered" must be a single logical value')
  }
  if( !methods::is(invert,"logical") | length(invert) !=1 ) {
    stop( '"invert" must be a single logical value')
  }

  if(invert){ perm <- order(perm) }
  y <- factor( x = order(perm)[as.numeric(x)],
               levels = seq_along(levels(x)),
               labels = levels(x)[perm],
               ordered = ordered )
  return(y)
}
