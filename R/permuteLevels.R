

# permuteLevels() allows the user to reorder the levels of a factor according
# to some arbitrary permutation. It's much like relevel() but more general.


#' Permute the levels of a factor
#'
#' @description Reorder the levels of a factor into any order you specify.
#'
#' @param x A factor.
#' @param perm An integer vector of the same length as \code{nlevels(x)}.
#'   When \code{invert = FALSE} (the default), \code{perm[k]} gives the
#'   index of the old level that should appear in position \code{k} of
#'   the new ordering. When \code{invert = TRUE}, \code{perm[k]} gives
#'   the new position to assign to the \code{k}-th old level.
#' @param ordered Set to \code{TRUE} to return an ordered factor. Defaults
#'   to \code{is.ordered(x)}, preserving the ordered status of the input.
#' @param invert Set to \code{TRUE} to apply the inverse of \code{perm}.
#'   See the \code{perm} description above and the examples below.
#'
#' @details Similar to \code{\link{relevel}}, but more general:
#' \code{relevel} can only move one level to the front, whereas
#' \code{permuteLevels} can place the levels in any order. This is useful
#' when you want to control the order in which levels appear on a plot axis
#' or in a table.
#'
#' @return A factor with the same values as \code{x} but with the levels
#' in the new order specified by \code{perm}.
#'
#' @export
#'
#' @seealso
#' \code{\link{factor}},
#' \code{\link{relevel}},
#' \code{\link{order}}
#'
#' @examples
#' # factor with levels a, b, c, d, e, f (in that order)
#' x <- factor(c(1, 4, 2, 2, 3, 3, 5, 5, 6, 6), labels = letters[1:6])
#' levels(x)
#'
#' # move level e to position 1, c to position 2, b to 3, a to 4, d to 5, f to 6
#' permuteLevels(x, perm = c(5, 3, 2, 1, 4, 6))
#'
#' # using invert = TRUE: move level a to position 5, b to 3, c to 2, etc.
#' permuteLevels(x, perm = c(5, 3, 2, 1, 4, 6), invert = TRUE)
#'
permuteLevels <- function(x,perm,ordered = is.ordered(x),invert=FALSE) {

  if( !methods::is(x,"factor") ) stop( '"x" must be a factor')
  if( !methods::is(perm,"numeric") ) stop('"perm" must be numeric')
  if( length(perm) != nlevels(x)) stop('length of "perm" must equal the number of levels of "x"')
  if( any( (sort(perm) - 1:nlevels(x)) != 0)) stop( '"perm" is not a valid permutation')
  if( !methods::is(ordered,"logical") || length(ordered) !=1 || is.na(ordered) ) {
    stop( '"ordered" must be a single logical value')
  }
  if( !methods::is(invert,"logical") || length(invert) !=1 || is.na(invert) ) {
    stop( '"invert" must be a single logical value')
  }

  if(invert){ perm <- order(perm) }
  y <- factor( x = order(perm)[as.numeric(x)],
               levels = seq_along(levels(x)),
               labels = levels(x)[perm],
               ordered = ordered )
  return(y)
}
