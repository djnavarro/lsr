

# sortFrame() sorts the cases of a data frame by one or more variables. There's a bit of
# work to do to make the sorting more intuitive, but it's fairly functional now.

#' Sort a data frame
#'
#' @description Sorts a data frame by one or more of its variables.
#'
#' @param x A data frame to be sorted.
#' @param ... One or more unquoted variable names to sort by, in order of
#'   priority. Prefix a variable name with \code{-} to sort in descending
#'   order (e.g., \code{sortFrame(x, -a, b)} sorts descending by \code{a},
#'   then ascending by \code{b}).
#' @param alphabetical Set to \code{TRUE} (the default) to sort character
#'   variables case-insensitively in alphabetical order. Set to \code{FALSE}
#'   to use the locale-dependent ordering used by \code{\link{sort}}.
#'
#' @details Sorts the rows of \code{x} by the variables listed in \code{...}.
#' Numeric variables and factors are sorted by their numeric values (for
#' factors, this corresponds to level order). Character variables are sorted
#' alphabetically by default, ignoring case; prefix with \code{-} for reverse
#' alphabetical order.
#'
#' Simple expressions combining variables are also accepted (e.g.,
#' \code{sortFrame(x, a + b)} sorts by the sum of \code{a} and \code{b}),
#' though care is required: the sort uses \code{\link{xtfrm}} internally to
#' convert variables to sortable numeric codes, which can produce unexpected
#' results for character variables when expressions other than \code{-}
#' are used.
#'
#' @return The sorted data frame.
#'
#' @seealso \code{\link{order}}, \code{\link{sort}}, \code{\link{xtfrm}}
#'
#' @export
#'
#' @examples
#' dataset <- data.frame(
#'   txt  = c("bob", "Clare", "clare", "bob", "eve", "eve"),
#'   num1 = c(3, 1, 2, 0, 0, 2),
#'   num2 = c(1, 1, 3, 0, 3, 2),
#'   stringsAsFactors = FALSE
#' )
#'
#' sortFrame(dataset, num1)           # sort by num1 ascending
#' sortFrame(dataset, num1, num2)     # sort by num1 then num2
#' sortFrame(dataset, -num1)          # sort by num1 descending
#' sortFrame(dataset, txt)            # sort alphabetically (case-insensitive)
sortFrame <- function(x,..., alphabetical = TRUE){

  if( !methods::is(x,"data.frame") ) {
    stop( '"x" must be a data frame')
  }
  if( !methods::is(alphabetical,"logical") || length(alphabetical) !=1 || is.na(alphabetical) ) {
    stop( '"alphabetical" must be a single logical value')
  }

  dots <- as.list(substitute(list(...)))[-1L] # list of quoted sort terms
  if( length(dots) == 0 ){ return(x) } # do nothing if null arguments
  rel.vars <- unlist(lapply(dots,all.vars)) # which variables are referred to
  y <- lapply(x[rel.vars], xtfrm) # numeric frame that sorts equivalently
  if( alphabetical == TRUE ) { # case conversion if necessary...
    char.vars <- unlist(lapply(x, methods::is,"character")) # find character vars
    char.vars <- names(which(char.vars)) # relevant variable names
    n <- length(y[[1]]) # number of cases
    for( v in char.vars ) {
      z <- xtfrm(tolower(x[[v]])) # sorts equivalently to lower case version
      y[[v]] <- z + y[[v]]/(n+1) # original only breaks ties
    }
  }

  ord <- with(y, do.call(order, dots)) # the sort order
  return( x[ord,] ) # sort and return

}
