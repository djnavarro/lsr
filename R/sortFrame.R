

# sortFrame() sorts the cases of a data frame by one or more variables. There's a bit of
# work to do to make the sorting more intuitive, but it's fairly functional now.

#' Sort a data frame
#' @description Sorts a data frame using one or more variables.
#'
#' @param x Data frame to be sorted
#' @param ... A list of sort terms (see below)
#' @param alphabetical Should character vectors be sorted alphabetically?
#'
#' @details The simplest use of this function is to sort a data frame \code{x}
#' in terms of one or more of the variables it contains. If for instance,
#' the data frame \code{x} contains two variables \code{a} and \code{b}, then
#' the command \code{sortFrame(x,a,b)} sorts by variable \code{a}, breaking
#' ties using variable \code{b}. Numeric variables are sorted in ascending
#' order: to sort in descending order of \code{a} and then ascending order
#' of \code{b}, use the command \code{sortFrame(x,-a,b)}. Factors are treated
#' as numeric variables, and are sorted by the internal codes (i.e., the first
#' factor level equals 1, the second factor levels equals 2 and so on).
#' Character vectors are sorted in alphabetical order, which differs from the
#' ordering used by the \code{\link{sort}} function; to use the default 'ascii'
#' ordering, specify \code{alphabetical=FALSE}. Minus signs can be used in
#' conjunction with character vectors in order to sort in reverse alphabetical
#' order. If \code{c} represents a character variable, then \code{sortFrame(x,c)}
#' sorts in alphabetical order, whereas \code{sortFrame(x,-c)} sorts in reverse
#' alphabetical order.
#'
#' It is also possible to specify more complicated sort terms by including
#' expressions using multiple variables within a single term, but care is
#' required. For instance, it is possible to sort the data frame by the sum of
#' two variables, using the command \code{sortFrame(x, a+b)}. For numeric
#' variables expressions of this kind should work in the expected manner, but
#' this is not always the case for non-numeric variables: \code{sortFrame} uses
#' the \code{\link{xtfrm}} function to provide, for every variable referred to
#' in the list of sort terms (\code{...}) a numeric vector that sorts in the
#' same order as the original variable. This reliance is what makes reverse
#' alphabetical order (e.g., \code{sortFrame(x,-c)}) work. However, it also
#' means that it is possible to specify somewhat nonsensical sort terms for
#' character vectors by abusing the numerical coding (e.g.
#' \code{sortFrame(x,(c-3)^2)}; see the examples section). It also means
#' that sorting in terms of string operation functions (e.g., \code{nchar})
#' do not work as expected. See examples section. Future versions of
#' \code{sortFrame} will (hopefully) address this, possibly by allowing the
#' user to "switch off" the internal use of \code{xtfrm}, or else by allowing
#' \code{\link{AsIs}} expressions to be used in sort terms.
#'
#' @return The sorted data frame
#' @export
#'
#' @seealso \code{\link{sort}}, \code{\link{order}}, \code{\link{xtfrm}}
#'
#' @examples
#' txt <- c("bob","Clare","clare","bob","eve","eve")
#' num1 <- c(3,1,2,0,0,2)
#' num2 <- c(1,1,3,0,3,2)
#' etc <- c("not","used","as","a","sort","term")
#' dataset <- data.frame( txt, num1, num2, etc, stringsAsFactors=FALSE )
#'
#' sortFrame( dataset, num1 )
#' sortFrame( dataset, num1, num2 )
#' sortFrame( dataset, txt )
#'
sortFrame <- function(x,..., alphabetical = TRUE){

  if( !methods::is(x,"data.frame") ) {
    stop( '"x" must be a data frame')
  }
  if( !methods::is(alphabetical,"logical") | length(alphabetical) !=1 ) {
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
