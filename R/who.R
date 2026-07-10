
# who() returns a data frame containing basic information about the variables
# in the workspace. It has an S3 print method (defined below) that displays
# the information in a convenient way


#' Contents of workspace
#'
#' @description Prints a summary of all objects in the workspace, showing
#' each object's name, class, and size.
#'
#' @param expand Set to \code{TRUE} to also list the variables inside any
#'   data frames in the workspace. Defaults to \code{FALSE}.
#'
#' @details Shows each object's name, class, and size. For objects with
#' explicit dimensions (e.g., data frames, matrices) the size is shown as
#' rows x columns; for other objects it is the length. Size is only shown
#' for objects whose mode is \code{numeric}, \code{character}, \code{logical},
#' \code{complex}, or \code{list}.
#'
#' Shows more information than \code{\link{objects}} (especially for variables
#' inside data frames) but less detail than \code{\link{ls.str}}.
#'
#' @return Prints the workspace summary and invisibly returns the underlying
#' data (a data frame with columns \code{Name}, \code{Class}, and \code{Size}),
#' which can be assigned to a variable and inspected if needed.
#'
#' @seealso \code{\link{objects}}, \code{\link{ls.str}}
#'
#' @export
#'
#' @examples
#' cats <- 4
#' mood <- "happy"
#' who()
#'
#' dataset <- data.frame(
#'   hi = c("hello", "cruel", "world"),
#'   pi = c(3, 1, 4)
#' )
#'
#' who()
#' who(expand = TRUE)
#'
who <- function(expand = FALSE) {

  if( !methods::is(expand,"logical") || length(expand) != 1 || is.na(expand) ) {
    stop( '"expand" must be a single logical value')
  }

  # extract a list of objects in the parent environment
  envir <- parent.frame()
  varnames <- objects( envir )

  # check to see if it's empty
  if ( length(varnames) == 0 ) {
    obj <- character(0)
    class(obj) <- "whoList"
    return(obj)
  }

  # define getWhoInfo as a subfunction
  getWhoInfo <- function(varnames, envir, is.df) {
    if( is.df ) { # data frame?
      df <- varnames
      x <- eval( as.name( df ), envir = envir ) # get the data frame
      varnames <- names(x) # get names of variables
    }
    n <- length(varnames)           # how many objects
    classes <- vector(length = n)   # all output lengths include class info
    info <- vector(length = n)      # output lengths 3+ use info
    for (i in 1:n) {
      if( is.df ) {
         var <- varnames[i]
         c <- call("$", as.name(df), as.name(var)) # construct a call
         v <- eval(c, envir) # evaluate call (i.e., extract variable)
      }
      else  { v <- eval( as.name( varnames[i] ), envir ) }
      classes[i] <- class(v)[1]  # class
      if( mode(v) %in% c("logical","numeric","complex","character","list") ) {
        if( is.null(dim(v)) ) { info[i] <- length(v) } # size = length
        else { info[i] <- paste( dim(v), collapse = " x ") }  # size = dimensions
      } else { info[i] <- "" } # size = nothing
    }
    if ( is.df ) { varnames <- paste( "$",varnames,sep="") }  # expand names?
    obj <- data.frame(varnames, classes, info, stringsAsFactors = FALSE)
    names(obj) <- c("Name","Class","Size")
    return(obj)
  }

  # get info
  obj <-  getWhoInfo(varnames[1], envir, 0) #hack!!!
  for (v in varnames) {
    obj2 <- getWhoInfo(v, envir, 0)
    obj <- rbind(obj, obj2 )
    if (expand) {
      if( inherits(eval( as.name(v), envir = envir), "data.frame")) {
      	obj2 <- getWhoInfo(v, envir, 1)   # get info
      	obj <- rbind(obj,obj2)  # add the expanded info to the output
  	  }
  	}
  }
  obj <- obj[-1,] #hack!!!
  class(obj) <- "whoList"
  return(obj)

}


#' Print workspace summary
#'
#' @description Prints a workspace summary in a readable format. This function
#' is called automatically whenever a result from \code{\link{who}} is
#' displayed.
#'
#' @param x A workspace summary, as returned by \code{\link{who}}.
#' @param ... Additional arguments (unused, included for compatibility).
#'
#' @return Invisibly returns \code{x} unchanged.
#' @export
print.whoList <- function(x,...) {

    if( length(x) ==0 ) {
      cat("No variables found\n")
    }
    else{
      obj <- x # copy x
      class(obj) <- "data.frame"  # this is okay
      ind <- grep('\\$', obj$Name)  # variables inside a data frame
      obj$Name[ind] <- paste(" ", obj$Name[ind], sep = "")   # add indent
      m <- as.matrix(format.data.frame(obj, digits = NULL, na.encode = FALSE)) # matrix
      dimnames(m)[[2]] <- paste( '--', dimnames(m)[[2]], '--', sep = " " ) # tweak header
      row.names(m) <- rep.int("",dim(m)[1]) # hide row names
      print(m, quote = FALSE, right = FALSE, print.gap = 3) # now print
    }
    return( invisible(x) )

}




