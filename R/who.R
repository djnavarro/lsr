
# who() returns a data frame containing basic information about the variables
# in the workspace. It has an S3 print method (defined below) that displays
# the information in a convenient way


#' Contents of workspace
#'
#' @description Prints out a simple summary of all the objects in the workspace
#'
#' @param expand Should R "expand" data frames when listing variables? If
#' \code{expand = TRUE}, variables inside a data frame are included in the
#' output. The default is \code{FALSE}
#'
#' @details The \code{who} function prints out some basic information about
#' all variables in the workspace. Specifically, it lists the names of all
#' variables, what class they are, and how big they are (see below for
#' specifics). If the \code{expand} argument is \code{TRUE} it will also
#' print out the same information about variables within data frames. See
#' the examples below to see what the output looks like.
#'
#' The purpose for the function is to show more information than the
#' \code{objects} function (especially as regards the names of variables
#' inside data frames), but not to show as much detail as the \code{ls.str}
#' function, which is generally too verbose for novice users.
#'
#' The "size" of an object is only reported for some kinds of object:
#' specifically, only those objects whose \code{\link{mode}} is either
#' \code{numeric}, \code{character}, \code{logical}, \code{complex} or
#' \code{list}. Nothing is printed for any other kind of object. If the
#' object has explicit dimensions (e.g., data frames or matrices) then
#' \code{who} prints out the dimension sizes (e.g., "2 x 3" ). Otherwise
#' the length of the object is printed.
#'
#' @return \code{who} returns an object of class \code{whoList} which is
#' just a data frame with a dedicated print method.
#'
#' @export
#'
#' @seealso \code{\link{objects}}, \code{\link{ls.str}}
#'
#' @examples
#'
#' cats <- 4
#' mood <- "happy"
#' who()
#'
#' dataset <- data.frame(
#'   hi = c( "hello","cruel","world" ),
#'   pi = c( 3,1,4 )
#' )
#'
#' who()
#' who(expand = TRUE)
#'
who <- function(expand = FALSE) {

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


#' Print method for whoList objects
#'
#' @param x An object of class 'whoList'
#' @param ... For consistency with the generic (unused)
#'
#' @return Invisibly returns the original object
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




