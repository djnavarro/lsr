
# rmAll() removes all variables from the workspace. It's not noticeably
# different to rm( list=objects() ), and it's largely unnecessary in
# RStudio because there's a button you can click to do the same thing,
# but it can be handy for beginners who don't yet understand how arguments
# work.

#' Remove all objects
#'
#' @description Removes all objects from the workspace
#'
#' @param ask Logical value indicating whether to ask user to confirm deletions. Default is \code{TRUE}
#'
#' @details The \code{rmAll} function provides a simple way of deleting all
#' objects from the workspace. It is almost equivalent to the usual
#' \code{rm(list = objects())} command. The only difference that it requires
#' the user to confirm the deletions first if \code{ask = TRUE}, after
#' displaying a list of the current objects in the worspace. This can
#' occasionally be useful for teaching purposes.
#'
#' @return Invisibly returns 0 if no deletions are made, 1 if at least one deletion is made.
#'
#' @seealso \code{\link{rm}}
#'
#' @export
#'
rmAll <- function(ask = TRUE) {

  # preliminaries
  env <- parent.frame() # evaluate in the parent frame
  object.list <- objects(env) # and the list of objects


  # ask the user if they mean it...
  if ( ask ) {

    # don't bother if already empty
    if (length(object.list) == 0) {
      print("Workspace is already empty")
      return( invisible(1) )
    }

    # first, display all the objects...
    cat("Current contents of workspace:\n")
    print( object.list )

    # then ask user for a decision...
    full.prompt <- paste( "Remove all objects? [y/n] ",sep = " ")
    response <- NA
    while( !(response %in% c("y","n")) ) {
      response <- readline( full.prompt )
    }

    # bail out if necessary
    if( response == "n" ) {
      return( invisible(0) )
    }

  }

  # remove everything and return
  rm( list = object.list, envir = env ) # ... remove all objects
  return( invisible(1) ) # ... return with invisible flag

}
