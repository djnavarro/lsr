
# rmAll() removes all variables from the workspace. It's not noticeably
# different to rm( list=objects() ), and it's largely unnecessary in
# RStudio because there's a button you can click to do the same thing,
# but it can be handy for beginners who don't yet understand how arguments
# work.

#' Remove all objects from the workspace
#'
#' @description Deletes all objects from the workspace, with an optional
#' confirmation prompt.
#'
#' @param ask Set to \code{TRUE} (the default) to display the current
#'   workspace contents and ask for confirmation before deleting. Set to
#'   \code{FALSE} to delete immediately without prompting.
#'
#' @details Removes all objects from the workspace. When \code{ask = TRUE},
#' the list of objects is printed and the user must type \code{y} to confirm
#' before anything is deleted. This is similar to \code{rm(list = objects())},
#' but with an interactive safety check.
#'
#' @return Invisibly returns \code{1} if objects were deleted, \code{0} if
#' the user declined or the workspace was already empty.
#'
#' @seealso \code{\link{rm}}, \code{\link{objects}}
#'
#' @export
#'
#' @examples
#' if (FALSE) {
#'   # interactive: displays workspace contents and asks for confirmation
#'   rmAll()
#'
#'   # non-interactive: deletes immediately without prompting
#'   rmAll(ask = FALSE)
#' }
rmAll <- function(ask = TRUE) {

  if( !methods::is(ask,"logical") || length(ask) != 1 || is.na(ask) ) {
    stop( '"ask" must be a single logical value')
  }

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
