

# importList() copies each element of a list into a separate variable in the workspace.


#' Import a list
#'
#' @description Creates variables in the workspace corresponding to the elements of a list
#'
#' @param x List to be imported
#' @param ask Should R ask the user to confirm the new variables before creating them? (default is \code{TRUE})
#'
#' @details The \code{importList} function creates variables in the parent
#' environment (generally the global workspace) corresponding to each of the
#' elements of the list object \code{x}. If the names of these elements do
#' not correspond to legitimate variables names they are converted using
#' the \code{\link{make.names}} functions to valid variables names.
#'
#' @return Invisibly returns \code{0} if the user chooses not to import the
#' variables, otherwise invisibly returns \code{1}.
#'
#' @seealso
#' \code{\link{unlist}},
#' \code{\link{attach}}
#'
#' @export
#'
#' @examples
#'
#' # data set organised into two groups
#' data <- c(1,2,3,4,5)
#' group <- c("group A","group A","group B","group B","group B")
#'
#' # the split function creates a list with two elements
#' # named "group A" and "group B", each containing the
#' # data for the respective groups
#' data.list <- split( data, group )
#'
#' # The data.list variable looks like this:
#'
#' #   $`group A`
#' #   [1] 1 2
#' #
#' #   $`group B`
#' #   [1] 3 4 5
#'
#' # importing the list with the default value of ask = TRUE will
#' # cause R to wait on the user's approval. Typing this:
#'
#' #   importList( data.list )
#'
#' # would produce the following output:
#'
#' #   Names of variables to be created:
#' #   [1] "group.A" "group.B"
#' #   Create these variables? [y/n]
#'
#' # If the user then types y, the new variables are created.
#'
#' # this version will silently import the variables.
#' importList( x = data.list, ask = FALSE )
#'
importList <- function(x, ask = TRUE ) {

  if( !methods::is(x,"list") & !methods::is(x,"data.frame")) stop( '"x" must be a list or data frame')
  if( !methods::is(ask,"logical") | length(ask) !=1 ) {
    stop( '"ask" must be a single logical value')
  }

  envir = parent.frame() # import to parent environment

  vars <- names(x)  # get variable names
  vars <- make.unique(vars) # make sure the names are unique
  vars <- make.names(vars) # convert to legitimate R names

  if( ask ) {
    cat("Names of variables to be created:\n")
    print(vars)
    ans <- NA
    while( ! (ans %in% c("y","n") ) ) {
      ans <- readline("Create these variables? [y/n] ")
    }
    if (ans == "n") {
      return( invisible(0) )
    }
  }

  for (v in seq_along(vars)) { # for each variable in x:
    assign(x = vars[v], value = x[[v]], envir = envir)
  }
  return( invisible(1) )

}
