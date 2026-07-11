# importList() copies each element of a list into a separate variable in the workspace.


#' Import a list into the workspace
#'
#' @description Copies each element of a list into a separate variable in the
#' workspace, using the list element names as variable names.
#'
#' @param x A list or data frame whose elements are to be imported as
#'   individual variables.
#' @param ask Set to \code{TRUE} (the default) to display the names of the
#'   variables that will be created and ask for confirmation before
#'   proceeding. Set to \code{FALSE} to import silently.
#'
#' @details Creates one variable per list element in the calling environment
#' (usually the global workspace). Element names that are not valid R variable
#' names are automatically converted using \code{\link{make.names}}.
#'
#' @return Called primarily for its side effect of creating variables in the
#' workspace. Invisibly returns \code{1} if variables were created, \code{0}
#' if the user declined.
#'
#' @seealso \code{\link{unlist}}, \code{\link{attach}}
#'
#' @export
#'
#' @examples
#' values <- c(1, 2, 3, 4, 5)
#' group <- c("group A", "group A", "group B", "group B", "group B")
#'
#' # split() returns a named list: one element per group
#' grp_list <- split(values, group)
#'
#' # import silently (no confirmation prompt)
#' importList(grp_list, ask = FALSE)
#'
#' if (FALSE) {
#'   # interactive: shows variable names and asks for confirmation
#'   importList(grp_list)
#' }
#'
importList <- function(x, ask = TRUE) {
  if (!methods::is(x, "list") & !methods::is(x, "data.frame")) stop('"x" must be a list or data frame')
  if (!methods::is(ask, "logical") || length(ask) != 1 || is.na(ask)) {
    stop('"ask" must be a single logical value')
  }

  envir <- parent.frame() # import to parent environment

  # check for empty list
  if (length(x) == 0) {
    message("Nothing to import: the list is empty")
    return(invisible(0))
  }

  # check that all elements are named
  vars <- names(x)
  if (is.null(vars) || any(vars == "")) {
    stop('"x" must be a named list or data frame: all elements must have names')
  }

  vars <- make.unique(vars) # make sure the names are unique
  vars <- make.names(vars) # convert to legitimate R names

  if (ask) {
    cat("Names of variables to be created:\n")
    print(vars)
    ans <- NA
    while (!(ans %in% c("y", "n"))) {
      ans <- readline("Create these variables? [y/n] ")
    }
    if (ans == "n") {
      return(invisible(0))
    }
  }

  for (v in seq_along(vars)) { # for each variable in x:
    assign(x = vars[v], value = x[[v]], envir = envir)
  }
  return(invisible(1))
}
