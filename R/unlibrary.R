

# unlibrary() removes a package from the search path. It's just a wrapper to detach() that
# allows the user to remove packages using the same syntax that they use to load it via
# library()

#' Unload a package
#'
#' @description A wrapper function to \code{\link{detach}} that removes a package
#' from the search path, but takes a package name as input similar to \code{\link{library}}.
#'
#' @param package A package name, which may be specified with or without quotes.
#'
#' @details Unloads a package. This is just a wrapper for the \code{detach}
#' function. However, the \code{package} argument is just the name of the
#' package (rather than the longer string that is required by the \code{detach}
#' function), and -- like the \code{library} function -- can be specified
#' without quote marks. The \code{unlibrary} function does not unload dependencies,
#' only the named package.
#'
#' The name "unlibrary" is a bit of an abuse of both R terminology (in which
#' one has a library of packages) and the English language, but I think it
#' helps convey that the goal of the \code{unlibrary} function is to do the
#' opposite of what the \code{library} function does.
#'
#' @return Identical to \code{detach}.
#' @export
#'
#' @seealso \code{\link{library}}, \code{\link{require}}, \code{\link{detach}}
#'
unlibrary <- function(package) {
  env.name <- deparse(substitute(package))   # allow input to drop the quote marks
  env.name <- paste("package:", env.name, sep="")   # add the "package:" bit (TODO: this is hacky)
  detach(name = env.name, character.only = TRUE)   # now detach it
}
