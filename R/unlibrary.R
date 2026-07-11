# unlibrary() removes a package from the search path. It's just a wrapper to detach() that
# allows the user to remove packages using the same syntax that they use to load it via
# library()

#' Unload a package
#'
#' @description Removes a package from the search path, using the same
#' naming convention as \code{\link{library}}.
#'
#' @param package The name of the package to unload, with or without quotes.
#'
#' @details Calls \code{\link{detach}} on the named package. Unlike
#' \code{detach}, which requires the full \code{"package:name"} string,
#' \code{unlibrary} accepts the bare package name (with or without quotes),
#' matching the syntax of \code{\link{library}}. Only the named package is
#' unloaded; dependencies are not affected.
#'
#' @return Called for its side effect of removing the package from the search
#' path. Returns the result of \code{\link{detach}} invisibly.
#'
#' @seealso \code{\link{library}}, \code{\link{detach}}
#'
#' @export
#'
#' @examples
#' if (FALSE) {
#'   # after loading a package with library(), unload it with unlibrary()
#'   unlibrary(MASS)
#' }
#'
unlibrary <- function(package) {
  env.name <- deparse(substitute(package)) # allow input to drop the quote marks
  env.name <- gsub('^"(.*)"$', "\\1", env.name) # strip quotes if user passed a string literal
  env.name <- paste("package:", env.name, sep = "") # add the "package:" bit (TODO: this is hacky)
  detach(name = env.name, character.only = TRUE) # now detach it
}
