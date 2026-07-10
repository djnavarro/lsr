

# expandFactors() takes a data frame as input, and returns the same data frame,
# but with all factor variables replaced by the corresponding contrasts. It's
# actually just a wrapper to model.matrix()

#' Expand factors to a set of contrasts
#'
#' @description Replaces each factor variable in a data frame with its
#' contrast-coded columns, leaving numeric variables unchanged.
#'
#' @param data A data frame.
#' @param ... Additional arguments passed to \code{\link{model.matrix}}, such
#'   as \code{contrasts.arg} for specifying non-default contrast schemes.
#'
#' @details Each factor in \code{data} is replaced by the numeric contrast
#' columns that \code{\link{model.matrix}} would generate for that factor
#' (using treatment contrasts by default). Numeric variables pass through
#' unchanged. This can be helpful when illustrating the connection between
#' ANOVA and regression.
#'
#' @return A data frame with factor columns replaced by numeric contrast
#' columns.
#'
#' @seealso \code{\link{model.matrix}}, \code{\link{contrasts}}
#'
#' @export
#'
#' @examples
#' grading <- data.frame(
#'   teacher = factor(c("Amy", "Amy", "Ben", "Ben", "Cat")),
#'   gender  = factor(c("male", "female", "female", "male", "male")),
#'   grade   = c(75, 80, 45, 50, 65)
#' )
#'
#' # expand using the default contrasts (treatment contrasts)
#' expandFactors(grading)
#'
#' # specify different contrasts via contrasts.arg
#' my.contrasts <- list(teacher = "contr.helmert", gender = "contr.treatment")
#' expandFactors(grading, contrasts.arg = my.contrasts)
#'
expandFactors <- function( data, ... ) {

  if( !methods::is(data, "data.frame") ) stop( '"data" must be a data frame')

  attr(data,"na.action") <- stats::na.pass # don't drop NA
  df <- stats::model.matrix( stats::as.formula( paste("~",names(data),collapse="+")), data, ... )
  df <- df[,-1,drop=FALSE] # remove intercept
  attr(df,"contrasts") <- NULL
  attr(df,"assign") <- NULL

  return( as.data.frame(df) )
}
