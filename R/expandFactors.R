

# expandFactors() takes a data frame as input, and returns the same data frame,
# but with all factor variables replaced by the corresponding contrasts. It's
# actually just a wrapper to model.matrix()

#' Expand factors to a set of contrasts
#'
#' @description Substitutes all factors in a data frame with the set of
#' contrasts with which that factor is associated
#'
#' @param data A data frame.
#' @param ... Additional arguments to be passed to model.matrix
#'
#' @details The \code{expandFactors} function replaces all of the factors
#' in a data frame with the set of contrasts output by the \code{contrasts}
#' function or \code{model.matrix}. It may be useful for teaching purposes
#' when explaining relationship between ANOVA and regression.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' grading <- data.frame( teacher = factor( c("Amy","Amy","Ben","Ben","Cat") ),
#'                        gender = factor( c("male","female","female","male","male") ),
#'                        grade = c(75,80,45,50,65) )
#'
#' # expand factors using the default contrasts (usually treatment contrasts)
#' expandFactors( grading )
#'
#' # specify the contrasts using the contrasts.arg argument to model.matrix
#' my.contrasts <- list( teacher = "contr.helmert", gender = "contr.treatment" )
#' expandFactors( grading, contrasts.arg = my.contrasts )
#'
expandFactors <- function( data, ... ) {

  attr(data,"na.action") <- stats::na.pass # don't drop NA
  df <- stats::model.matrix( stats::as.formula( paste("~",names(data),collapse="+")), data, ... )
  print(df)
  df <- df[,-1,drop=FALSE] # remove intercept
  attr(df,"contrasts") <- NULL
  attr(df,"assign") <- NULL

  return( as.data.frame(df) )
}
