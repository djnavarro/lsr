

# expandFactors() takes a data frame as input, and returns the same data frame,
# but with all factor variables replaced by the corresponding contrasts. It's
# actually just a wrapper to model.matrix()
expandFactors <- function( data, ... ) {

  attr(data,"na.action") <- stats::na.pass # don't drop NA
  df <- stats::model.matrix( stats::as.formula( paste("~",names(data),collapse="+")), data, ... )
  print(df)
  df <- df[,-1,drop=FALSE] # remove intercept
  attr(df,"contrasts") <- NULL
  attr(df,"assign") <- NULL

  return( as.data.frame(df) )
}
