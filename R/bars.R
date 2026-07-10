

#' Grouped bar plots with error bars
#'
#' @description Creates a bar plot showing group means with error bars showing
#' confidence intervals, broken down by one or two grouping factors.
#'
#' @param formula A two-sided formula of the form
#'   \code{response ~ group1} or \code{response ~ group1 + group2}. The
#'   response variable must be numeric; grouping variables must be factors.
#' @param data An optional data frame containing the variables named in
#'   \code{formula}. If omitted, the variables are looked up in the workspace.
#' @param heightFun The function used to calculate bar heights. Defaults to
#'   \code{mean}. Must return a single number.
#' @param errorFun The function used to calculate error bar positions. Defaults
#'   to \code{\link{ciMean}}. Must return two numbers (lower and upper bounds).
#'   Set to \code{FALSE} to suppress error bars.
#' @param yLabel The y-axis label. Defaults to the name of the response
#'   variable.
#' @param xLabels Labels for the x-axis tick marks. Defaults to the levels of
#'   \code{group1}.
#' @param main The plot title. Defaults to no title.
#' @param ylim A numeric vector of length 2 giving the y-axis limits. The
#'   lower bound defaults to 0; the upper bound is estimated automatically.
#' @param barFillColour A vector of colours used to fill the bars. Defaults to
#'   a pastel rainbow palette.
#' @param barLineWidth The width of the bar border lines. Defaults to \code{2}.
#' @param barLineColour The colour of the bar border lines. Defaults to
#'   \code{"black"}.
#' @param barSpaceSmall The gap between bars within a cluster, as a proportion
#'   of bar width. Defaults to \code{0.2}.
#' @param barSpaceBig The gap separating clusters of bars, as a proportion of
#'   bar width. Defaults to \code{1}.
#' @param legendLabels Labels for the legend entries. Defaults to the levels of
#'   \code{group2}. Set to \code{FALSE} to suppress the legend. No legend is
#'   drawn when only one grouping variable is specified.
#' @param legendDownShift How far below the top of the plot to place the
#'   legend, as a proportion of plot height. Defaults to \code{0}.
#' @param legendLeftShift How far from the right edge to place the legend, as
#'   a proportion of plot width. Defaults to \code{0}.
#' @param errorBarLineWidth The line width for the error bars. Defaults to
#'   \code{1}.
#' @param errorBarLineColour The colour of the error bars. Defaults to
#'   \code{"grey40"}.
#' @param errorBarWhiskerWidth The width of the error bar whiskers, as a
#'   proportion of bar width. Defaults to \code{0.2}.
#'
#' @details Plots group means (or the output of \code{heightFun}) with error
#' bars (or the output of \code{errorFun}) for one or two grouping factors.
#' When two grouping factors are given, \code{group1} determines the primary
#' x-axis grouping and \code{group2} determines the sub-grouping shown as
#' clusters of bars with a legend.
#'
#' Missing values are removed with a warning. At least 2 complete cases are
#' required per group.
#'
#' @return Invisibly returns a data frame containing the factor levels, group
#' summary values, and error bar bounds. This function is primarily used for
#' its side effect of drawing the plot.
#'
#' @seealso
#' \code{\link{barplot}},
#' \code{\link{ciMean}}
#'
#' @export
#'
#' @examples
#' # one grouping factor
#' df <- data.frame(
#'   outcome = c(3, 4, 5, 2, 4, 6, 5, 7, 8),
#'   group = factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c"))
#' )
#' bars(outcome ~ group, data = df)
#'
#' # two grouping factors
#' df2 <- data.frame(
#'   outcome = c(3, 4, 5, 2, 4, 6, 5, 7, 8, 4, 3, 6),
#'   group1  = factor(rep(c("a", "b"), each = 6)),
#'   group2  = factor(rep(c("x", "y", "z"), times = 4))
#' )
#' bars(outcome ~ group1 + group2, data = df2)
bars <- function(
  formula, # two-sided formula specifying the response variable and the grouping factors
  data=NULL, # optional data frame containing the variables
  heightFun=mean, # function used to calculate the bar height for a group
  errorFun=ciMean, # function used to calculate the error bar for a group
  yLabel=NULL, # y-axis label: defaults to the name of the response varialbe
  xLabels=NULL, # x-axis bar labels: defaults to the levels of the 1st factor
  main="", # plot title
  ylim=NULL, # y-axis limit: lower bound defaults to 0, default upper bound estimated
  barFillColour=NULL, # colours to fill the bars (defaults to ???)
  barLineWidth=2, # width of the bar border lines
  barLineColour="black", # colour of the bar border lines
  barSpaceSmall=.2, # size of the gap between bars within a cluster (as a proportion of bar width)
  barSpaceBig=1, # size of the gap separating clusters of bars (as a proportion of bar width)
  legendLabels=NULL, # text for the legend (defaults to the levels of the 2nd factor, if present)
  legendDownShift=0, # how far down is the legend? (as proportion of plot)
  legendLeftShift=0, # how far left is the legend? (as proportion of plot)
  errorBarLineWidth=1, # line width for the error bars
  errorBarLineColour="grey40", # colour of the error bars
  errorBarWhiskerWidth=.2 # width of error bar whiskers (as proportion of bar width)
  ){

  ####### handy functions for later #######

  # function to add a confidence interval
  addOneErrorBar <- function( x, y, ci, wwd, lwd, col  ) {
    graphics::lines( c(x,x),ci, col=col, lwd=lwd )
    graphics::lines( c(x-wwd/2,x+wwd/2), c(ci[2],ci[2]), col=col, lwd=lwd )
    graphics::lines( c(x-wwd/2,x+wwd/2), c(ci[1],ci[1]), col=col, lwd=lwd )
  }

  # function to get a factor level
  factorLevels <- function( j ) {
    levels(data[,gpName[j]])
  }

  # function checking that an input is numeric and of a specific length
  isNumbers <- function( x, n=NULL ) {
    out <- inherits(x, c("numeric","integer"))
    if( !is.null(n)) out <- out && length(x)==n
    return(out)
  }

#   TODO: support transparency (ie. 8-digit hex), check if I've missed anything
#   # function checking that a vector of inputs specify colours
#   isColours <- function (x) {
#     isOK <- rep.int(FALSE,length(x))
#     if( is(x,"character")) {
#       pattern <- paste0( "^#", paste(rep.int("[0-9A-Fa-f]",6),collapse=""), "$" )
#       isOK[ grep(pattern,x) ] <- TRUE
#       isOK <- isOK | (x %in% colours())
#     }
#     if( isNumbers(x) ){
#       isOK <- TRUE # numbers are rounded and plugged into colours()
#     }
#     return(all(isOK))
#   }

  ####### setup and input checking #######

  # CHECK that those inputs that should be exactly one number are in fact one number
  if( !isNumbers( barLineWidth, 1) ) stop( "'barLineWidth' must be a single number")
  if( !isNumbers( barSpaceSmall, 1) ) stop( "'barSpaceSmall' must be a single number")
  if( !isNumbers( barSpaceBig, 1) ) stop( "'barSpaceBig' must be a single number")
  if( !isNumbers( legendDownShift, 1) ) stop( "'legendDownShift' must be a single number")
  if( !isNumbers( legendLeftShift, 1) ) stop( "'legendLeftShift' must be a single number")
  if( !isNumbers( errorBarLineWidth, 1) ) stop( "'errorBarLineWidth' must be a single number")
  if( !isNumbers( errorBarWhiskerWidth, 1) ) stop( "'errorBarWhiskerWidth' must be a single number")

#  (currently doesn't work properly, so the check is removed)
#  # CHECK that the colour inputs are vectors of colours, but no more
#  if( !is.null(barFillColour) && !isColours( barFillColour )) stop( "'barFillColour' must specify colours" )
#  if( !isColours( barLineColour )) stop( "'barLineColour' must specify colours" )
#  if( !isColours( errorBarLineColour )) stop( "'errorBarLineColour' must specify colours" )

  # CHECK that titles & labels can be coerced to character
  if( methods::is( try( as.character(main), silent=TRUE), "try-error" ))
    stop( "'main' cannot be coerced to character" )
  if( !is.null(yLabel) && methods::is( try( as.character(yLabel), silent=TRUE), "try-error" ))
    stop( "'yLabel' cannot be coerced to character" )
  if( !is.null(xLabels) && methods::is( try( as.character(xLabels), silent=TRUE), "try-error" ))
    stop( "'xLabels' cannot be coerced to character" )
  if( !is.null(legendLabels) && methods::is( try( as.character(legendLabels), silent=TRUE), "try-error" ))
    stop( "'legendLabels' cannot be coerced to character" )

  # WARN if the title or y-axis label lengths don't make sense
  if( length( main ) !=1 ) warning( "'main' does not usually have more than one element" )
  if(  !is.null(yLabel) && length( yLabel ) !=1 ) warning( "'yLabel' does not usually have more than one element" )

  # CHECK that ylim is numeric of length 2
  if( !is.null(ylim) && !isNumbers( ylim, 2)) stop( "'ylim' must be a numeric vector with two elements")

  # CHECK that formula is a two sided formula
  if( !methods::is(formula,"formula")) stop( "'formula' input must be a formula" )
  if( length(formula) !=3 ) stop( "'formula' must be two sided")

  # get the variable names
  responseName <- all.vars(formula[[2]]) # string containing name of the response variable
  gpName <- all.vars(formula[[3]]) # vector of strings naming the grouping variables
  nFactors <- length( gpName ) # how many grouping variables were input

  # CHECK variable numbers in the formula: one reponse, one or two groups
  if( length( responseName ) != 1) stop( "'formula' must have one response variable")
  if( nFactors == 0) stop( "'formula' must have at least one grouping variable")
  if( nFactors > 2) stop( "'formula' cannot have more than two grouping variables")

  # CHECK data frame:
  if( !is.null(data) ) {

    # if it's specified, it needs to be data frame, because a matrix can't
    # contain both factors and numeric variables
    if( !methods::is(data,"data.frame") ) stop ( "'data' is not a data frame")

  } else {

    # copy variables into a data frame if none is specified, and
    # CHECK that the variables are appropriate for a data frame
    data <- try( eval( stats::model.frame( formula = formula ),
                       envir=parent.frame() ), silent=TRUE)
    if( methods::is(data,"try-error") ) {
      stop( "cannot create data frame from variables in 'formula'.
            Possible reason: variables are not all the same length")
    }
  }

  # CHECK that all grouping variables are factors
  if( !all(sapply( gpName, function(x){methods::is(data[,x],"factor")} )) ) {
    stop( 'grouping variables must be factors')
  }

  # CHECK that the response is numeric or integer
  if( !isNumbers( data[,responseName]) ) {
    stop( 'response variable must be numeric')
  }

  # CHECK that heightFun is a function that takes arguments
  if( !methods::is(heightFun,"function")) stop( "'heightFun' must be a function")
  if( length(formals(heightFun))==0) stop( "'heightFun' must accept inputs")

  # CHECK that errorFun is a function, or else equals FALSE
  if( !methods::is(errorFun,"function") ) { # if it's not a function
    if( length(errorFun) != 1 || errorFun != FALSE ) { # and it's not FALSE
      stop( "'errorFun' must be a function, or FALSE") # complain
    }
  } else { # okay it's a function, CHECK that it takes arguments:
    if( length(formals(errorFun))==0) stop( "'errorFun' must accept inputs")
  }

  # get number of levels for each grouping factor
  nLevels <- sapply( gpName, function(x){nlevels(data[,x])})

  # CHECK more label lengths, now that we know the number of levels
  if( !is.null(xLabels) && length(xLabels) != nLevels[1] )
    stop( paste0( "'xLabels' has ", length(xLabels), " elements, but '",
                    gpName[1], "' has ", nLevels[1], " levels" ))
  if( nFactors==2 && !is.null(legendLabels) && length(legendLabels) != nLevels[2] )
    stop( paste0( "'legendLabels' has ", length(legendLabels), " elements, but '",
                  gpName[2], "' has ", nLevels[2], " levels" ))

  # see if error bars and/or legend are to be plotted
  showErrorBars <- methods::is(errorFun,"function")
  showLegend <- is.null(legendLabels) || length(legendLabels) > 1 || legendLabels != FALSE
  showLegend <- showLegend & nFactors==2 # currently no legends for the one-factor case

  # REMOVE missing data and CHECK cell counts
  completeCases <- !apply( is.na( data[,c(gpName,responseName)] ), 1, any )
  data <- data[ completeCases, ]
  if( any(!completeCases)) {
    warning( paste( sum(!completeCases), "cases removed due to missing data" ))
  }
  counts <- table(data[,gpName])
  if( min(counts) < 2 ) stop( "at least 2 complete cases are needed for each group" )


  ####### compute bar heights and error bar locations #######

  # compute heights
  barHeights <- stats::aggregate( formula, data, heightFun )

  # CHECK that heightFun produced one number as output. Multiple outputs would
  # mean that barHeights[[nFactors+1]] would be a matrix/data frame. So we
  # check to see if it's a numeric vector instead
  if( !isNumbers( barHeights[[nFactors+1]] )) {
    stop( "'heightFun' must output a single number" )
  }

  # compute error bars if requested
  if( showErrorBars ) {
    errorBars <- stats::aggregate( formula, data, errorFun )

    # CHECK that errorFun produced two numbers as output
    dims <- dim(errorBars[[nFactors+1]])
    if( is.null(dims) || length(dims) !=2 || dims[2] !=2 ||
        !isNumbers(errorBars[[nFactors+1]][,1]) ||
        !isNumbers(errorBars[[nFactors+1]][,2]) ) {
      stop( "'errorFun' must output two numbers" )
    }
  }


  ####### compute default values where needed #######

  # customising the axes & title
  if( is.null( yLabel )) yLabel <- responseName
  if( is.null( xLabels )) xLabels <- factorLevels(1)
  if( is.null( ylim )) {
    ylim <- c(0, max(barHeights[[nFactors+1]])*1.2)
    if( showErrorBars ) ylim <- c(0, max( ylim[2], errorBars[[nFactors+1]]*1.1))
  }

  # customising the style of the bars
  if (is.null( barFillColour )) {
    if( nFactors==1 ) barFillColour <-grDevices::rainbow(nLevels[1], s=.3)
    if( nFactors==2 ) barFillColour <- grDevices::rainbow(nLevels[2], s=.3)
  }

  # legend
  if( is.null( legendLabels )) legendLabels <- factorLevels(nFactors)


  ####### bar plot #######

  old.lwd <- graphics::par()$lwd # store old graphics parameters
  graphics::par( lwd=barLineWidth ) # set new graphics parameters

  if( nFactors==2 ) { # format height data for the two-factor case
    h <- matrix( barHeights[[3]], nrow=nLevels[2],
                 ncol=nLevels[1], byrow=TRUE )
    b <- TRUE

  } else { # format height data for the one-factor case
    h <- barHeights[[2]]
    b <- FALSE
  }

  # barplot
  xloc <- graphics::barplot( height=h, beside=b, legend.text=FALSE, width=1,
                   ylab=yLabel, space=c(barSpaceSmall,barSpaceBig),
                   ylim=ylim, col=barFillColour, border=barLineColour,
                   main=main, font.main=1, names.arg=xLabels )

  ####### legend #######

  if( showLegend ){

    graphics::legend( x="topright", # default location
            inset=c(legendLeftShift,legendDownShift), # user specified shift
            xjust=0,yjust=0, # location refers to top right of legend area
            legend=legendLabels, # labels
            fill=barFillColour, # colours
            bty="n", # no box
            xpd=TRUE # allow legend to move outside plot region
          )  # barLineColour ???
  }


  ####### error bars #######

  if( showErrorBars ) {

    # match the row order of errorBars to the plot order!
    if(nFactors==2) { # two-factor case is awkward
      k <- 0
      ind <- vector( length=dim(errorBars)[1])
      for( i in 1:nLevels[1]) {
        for( j in 1:nLevels[2]) {
          k <- k+1
          ind[k] <- which( errorBars[,1]==factorLevels(1)[i] &
                           errorBars[,2]==factorLevels(2)[j] )
        }
      }
    } else { # one-factor case is easy
      ind <- 1:nLevels[1]
    }

    # recycle colours
    tmp <- as.numeric( gl( length(errorBarLineColour),1, length(xloc) ))
    errorBarLineColour <- errorBarLineColour[tmp]

    # draw the error bars
    for( k in 1:dim(errorBars)[1]) {
      addOneErrorBar( xloc[k],barHeights[[nFactors+1]][i],
                      errorBars[[nFactors+1]][ind[k],], errorBarWhiskerWidth,
                      errorBarLineWidth, errorBarLineColour[k] )
    }


  }


  ####### clean up #######

  # reset graphics
  #par( old.par )
  graphics::par( lwd=old.lwd )


  # concatenate for output
  out <- cbind( barHeights[,1:nFactors,drop=FALSE],
                barHeights[,nFactors+1,drop=FALSE] )
  if( showErrorBars ) {
    out <- cbind( out, errorBars[[nFactors+1]])
  }

  return(invisible(out))

}
