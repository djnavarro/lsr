
# compute correlation matrix:
#  - automatically removes non-numeric variables from the data
#  - automatically does "pairwise.complete.obs" for handling missing data
#  - can report the result of correlation tests if requested
#  - can do pearson, spearman and kendall
#  - defaults to no test, and to pearson correlation

#' Correlation matrices
#'
#' @description Computes a correlation matrix, optionally with hypothesis tests
#' and corrections for multiple comparisons.
#'
#' @param x A numeric vector, matrix, or data frame containing the variables to
#'   be correlated.
#' @param y An optional second matrix or data frame. If provided, the variables
#'   in \code{x} are correlated with the variables in \code{y} rather than with
#'   each other.
#' @param test Set to \code{TRUE} to display p-values and sample sizes
#'   alongside the correlations. Defaults to \code{FALSE}.
#' @param corr.method The type of correlation to compute: \code{"pearson"}
#'   (the default), \code{"spearman"}, or \code{"kendall"}.
#' @param p.adjust.method The method used to correct p-values for multiple
#'   comparisons. Defaults to \code{"holm"}. All methods supported by
#'   \code{\link{p.adjust}} are accepted.
#'
#' @details Calculates a correlation matrix between all pairs of numeric
#' variables. If only \code{x} is supplied, all pairwise correlations among
#' the variables in \code{x} are computed. If both \code{x} and \code{y} are
#' supplied, variables in \code{x} are correlated with variables in \code{y}.
#'
#' Non-numeric variables (e.g., factors) are silently ignored: they appear in
#' the output with \code{NA} in place of correlation values. This makes it
#' convenient to pass an entire data frame without first removing categorical
#' columns.
#'
#' When \code{test = TRUE}, hypothesis tests are run for every pair of numeric
#' variables. To reduce the risk of false positives from testing many pairs at
#' once, p-values are adjusted using the Holm method by default. See
#' \code{\link{p.adjust}} for other available methods.
#'
#' Missing data are handled using pairwise complete cases, so sample sizes may
#' differ across pairs of variables.
#'
#' @return Prints the correlation matrix. If \code{test = TRUE}, also prints a
#' matrix of adjusted p-values and a matrix of sample sizes. The results are
#' also returned as a list with four elements: \code{correlation} (the
#' correlation matrix), \code{p.value} (the matrix of p-values),
#' \code{sample.size} (the matrix of sample sizes), and \code{args} (a record
#' of the options used). The list can be assigned to a variable and inspected
#' if needed.
#'
#' @seealso
#' \code{\link{cor}},
#' \code{\link{cor.test}},
#' \code{\link{p.adjust}}
#'
#' @export
#'
#' @examples
#' # data frame with factors and missing values
#' data <- data.frame(
#'   anxiety    = c(1.31, 2.72, 3.18, 4.21, 5.55, NA),
#'   stress     = c(2.01, 3.45, 1.99, 3.25, 4.27, 6.80),
#'   depression = c(2.51, 1.77, 3.34, 5.83, 9.01, 7.74),
#'   happiness  = c(4.02, 3.66, 5.23, 6.37, 7.83, 1.18),
#'   gender = factor(c("male", "female", "female", "male", "female", "female")),
#'   ssri   = factor(c("no", "no", "no", NA, "yes", "yes"))
#' )
#'
#' # Pearson correlation matrix (the default)
#' correlate(data)
#'
#' # Spearman correlations
#' correlate(data, corr.method = "spearman")
#'
#' # correlate two subsets of variables with each other
#' nervous <- data[, c("anxiety", "stress")]
#' happy   <- data[, c("happiness", "depression")]
#' correlate(nervous, happy)
#'
#' # include Holm-corrected p-values and sample sizes
#' correlate(data, test = TRUE)
#'
correlate <- function( x, y=NULL, test=FALSE, corr.method="pearson", p.adjust.method="holm" ) {

  # did the user specify two input matrices?
  two.inputs <- !is.null(y)

  # allow numeric vectors
  if( is.vector(x) & is.numeric(x) ) {
    x <- data.frame(x)
    call <- match.call()
    n <- call[[2]]
    names(x) <- "x.var"
    if(is.name(n)) names(x) <- as.character(n)
  }
  if( two.inputs & is.vector(y) & is.numeric(y) ) {
    y <- data.frame(y)
    call <- match.call()
    n <- call[[3]]
    names(y) <- "y.var"
    if(is.name(n)) names(y) <- as.character(n)
  }

  # check input matrices are data frames
  if( !inherits(x, c("matrix","data.frame")) ) {
    stop( 'x must be a matrix or data frame' )
  }
  if( two.inputs & !inherits(y, c("matrix","data.frame")) ) {
    stop( 'y must be a matrix or data frame' )
  }

  # warn if x and y appear to have the same variables!

  # coerce to data frame
  if( is.matrix(x) ) x <- as.data.frame(x)
  if( two.inputs & is.matrix(y) ) y <- as.data.frame(y)

  # store other args
  args <- c( two.inputs=two.inputs, test=test,
             corr.method=corr.method, p.adjust.method=p.adjust.method )

  # define a function to run correlation test and trap warning message
  getCT <- function( x,y, method ) {

    # get the correlation test, trapping the ties problem warning as needed...
    old.warn <- options(warn=2) # convert warnings to errors
    ct <- try( stats::cor.test( x, y, method=method), silent=TRUE ) # try the correlation
    tp <- FALSE # assume no ties problem unless...

    # check for failures:
    if( inherits(ct, "try-error") ) {

      # handle the case when the "error" was a ties warning
      tp <- length( grep("exact p-value with ties",ct )) > 0
      if( tp ) { # if it was a ties problem...
        options(warn=-1) # suppress warnings completely for the next run...
      } else { # if not...
        options( old.warn ) # reset warning state and let R throw what it likes...
      }

      # now run the test again...
      ct <- stats::cor.test( x, y, method=method )
    }
    options( old.warn ) # reset warnings to original state

    return( list( ct=ct, tp=tp) )

  }


  if( !two.inputs ) {
    #### one matrix case ###

    # drop categorical variables
    classes <- sapply(x,"class") # get the classes
    inds <- which( classes %in% c("integer","numeric")) # retain only int and num
    n.vars <- dim(x)[2] # number of variables
    n.cont <- length(inds) # number of continuous variables

    # initialise the output list with empty matrices
    R <- list( correlation = matrix( NA, n.vars, n.vars) )
    rownames( R$correlation ) <- colnames( R$correlation ) <- colnames(x)
    R$sample.size <- R$p.value <- R$correlation
    R$args <- args
    R$tiesProblem <- FALSE

    # run pairwise tests (inefficient looping!)
    for( a in seq_len(n.cont-1) ){
      for( b in (a+1):n.cont) {
        i <- inds[a] # the a-th continuous variable
        j <- inds[b] # the b-th continuous variable

       # ct <- cor.test( x[,i], x[,j], method=corr.method )
        cttp <- getCT( x[,i], x[,j], corr.method )

        # store the output
        R$tiesProblem <- R$tiesProblem | cttp$tp
        R$correlation[j,i] <- R$correlation[i,j] <- cttp$ct$estimate
        R$p.value[j,i] <- R$p.value[i,j] <- cttp$ct$p.value
      }
    }

    # adjust p (inefficient duplication here)
    upper.inds <- upper.tri(R$p.value) & !is.na(R$p.value) # cells containing p-values, upper
    R$p.value[upper.inds] <- stats::p.adjust( R$p.value[upper.inds], method=p.adjust.method )
    lower.inds <- lower.tri(R$p.value) & !is.na(R$p.value) # cells containing p-values, lower
    R$p.value[lower.inds] <- stats::p.adjust( R$p.value[lower.inds], method=p.adjust.method )


    # fill in sample sizes for individual variables
    for( i in 1:n.vars ) {
      R$sample.size[i,i] <- sum(!is.na(x[,i]))
    }

    # and pairwise sample sizes for all variables
    for( i in seq_len(n.vars-1) ) {
      for( j in (i+1):n.vars) {
        R$sample.size[j,i] <- R$sample.size[i,j] <- sum(!(is.na(x[,i]) | is.na(x[,j])))
      }
    }

  } else {
    #### two matrix case ###

    # track only the categorical variables
    inds.x <- which(sapply(x,"class") %in% c("integer","numeric"))
    inds.y <- which(sapply(y,"class") %in% c("integer","numeric"))

    # variable numbers
    n.vars.x <- dim(x)[2] # number of variables in x
    n.vars.y <- dim(y)[2] # number of variables in y
    n.cont.x <- length(inds.x) # number of continuous variables in x
    n.cont.y <- length(inds.y) # number of continuous variables in y

    # initialise the output list
    R <- list( correlation = matrix( NA, n.vars.x, n.vars.y) )
    rownames( R$correlation ) <- colnames(x)
    colnames( R$correlation ) <- colnames(y)
    R$sample.size <- R$p.value <- R$correlation
    R$args <- args
    R$tiesProblem <- FALSE

    # run pairwise tests (inefficient looping!)
    for( a in 1:n.cont.x ){
      for( b in 1:n.cont.y) {
        i <- inds.x[a] # the a-th continuous variable in x
        j <- inds.y[b] # the b-th continuous variable in y

        #ct <- cor.test( x[,i], y[,j], method=corr.method ) # run the test
        cttp <- getCT( x[,i], y[,j], corr.method )


        # store the output
        R$tiesProblem <- R$tiesProblem | cttp$tp
        R$correlation[i,j] <- cttp$ct$estimate
        R$p.value[i,j] <- cttp$ct$p.value

        # store sample size
        R$sample.size[i,j] <- sum(!(is.na(x[,i]) | is.na(y[,j])))
      }
    }

    # adjust p
    R$p.value <- matrix( stats::p.adjust( R$p.value, method=p.adjust.method ),
                         n.vars.x, n.vars.y,
                         dimnames=dimnames(R$p.value) )

  }

  # define an S3 class
  class(R) <- c("correlate","list")
  return(R)

}


# print method

#' Print correlation matrix results
#'
#' @description Prints the results of a correlation analysis in a readable
#' format. This function is called automatically whenever a result from
#' \code{\link{correlate}} is displayed.
#'
#' @param x A correlation result, as returned by \code{\link{correlate}}.
#' @param ... Additional arguments (unused, included for compatibility).
#'
#' @return Invisibly returns \code{x} unchanged.
#' @export
print.correlate <- function( x, ... ){

  # fixed properties that should probably be converted to
  # input arguments?
  nDigits <- 3
  naPrint <- "."

  # function to force equal digit printing
  makeTxt <- function(x, nDigits, naPrint ) {
    n <- dim(x)
    format <- paste0("%.",nDigits,"f")
    txt <- sprintf( format, x)
    txt <- gsub("NA", naPrint, txt, fixed=TRUE)
    txt <- matrix(txt,n[1],n[2], dimnames=dimnames(x))
    return(txt)
  }

  # function to print the text matrix
  printTxt <- function( txt ) {
    print.default( txt, quote=FALSE, right=TRUE)
  }

  # print the correlations
  cat("\n")
  cat("CORRELATIONS\n")
  cat("============\n")
  cat("- correlation type: ", x$arg["corr.method"], "\n")
  cat("- correlations shown only when both variables are numeric\n")
  cat("\n")

  if( options()$show.signif.stars & x$arg["test"]==TRUE ) { # if significance stars needed...

    # function to return the significance stars string
    getSigString <- function(p) {
      if( is.na(p) | p > .1 ) return( "   " )
      if( p > .05 ) return( ".  " )
      if( p > .01 ) return( "*  " )
      if( p > .001 ) return( "**" )
      return( "***" )
    }

    # function to generate interleaved indices
    interleave <- function(n){
      ord <- vector()
      ord[ seq(1,2*n-1,2) ] <- 1:n
      ord[ seq(2,2*n,2)] <- (n+1):(2*n)
      return(ord)
    }

    # print correlation matrix with sig stars
    n <- dim( x$correlation )[2] # number of columns
    txt <- makeTxt( x$correlation, nDigits, naPrint ) # text form of the correlation matrix
    for( i in 1:n ) txt[,i] <- paste0(txt[,i], sapply(x$p.value[,i], getSigString)) # append stars
    colnames(txt) <- paste0( colnames(x$correlation), rep.int("   ",n)) # column names
    rownames(txt) <- rownames( x$correlation ) # row names
    printTxt(txt)

    # print the key
    cat("\n---\n")
    cat("Signif. codes: . = p < .1, * = p<.05, ** = p<.01, *** = p<.001\n")

  } else { # if no significance stars needed...

    txt <- makeTxt( x$correlation, nDigits, naPrint )
    printTxt( txt )
  }

  if( x$arg["test"]==TRUE ) {

    # print the p.values
    cat("\n\np-VALUES\n")
    cat("========\n")
    if(  x$args["two.inputs"] ) { nTests <- sum( !is.na( x$correlation))
    } else { nTests <- sum( !is.na( x$correlation)) / 2 }
    cat("- total number of tests run: ", nTests, "\n")
    cat("- correction for multiple testing: ", x$arg["p.adjust.method"],"\n")
    if( x$tiesProblem ) {
      cat("- WARNING: cannot compute exact p-values with ties\n" )
    }
    cat("\n")

    # print p-values, forcing nDigits
    txt <- makeTxt( x$p.value, nDigits, naPrint )
    printTxt( txt )


    # print the sample sizes
    cat("\n\nSAMPLE SIZES\n")
    cat("============\n")
    cat("\n")
    txt <- makeTxt( x$sample.size, nDigits=0, naPrint )
    printTxt( txt )


  }

  # invisbly return the input
  return( invisible(x) )
}



