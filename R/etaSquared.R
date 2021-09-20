

# etaSquared() calculates eta-squared and partial eta-squared for linear models
# (usually ANOVAs). It takes an lm object as input and computes the effect size
# for all terms in the model. By default uses Type II sums of squares to calculate
# the effect size, but Types I and III are also possible. By default the output
# only displays the effect size, but if requested it will also print out the full
# ANOVA table.


#' Effect size calculations for ANOVAs
#'
#' @description Calculates eta-squared and partial eta-squared
#'
#' @param x An analysis of variance (aov) object.
#' @param type What type of sum of squares to calculate?
#' @param anova Should the full ANOVA table be printed out in addition to the effect sizes?
#'
#' @details Calculates the eta-squared and partial eta-squared measures of
#' effect size that are commonly used in analysis of variance. The input
#' \code{x} should be the analysis of variance object itself.
#'
#' For unbalanced designs, the default in \code{etaSquared} is to compute
#' Type II sums of squares (\code{type=2}), in keeping with the \code{Anova}
#' function in the \code{car} package. It is possible to revert to the
#' Type I SS values (\code{type=1}) to be consistent with \code{anova}, but
#' this rarely tests hypotheses of interest. Type III SS values (\code{type=3})
#' can also be computed.
#'
#' @return If \code{anova=FALSE}, the output is an M x 2 matrix. Each of the
#' M rows corresponds to one of the terms in the ANOVA (e.g., main effect 1,
#' main effect 2, interaction, etc), and each of the columns corresponds to
#' a different measure of effect size. Column 1 contains the eta-squared
#' values, and column 2 contains partial eta-squared values. If
#' \code{anova=TRUE}, the output contains additional columns containing the
#' sums of squares, mean squares, degrees of freedom, F-statistics and p-values.
#'
#' @export
#'
#' @examples
#' # Example 1: one-way ANOVA
#'
#' outcome <- c( 1.4,2.1,3.0,2.1,3.2,4.7,3.5,4.5,5.4 )  # data
#' treatment1 <- factor( c( 1,1,1,2,2,2,3,3,3 ))        # grouping variable
#' anova1 <- aov( outcome ~ treatment1 )                # run the ANOVA
#' summary( anova1 )                                    # print the ANOVA table
#' etaSquared( anova1 )                                 # effect size
#'
#' # Example 2: two-way ANOVA
#'
#' treatment2 <- factor( c( 1,2,3,1,2,3,1,2,3 ))      # second grouping variable
#' anova2 <- aov( outcome ~ treatment1 + treatment2 ) # run the ANOVA
#' summary( anova2 )                                  # print the ANOVA table
#' etaSquared( anova2 )                               # effect size
#'
etaSquared<- function( x, type = 2, anova = FALSE ) {

  if( !methods::is(anova,"logical") | length(anova) !=1 ) {
    stop( '"anova" must be a single logical value')
  }
  if( !methods::is(x,"lm") ) {stop( '"x" must be a linear model object')}
  if( !methods::is(type,"numeric") | length(type) !=1 ) {
    stop("type must be equal to 1,2 or 3")
  }

  if( type == 1) {

    ss <- anova(x)[,"Sum Sq",drop=FALSE]  # Type 1 SS
    ss.res <- ss[dim(ss)[1],]  # Full model RSS
    ss.tot <- sum( ss )  # Total SS
    ss <- ss[-dim(ss)[1],,drop=FALSE]
    ss <- as.matrix(ss) # later code assumes ss is a matrix

  } else { if (type == 2) {

    # get the residual and total sum of squares
    ss.tot <- sum(( x$model[,1] - mean(x$model[,1]) )^2)
    ss.res <- sum(( x$residuals)^2)

    # get information about how terms depend on variables (1st row is the DV, so drop it)
    terms <- attr(x$terms,"factors")[-1,,drop=FALSE]

    # initialise the ss matrix
    l <- attr(x$terms,"term.labels")
    ss <- matrix(NA,length(l),1)
    rownames(ss) <- l

    # compute ss values
    for( i in seq_along(ss) ) {

      # what variables does this term depend on?
      vars.this.term <- which( terms[,i] != 0 )

      # which terms are dependent on this term?
      dependent.terms <- which( apply( terms[ vars.this.term,,drop=FALSE], 2, prod )>0 )

      # null model removes all of the dependent terms
      m0 <- stats::lm( x$terms[-dependent.terms], x$model )  # remove all of these

      # terms with higher order terms need a separate alternative model...
      if( length(dependent.terms)>1 ) {
        m1 <- stats::lm( x$terms[-setdiff(dependent.terms,i)], x$model ) # remove all except i-th term
        ss[i] <- anova(m0,m1)$`Sum of Sq`[2] # get the ss value

        # terms without higher order dependent terms can be directly compared to the full model...
      } else {
        ss[i] <- anova(m0,x)$`Sum of Sq`[2]
      }
    }


  } else { if (type == 3) {

    mod <- stats::drop1(x,scope=x$terms)
    ss <- mod[-1,"Sum of Sq",drop=FALSE] # Type 3 SS
    ss.res <- mod[1,"RSS"] # residual SS
    ss.tot <- sum(( x$model[,1] - mean(x$model[,1]) )^2)
    ss <- as.matrix(ss) # later code assumes ss is a matrix

  } else {
    stop("type must be equal to 1,2 or 3")
  }}}

  # output matrix if anova not requested...
  if( anova == FALSE) {
    eta2 <- ss / ss.tot
    eta2p <- ss / (ss + ss.res)
    E <- cbind(eta2, eta2p)
    rownames(E) <- rownames(ss)
    colnames(E) <- c("eta.sq","eta.sq.part")

    # output matrix if anova is requested...
  } else {
    ss <- rbind( ss, ss.res )
    eta2 <- ss / ss.tot
    eta2p <- ss / (ss + ss.res)
    k <- length(ss)
    eta2p[k] <- NA
    df <- anova(x)[,"Df"] # lazy!!!
    ms <- ss/df
    Fval <- ms / ms[k]
    p <- 1-stats::pf( Fval, df, rep.int(df[k],k) )
    E <- cbind(eta2, eta2p, ss, df, ms, Fval, p)
    E[k,6:7] <- NA
    colnames(E) <- c("eta.sq","eta.sq.part","SS","df","MS","F","p")
    rownames(E) <- rownames(ss)
    rownames(E)[k] <- "Residuals"
  }
  return(E)

}
