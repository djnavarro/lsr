# etaSquared() calculates eta-squared and partial eta-squared for linear models
# (usually ANOVAs). It takes an lm object as input and computes the effect size
# for all terms in the model. By default uses Type II sums of squares to calculate
# the effect size, but Types I and III are also possible. By default the output
# only displays the effect size, but if requested it will also print out the full
# ANOVA table.


#' Effect size for ANOVAs
#'
#' @description Calculates eta-squared and partial eta-squared effect sizes for
#' an analysis of variance.
#'
#' @param x An \code{aov} object, as returned by \code{\link{aov}}.
#' @param type Which type of sums of squares to use: \code{1} for Type I,
#'   \code{2} for Type II (the default), or \code{3} for Type III. Type II is
#'   recommended for most unbalanced designs.
#' @param anova Set to \code{TRUE} to include the full ANOVA table alongside
#'   the effect sizes. Defaults to \code{FALSE}.
#'
#' @details Calculates eta-squared and partial eta-squared, two commonly used
#' measures of effect size in analysis of variance. The input \code{x} should
#' be an ANOVA fitted with \code{\link{aov}}.
#'
#' For unbalanced designs, Type II sums of squares (\code{type = 2}) are
#' recommended and are the default, consistent with the \code{Anova} function
#' in the \pkg{car} package. Type I (\code{type = 1}) matches the output of
#' \code{\link{anova}} but tests hypotheses that are often not of interest in
#' unbalanced designs. Type III (\code{type = 3}) is also available.
#'
#' @return A matrix with one row per term in the ANOVA model and columns for
#' eta-squared (\code{eta.sq}) and partial eta-squared (\code{eta.sq.part}).
#' If \code{anova = TRUE}, additional columns show the sums of squares,
#' mean squares, degrees of freedom, F-statistics, and p-values.
#'
#' @seealso
#' \code{\link{aov}},
#' \code{\link{summary.aov}}
#'
#' @export
#'
#' @examples
#' outcome <- c(1.4, 2.1, 3.0, 2.1, 3.2, 4.7, 3.5, 4.5, 5.4)
#' treatment1 <- factor(c(1, 1, 1, 2, 2, 2, 3, 3, 3))
#'
#' # one-way ANOVA
#' anova1 <- aov(outcome ~ treatment1)
#' summary(anova1)
#' etaSquared(anova1)
#'
#' # include the full ANOVA table
#' etaSquared(anova1, anova = TRUE)
#'
#' # two-way ANOVA
#' treatment2 <- factor(c(1, 2, 3, 1, 2, 3, 1, 2, 3))
#' anova2 <- aov(outcome ~ treatment1 + treatment2)
#' etaSquared(anova2)
#'
etaSquared <- function(x, type = 2, anova = FALSE) {
  if (!methods::is(anova, "logical") || length(anova) != 1 || is.na(anova)) {
    stop('"anova" must be a single logical value')
  }
  if (!methods::is(x, "lm")) {
    stop('"x" must be a linear model object')
  }
  if (!methods::is(type, "numeric") | length(type) != 1) {
    stop("type must be equal to 1,2 or 3")
  }

  if (type == 1) {
    ss <- anova(x)[, "Sum Sq", drop = FALSE] # Type 1 SS
    ss.res <- ss[dim(ss)[1], ] # Full model RSS
    ss.tot <- sum(ss) # Total SS
    ss <- ss[-dim(ss)[1], , drop = FALSE]
    ss <- as.matrix(ss) # later code assumes ss is a matrix
  } else {
    if (type == 2) {
      # get the residual and total sum of squares
      ss.tot <- sum((x$model[, 1] - mean(x$model[, 1]))^2)
      ss.res <- sum((x$residuals)^2)

      # get information about how terms depend on variables (1st row is the DV, so drop it)
      terms <- attr(x$terms, "factors")[-1, , drop = FALSE]

      # initialise the ss matrix
      l <- attr(x$terms, "term.labels")
      ss <- matrix(NA, length(l), 1)
      rownames(ss) <- l

      # compute ss values
      for (i in seq_along(ss)) {
        # what variables does this term depend on?
        vars.this.term <- which(terms[, i] != 0)

        # which terms are dependent on this term?
        dependent.terms <- which(apply(terms[vars.this.term, , drop = FALSE], 2, prod) > 0)

        # null model removes all of the dependent terms
        m0 <- stats::lm(x$terms[-dependent.terms], x$model) # remove all of these

        # terms with higher order terms need a separate alternative model...
        if (length(dependent.terms) > 1) {
          m1 <- stats::lm(x$terms[-setdiff(dependent.terms, i)], x$model) # remove all except i-th term
          ss[i] <- anova(m0, m1)$`Sum of Sq`[2] # get the ss value

          # terms without higher order dependent terms can be directly compared to the full model...
        } else {
          ss[i] <- anova(m0, x)$`Sum of Sq`[2]
        }
      }
    } else {
      if (type == 3) {
        mod <- stats::drop1(x, scope = x$terms)
        ss <- mod[-1, "Sum of Sq", drop = FALSE] # Type 3 SS
        ss.res <- mod[1, "RSS"] # residual SS
        ss.tot <- sum((x$model[, 1] - mean(x$model[, 1]))^2)
        ss <- as.matrix(ss) # later code assumes ss is a matrix
      } else {
        stop("type must be equal to 1,2 or 3")
      }
    }
  }

  # output matrix if anova not requested...
  if (anova == FALSE) {
    eta2 <- ss / ss.tot
    eta2p <- ss / (ss + ss.res)
    E <- cbind(eta2, eta2p)
    rownames(E) <- rownames(ss)
    colnames(E) <- c("eta.sq", "eta.sq.part")

    # output matrix if anova is requested...
  } else {
    ss <- rbind(ss, ss.res)
    eta2 <- ss / ss.tot
    eta2p <- ss / (ss + ss.res)
    k <- length(ss)
    eta2p[k] <- NA
    df <- anova(x)[, "Df"] # lazy!!!
    ms <- ss / df
    Fval <- ms / ms[k]
    p <- 1 - stats::pf(Fval, df, rep.int(df[k], k))
    E <- cbind(eta2, eta2p, ss, df, ms, Fval, p)
    E[k, 6:7] <- NA
    colnames(E) <- c("eta.sq", "eta.sq.part", "SS", "df", "MS", "F", "p")
    rownames(E) <- rownames(ss)
    rownames(E)[k] <- "Residuals"
  }
  return(E)
}
