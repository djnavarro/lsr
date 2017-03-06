\name{cramersV}
\alias{cramersV}

\title{Cramer's V}

\description{Calculate the Cramer's V measure of association
}

\usage{
cramersV(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{Arguments to be passed to the \code{chisq.test} function.}
}

\details{
Calculates the Cramer's V measure of effect size for chi-square tests of association and goodness of fit. The arguments to the \code{cramersV} function are all passed straight to the \code{chisq.test} function, and should have the same format.}

\value{A numeric variable with a single element corresponding to the value of V.}

%\references{
%% ~put references to the literature/web site here ~
%}

\section{Warning}{This package is under development, and has been released only due to teaching constraints. Until this notice disappears from the help files, you should assume that everything in the package is subject to change. Backwards compatibility is NOT guaranteed. Functions may be deleted in future versions and new syntax may be inconsistent with earlier versions. For the moment at least, this package should be treated with extreme caution.}

\author{
Daniel Navarro
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{chisq.test}}, \code{assocstats} (in the \pkg{vcd} package)
}


\examples{# Consider an experiment with two conditions, each with 100 
# participants. Each participant chooses between one of three
# options. Possible data for this experiment:

condition1 <- c(30, 20, 50) 
condition2 <- c(35, 30, 35)
X <- cbind( condition1, condition2 )
rownames(X) <- c( 'choice1', 'choice2', 'choice3' )
print(X)

# To test the null hypothesis that the distribution of choices
# is identical in the two conditions, we would run a chi-square
# test:
chisq.test(X)

# To estimate the effect size we can use Cramer's V:
cramersV( X )  # returns a value of 0.159


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
