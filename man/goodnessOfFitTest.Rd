\name{goodnessOfFitTest}
\alias{goodnessOfFitTest}


\title{
Chi-square test against specified probabilities
}

\description{
Convenience function that runs a chi-square goodness of fit test against specified probabilities. This is a wrapper function intended to be used for pedagogical purposes only.
}

\usage{
goodnessOfFitTest( x, p=NULL )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Factor variable containing the raw outcomes.}
  \item{p}{Numeric variable containing the null-hypothesis probabilities (default = all outcomes equally likely)}
}

\details{
The \code{goodnessOfFitTest} function runs the chi-square goodness of fit test of the hypothesis that the outcomes in the factor \code{x} were generated according to the probabilities in the vector \code{p}. The probability vector \code{p} must be a numeric variable of length \code{nlevels(x)}. If no probabilities are specified, all outcomes are assumed to be equally likely.
}

\value{An object of class 'gofTest'. When printed, the output is organised into four short sections. The first section lists the name of the test and the variables included. The second lists the null and alternative hypotheses for the test. The third shows the observed frequency table, the expected frequency table under the null hypothesis, and the probabilities specified by the null. The fourth prints out the test results.}

%\references{
%% ~put references to the literature/web site here ~
%}

\section{Warning}{This package is under development, and has been released only due to teaching constraints. Until this notice disappears from the help files, you should assume that everything in the package is subject to change. Backwards compatibility is NOT guaranteed. Functions may be deleted in future versions and new syntax may be inconsistent with earlier versions. For the moment at least, this package should be treated with extreme caution.}

\author{
Daniel Navarro
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{chisq.test}}, \code{\link{associationTest}}, \code{\link{cramersV}}
}


\examples{
# raw data
gender <- factor( 
	c( "male","male","male","male","female","female",
       "female","male","male","male" ))

# goodness of fit test against the hypothesis that males and
# females occur with equal frequency
goodnessOfFitTest( gender )

# goodness of fit test against the hypothesis that males appear
# with probability .6 and females with probability .4. 
goodnessOfFitTest( gender, p=c(.4,.6) )
goodnessOfFitTest( gender, p=c(female=.4,male=.6) )
goodnessOfFitTest( gender, p=c(male=.6,female=.4) )
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
