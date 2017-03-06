\name{associationTest}
\alias{associationTest}


\title{
Chi-square test of association / independence
}

\description{
Convenience function that runs a chi-square test of association/independence. This is a wrapper function intended to be used for pedagogical purposes only.
}

\usage{
associationTest( formula, data=NULL )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{One-sided formula specifying the two variables (required).}
  \item{data}{Optional data frame containing the variables.}
}

\details{
The \code{associationTest} function runs the chi-square test of association on the variables specified in the \code{formula} argument. The formula must be a one-sided formula of the form \code{~variable1 + variable2}, and both variables must be factors. 
}

\value{An object of class 'assocTest'. When printed, the output is organised into six short sections. The first section lists the name of the test and the variables included. The second lists the null and alternative hypotheses for the test. The third shows the observed contingency table, and the fourth shows the expected contingency table under the null. The fifth prints out the test results, and the sixth reports an estimate of effect size.}

%\references{
%% ~put references to the literature/web site here ~
%}

\section{Warning}{This package is under development, and has been released only due to teaching constraints. Until this notice disappears from the help files, you should assume that everything in the package is subject to change. Backwards compatibility is NOT guaranteed. Functions may be deleted in future versions and new syntax may be inconsistent with earlier versions. For the moment at least, this package should be treated with extreme caution.}

\author{
Daniel Navarro
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{chisq.test}}, \code{\link{goodnessOfFitTest}}, \code{\link{cramersV}}
}


\examples{

df <- data.frame(     
 gender=factor(c("male","male","male","male","female","female","female")),
 answer=factor(c("heads","heads","heads","heads","tails","tails","heads"))
)

associationTest( ~ gender + answer, df )

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
