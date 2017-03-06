\name{oneSampleTTest}
\alias{oneSampleTTest}


\title{
One sample t-test
}

\description{
Convenience function that runs a one sample t-test. This is a wrapper function intended to be used for pedagogical purposes only.
}

\usage{
oneSampleTTest( 
	x, mu, one.sided=FALSE, conf.level=.95 
) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The variable to be tested (required).}
  \item{mu}{The value against which the mean should be tested (required).}
  \item{one.sided}{One sided or two sided hypothesis test (default = \code{FALSE})}
  \item{conf.level}{The confidence level for the confidence interval (default = .95).}
}

\details{
The \code{oneSampleTTest} function runs a one-sample t-test on the data in \code{x}, and prints the results in a format that is easier for novices to handle than the output of \code{t.test}. All the actual calculations are done by the \code{t.test} and \code{cohensD} functions. 

As with the \code{t.test} function, the default test is two sided, corresponding to a default value of \code{one.sided = FALSE}. To specify a one sided test in which the alternative hypothesis is that \code{x} is larger than \code{mu}, the input must be \code{one.sided = "greater"}. Similarly, if \code{one.sided="less"}, then the alternative hypothesis is that the mean of \code{x} is smaller than \code{mu}.
}

\value{An object of class 'TTest'. When printed, the output is organised into five short sections. The first section lists the name of the test and the variables included. The second provides means and standard deviations. The third states explicitly what the null and alternative hypotheses were. The fourth contains the test results: t-statistic, degrees of freedom and p-value. The final section includes the relevant confidence interval and an estimate of the effect size (i.e., Cohen's d).}

%\references{
%% ~put references to the literature/web site here ~
%}

\section{Warning}{This package is under development, and has been released only due to teaching constraints. Until this notice disappears from the help files, you should assume that everything in the package is subject to change. Backwards compatibility is NOT guaranteed. Functions may be deleted in future versions and new syntax may be inconsistent with earlier versions. For the moment at least, this package should be treated with extreme caution.}

\author{
Daniel Navarro
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{t.test}}, \code{\link{pairedSamplesTTest}}, \code{\link{independentSamplesTTest}}, \code{\link{cohensD}}
}


\examples{
likert <- c(3,1,4,1,4,6,7,2,6,6,7)

oneSampleTTest( x = likert, mu = 4 ) 
oneSampleTTest( x = likert, mu = 4, one.sided = "greater" ) 
oneSampleTTest( x = likert, mu = 4, conf.level=.99 ) 

likert <- c(3,NA,4,NA,4,6,7,NA,6,6,7)
oneSampleTTest( x = likert, mu = 4 ) 

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
