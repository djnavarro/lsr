\name{independentSamplesTTest}
\alias{independentSamplesTTest}


\title{
Independent samples t-test
}

\description{
Convenience function that runs an independent samples t-test. This is a wrapper function intended to be used for pedagogical purposes only.
}

\usage{
independentSamplesTTest( 
	formula, data=NULL, var.equal=FALSE, 
	one.sided=FALSE, conf.level=.95 
) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{Formula specifying the outcome and the groups (required).}
  \item{data}{Optional data frame containing the variables.}
  \item{var.equal}{Should the test assume equal variances (default = \code{FALSE}).}
  \item{one.sided}{One sided or two sided hypothesis test (default = \code{FALSE})}
  \item{conf.level}{The confidence level for the confidence interval (default = .95).}
}

\details{
The \code{independentSamplesTTest} function runs an independent-samples t-test and prints the results in a format that is easier for novices to handle than the output of \code{t.test}. All the actual calculations are done by the \code{t.test} and \code{cohensD} functions. The \code{formula} argument must be a two-sided formula of the form \code{outcome ~ group}. When \code{var.equal=TRUE}, a Student's t-test is run and the estimate of Cohen's d uses a pooled estimate of standard deviation. When \code{var.equal=FALSE}, the Welch test is used, and the estimate of Cohen's d uses the "unequal" method.

As with the \code{t.test} function, the default test is two sided, corresponding to a default value of \code{one.sided = FALSE}. To specify a one sided test, the \code{one.sided} argument must specify the name of the factor level that is hypothesised (under the alternative) to have the larger mean. For instance, if the outcome for "group2" is expected to be higher than for "group1", then the corresponding one sided test is specified by \code{one.sided = "group2"}.    
}

\value{An object of class 'TTest'. When printed, the output is organised into five short sections. The first section lists the name of the test and the variables included. The second provides means and standard deviations. The third states explicitly what the null and alternative hypotheses were. The fourth contains the test results: t-statistic, degrees of freedom and p-value. The final section includes the relevant confidence interval and an estimate of the effect size (i.e., Cohen's d)}

%\references{
%% ~put references to the literature/web site here ~
%}

\section{Warning}{This package is under development, and has been released only due to teaching constraints. Until this notice disappears from the help files, you should assume that everything in the package is subject to change. Backwards compatibility is NOT guaranteed. Functions may be deleted in future versions and new syntax may be inconsistent with earlier versions. For the moment at least, this package should be treated with extreme caution.}

\author{
Daniel Navarro
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{t.test}}, \code{\link{oneSampleTTest}}, \code{\link{pairedSamplesTTest}}, \code{\link{cohensD}}
}


\examples{

df <- data.frame( 
	rt = c(451, 562, 704, 324, 505, 600, 829), 
	cond = factor( x=c(1,1,1,2,2,2,2), labels=c("group1","group2")))

# Welch t-test
independentSamplesTTest( rt ~ cond, df )

# Student t-test
independentSamplesTTest( rt ~ cond, df, var.equal=TRUE )

# one sided test
independentSamplesTTest( rt ~ cond, df, one.sided="group1" )

# missing data
df$rt[1] <- NA
df$cond[7] <- NA
independentSamplesTTest( rt ~ cond, df )

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
