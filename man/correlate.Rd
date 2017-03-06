\name{correlate}
\alias{correlate}

\title{Correlation matrices}

\description{Computes a correlation matrix and runs hypothesis tests with corrections for multiple comparisons}

\usage{
correlate(x, y=NULL, test=FALSE, corr.method="pearson", p.adjust.method="holm") 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{Matrix or data frame containing variables to be correlated}
\item{y}{Optionally, a second set of variables to be correlated with those in \code{x}}
\item{test}{Should hypothesis tests be displayed? (Default=\code{FALSE})}
\item{corr.method}{What kind of correlations should be computed? Default is \code{"pearson"}, but \code{"spearman"} and \code{"kendall"} are also supported}
\item{p.adjust.method}{What method should be used to correct for multiple comparisons. Default value is \code{"holm"}, and the allowable values are the same as for \code{\link{p.adjust}}}
}

\details{The \code{correlate} function calculates a correlation matrix between all pairs of variables. Much like the \code{cor} function, if the user inputs only one set of variables (\code{x}) then it computes all pairwise correlations between the variables in \code{x}. If the user specifies both \code{x} and \code{y} it correlates the variables in \code{x} with the variables in \code{y}. 

Unlike the \code{cor} function, \code{correlate} does not generate an error if some of the variables are categorical (i.e., factors). Variables that are not numeric (or integer) class are simply ignored. They appear in the output, but no correlations are reported for those variables. The decision to have the \code{correlate} function allow the user a little leniency when the input contains non-numeric variables should be explained. The motivation is pedagogical rather than statistical. It is sometimes the case in psychology that students need to work with correlation matrices before they are comfortable subsetting a data frame, so it is convenient to allow them to type commands like \code{correlate(data)} even when \code{data} contains variables for which Pearson/Spearman correlations are not appropriate. (It is also useful to use the output of \code{correlate} to illustrate the fact that Pearson correlations should not be used for categorical variables).

A second difference between \code{cor} and \code{correlate} is that \code{correlate} runs hypothesis tests for all correlations in the correlation matrix (using the \code{cor.test} function to do the work). The results of the tests are only displayed to the user if \code{test=TRUE}. This is a pragmatic choice, given the (perhaps unfortunate) fact that psychologists often want to see the results of these tests: it is probably not coincidental that the \code{corr.test} function in the \pkg{psych} package already provides this functionality (though the output is difficult for novices to read). 

The concern with running hypothesis tests for all elements of a correlation matrix inflated Type I error rates. To minimise this risk, reported p-values are adjusted using the Holm method. The user can change this setting by specifying \code{p.adjust.method}. See \code{\link{p.adjust}} for details.

Missing data are handled using pairwise complete cases.}

\value{The printed output shows the correlation matrix, and if tests are requested it also reports a matrix of p-values and sample sizes associated with each correlation (these can vary if there are missing data). The underlying data structure is an object of class \code{correlate} (an S3 class). It is effectively a list containing four elements: \code{correlation} is the correlation matrix, \code{p.value} is the matrix of p-values, \code{sample.size} is the matrix of sample sizes, and \code{args} is a vector that stores information about what the user requested.}

%\references{
%% ~put references to the literature/web site here ~
%}


\section{Warning}{This package is under development, and has been released only due to teaching constraints. Until this notice disappears from the help files, you should assume that everything in the package is subject to change. Backwards compatibility is NOT guaranteed. Functions may be deleted in future versions and new syntax may be inconsistent with earlier versions. For the moment at least, this package should be treated with extreme caution.}

\author{
Daniel Navarro
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cor}}, \code{cor.test}, \code{p.adjust}, \code{corr.test} (in the \pkg{psych} package)
}


\examples{# data frame with factors and missing values
data <- data.frame( 
  anxiety = c(1.31,2.72,3.18,4.21,5.55,NA), 
  stress = c(2.01,3.45,1.99,3.25,4.27,6.80), 
  depression = c(2.51,1.77,3.34,5.83,9.01,7.74),
  happiness = c(4.02,3.66,5.23,6.37,7.83,1.18),
  gender = factor( c("male","female","female","male","female","female") ),
  ssri = factor( c("no","no","no",NA,"yes","yes") )
)

# default output is just the (Pearson) correlation matrix
correlate( data )

# other types of correlation:
correlate( data, corr.method="spearman" )

# two meaningful subsets to be correlated:
nervous <- data[,c("anxiety","stress")]
happy <- data[,c("happiness","depression","ssri")] 

# default output for two matrix input
correlate( nervous, happy )

# the same examples, with Holm-corrected p-values
correlate( data, test=TRUE ) 
correlate( nervous, happy, test=TRUE )
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
