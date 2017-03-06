\name{bars}
\alias{bars}

\title{Grouped Bar Plots with Error Bars}

\description{Grouped bar plots with error bars}

\usage{bars( formula, data=NULL, heightFun=mean, errorFun=ciMean, 
      yLabel=NULL, xLabels=NULL, main="", ylim=NULL,
      barFillColour=NULL, barLineWidth=2, barLineColour="black",
      barSpaceSmall=.2, barSpaceBig=1, legendLabels=NULL,
      legendDownShift=0, legendLeftShift=0, errorBarLineWidth=1,
      errorBarLineColour="grey40", errorBarWhiskerWidth=.2 ) }

%- maybe also 'usage' for other objects documented here.
\arguments{
\item{formula}{A two-sided formula specifying the response variable and the grouping factors}
\item{data}{An optional data frame containing the variables}
\item{heightFun}{The function used to calculate the bar height for a group (default=mean)}
\item{errorFun}{The function used to calculate the error bar for a group (default=ciMean). No bars drawn if \code{errorFun=FALSE}}
\item{yLabel}{The y-axis label (defaults to the name of the response variable)}
\item{xLabels}{The x-axis bar labels (defaults to factor labels of the appropriate grouping variable)}
\item{main}{The plot title} 
\item{ylim}{The y-axis limit: lower bound defaults to 0, default upper bound estimated}
\item{barFillColour}{The colours to fill the bars (defaults to a rainbow palette with saturation .3)}
\item{barLineWidth}{The width of the bar border lines (default=2)}
\item{barLineColour}{The colour of the bar border lines (default="black")}
\item{barSpaceSmall}{The size of the gap between bars within a cluster, as a proportion of bar width (default=.2)}
\item{barSpaceBig}{The size of the gap separating clusters of bars, as a proportion of bar width (default=1)}
\item{legendLabels}{The text for the legend (defaults to factor labels of the appropriate grouping variable). No legends drawn if \code{legendLabels=FALSE} or if only one grouping variable is specified}
\item{legendDownShift}{How far below the top is the legend, as proportion of plot height? (default=0)} 
\item{legendLeftShift}{How far away from the right edge is the legend, as proportion of plot? (default=0)}
\item{errorBarLineWidth}{The line width for the error bars (default=1)}
\item{errorBarLineColour}{The colour of the error bars (default="grey40")} 
\item{errorBarWhiskerWidth}{The width of error bar whiskers, as proportion of bar width (default=.2)}
}

\details{Plots group means (or other function, if specified) broken down by one or two grouping factors. Confidence intervals (or other function) are plotted. User specifies a two sided formula of the form \code{response ~ group1 + group2}, where \code{response} must be numeric and \code{group1} and \code{group2} are factors. The \code{group1} variable defines the primary separation on the x-axis, and the x-axis labels by default print out the levels of this factor. The \code{group2} variable defines the finer grain separation, and the legend labels correspond to the levels of this factor. Note that \code{group2} is optional.}

\value{Invisibly returns a data frame containing the factor levels, group means and confidence intervals.}

%\references{
%% ~put references to the literature/web site here ~
%}


\section{Warning}{This package is under development, and has been released only due to teaching constraints. Until this notice disappears from the help files, you should assume that everything in the package is subject to change. Backwards compatibility is NOT guaranteed. Functions may be deleted in future versions and new syntax may be inconsistent with earlier versions. For the moment at least, this package should be treated with extreme caution.}

\author{
Daniel Navarro
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{barplot}}, \code{bargraph.CI} (in the \pkg{sciplot} package)
}


\examples{# examples here...
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
