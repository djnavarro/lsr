\name{longToWide}
\alias{longToWide}


\title{
Reshape from long to wide
}

\description{
Reshape a data frame from long form to wide form
}

\usage{
longToWide( data, formula, sep="_")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{The data frame.}
  \item{formula}{A two-sided formula specifying measure variables and within-subject variables }
  \item{sep}{Separator string used in wide-form variable names}
}

\details{
The \code{longToWide} function is the companion function to \code{wideToLong}. The \code{data} argument is a "long form" data frame, in which each row corresponds to a single observation. The output is a "wide form" data frame, in which each row corresponds to a single experimental unit (e.g., a single subject). 

The reshaping formula should list all of the measure variables on the left hand side, and all of the within-subject variables on the right hand side. All other variables are assumed to be between-subject variables. For example, if the \code{accuracy} of a participant's performance is measured at multiple \code{time} points, then the formula would be \code{accuracy ~ time}. 

Multiple variables are supported on both sides of the formula. For example, suppose we measured the response time \code{rt} and \code{accuracy} of participants, across three separate \code{days}, and across three separate \code{sessions} within each day. In this case the formula would be \code{rt + accuracy ~ days + sessions}. 
}

\value{The output is a "wide form" data frame in containing one row per subject (or experimental unit, more generally), with each observation of that subject corresponding to a separate variable. The naming scheme for these variables places the name of the measured variable first, followed by the levels of within-subjects variable(s), separated by the separator string \code{sep}. In the example above where the reshaping formula was \code{accuracy ~ time}, if the default separator of \code{sep="_"} was used, and the levels of the \code{time} variable are \code{t1}, \code{t2} and \code{t3}, then the output would include the variables \code{accuracy_t1}, \code{accuracy_t2} and \code{accuracy_t3}. 

In the second example listed above, where the reshaping formula was \code{rt + accuracy ~ days + sessions}, the output variables would refer to levels of both within-subjects variables. For instance, \code{rt_day1_session1}, and \code{accuracy_day2_session1} might be the names of two of the variables in the wide form data frame.
}


%\references{
%% ~put references to the literature/web site here ~
%}

\section{Warning}{This package is under development, and has been released only due to teaching constraints. Until this notice disappears from the help files, you should assume that everything in the package is subject to change. Backwards compatibility is NOT guaranteed. Functions may be deleted in future versions and new syntax may be inconsistent with earlier versions. For the moment at least, this package should be treated with extreme caution.}

\author{
Daniel Navarro
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{wideToLong}}, \code{\link{reshape}}, \code{\link{stack}}, \code{cast} and \code{melt} (in the \pkg{reshape} package)
}


\examples{
long <- data.frame( id = c(1,2,3,1,2,3,1,2,3), 
                    time = c("t1","t1","t1","t2","t2","t2","t3","t3","t3"), 
                    accuracy = c(.50,.03,.72,.94,.63,.49,.78,.71,.16) )

longToWide( long, accuracy~time)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
