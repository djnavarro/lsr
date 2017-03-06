\name{modeOf}
\alias{modeOf}
\alias{maxFreq}

\title{
Sample mode
}

\description{
Calculate the mode of a sample: both modal value(s) and the corresponding frequency
}

\usage{
modeOf(x, na.rm = TRUE)
maxFreq(x, na.rm = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A vector containing the observations.}
  \item{na.rm}{ Logical value indicating whether NA values should be removed. }
}

\details{
These two functions can be used to calculate the mode (most frequently observed value) of a sample, and the actual frequency of the modal value. The only complication is in respect to missing data. If \code{na.rm = FALSE}, then there are multiple possibilities for how to calculate the mode. One possibility is to treat \code{NA} as another possible value for the elements of \code{x}, and therefore if \code{NA} is more frequent than any other value, then \code{NA} is the mode; and the modal frequency is equal to the number of missing values. This is the version that is currently implemented. 

Another possibility is to treat \code{NA} as meaning "true value unknown", and to the mode of \code{x} is itself known only if the number of missing values is small enough that -- regardless of what value they have -- they cannot alter the sample mode. For instance, if \code{x} were \code{c(1,1,1,1,2,2,NA)}, we know that the mode of \code{x} is \code{1} regardless of what the true value is for the one missing datum; and we know that the modal frequency is between 4 and 5. This is also a valid interpretation, depending on what precisely it is the user wants, but is not currently implemented. 

Because of the ambiguity of how \code{na.rm = FALSE} should be interpreted, the default value has been set to \code{na.rm = TRUE}, which differs from the default value that I've used elsewhere in the package. 
}

\value{
The \code{modeOf} function returns the mode of \code{x}. If there are ties, it returns a vector containing all values of \code{x} that have the modal frequency. 
The \code{maxFreq} function returns the modal frequency as a numeric value.
}

\section{Warning}{This package is under development, and has been released only due to teaching constraints. Until this notice disappears from the help files, you should assume that everything in the package is subject to change. Backwards compatibility is NOT guaranteed. Functions may be deleted in future versions and new syntax may be inconsistent with earlier versions. For the moment at least, this package should be treated with extreme caution.}

%\references{
%% ~put references to the literature/web site here ~
%}

\author{
Daniel Navarro
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{mean}}, \code{\link{median}}, \code{\link{table}}
}


\examples{
# simple example
eyes <- c("green","green","brown","brown","blue")
modeOf(eyes)
maxFreq(eyes)

# vector with missing data
eyes <- c("green","green","brown","brown","blue",NA,NA,NA)

# returns NA as the modal value.
modeOf(eyes, na.rm = FALSE) 
maxFreq(eyes, na.rm = FALSE)

# returns c("green", "brown") as the modes, as before
modeOf(eyes, na.rm = TRUE) 
maxFreq(eyes, na.rm = TRUE)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
