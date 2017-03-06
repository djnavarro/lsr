\name{ciMean}
\alias{ciMean}

\title{
Confidence interval around the mean
}

\description{
Calculates confidence intervals for the mean of a normally-distributed variable.
}

\usage{
ciMean(x, conf = 0.95, na.rm = FALSE)
}
\arguments{
  \item{x}{A numeric vector, data frame or matrix containing the observations.}
  \item{conf}{The level of confidence desired. Defaults to a 95\% confidence interval}
  \item{na.rm}{Logical value indicating whether missing values are to be removed. Defaults to \code{FALSE}.}
}

\details{
This function calculates the confidence interval for the mean of a variable (or set of variables in a data frame or matrix), under the standard assumption that the data are normally distributed. By default it returns a 95\% confidence interval (\code{conf = 0.95}) and does not remove missing values (\code{na.rm = FALSE}).  
}

\value{
The output is a matrix containing the lower and upper ends of the confidence interval for each variable. If a data frame is specified as input and contains non-numeric variables, the corresponding rows in the output matrix have NA values. 
}

%\references{
%% ~put references to the literature/web site here ~
%}

\author{
Daniel Navarro
}


\section{Warning}{This package is under development, and has been released only due to teaching constraints. Until this notice disappears from the help files, you should assume that everything in the package is subject to change. Backwards compatibility is NOT guaranteed. Functions may be deleted in future versions and new syntax may be inconsistent with earlier versions. For the moment at least, this package should be treated with extreme caution.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{confint}}
}


\examples{
X <- c(1, 3, 6)          # data 
ciMean(X)                # 95 percent confidence interval
ciMean(X, conf = .8)     # 80 percent confidence interval

confint( lm(X ~ 1) )     # for comparison purposes

X <- c(1, 3, NA, 6)      # data with missing values
ciMean(X, na.rm = TRUE)  # remove missing values

}

