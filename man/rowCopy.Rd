\name{rowCopy}
\alias{rowCopy}
\alias{colCopy}

\title{Copy a vector into a matrix}

\description{
Creates a matrix by binding multiple copies of a vector together, either by row or by column
}

\usage{
	colCopy(x,times, dimnames=NULL )
	rowCopy(x,times, dimnames=NULL )
}

\arguments{
  \item{x}{The vector to be copied}
  \item{times}{Number of copies of the vector to bind together}
  \item{dimnames}{List specifying row and column names}
}

\details{
This is a convenience function for binding together multiple copies of the same vector. The intended usage is for situations where one might ordinarily use \code{rbind} or \code{cbind}, but the work is done by the \code{matrix} function. Instead of needing to input multiple copies of the input vector \code{x} (as one would for \code{rbind}), one only needs to specify the number of \code{times} that the vector should be copied.
}

\value{
For \code{rowCopy}, the output is a matrix with \code{times} rows and \code{length(x)} columns, in which each row contains the vector \code{x}. For \code{colCopy}, each column corresponds to the vector \code{x}.
}



%\references{
%% ~put references to the literature/web site here ~
%}

\author{Daniel Navarro}

\section{Warning}{This package is under development, and has been released only due to teaching constraints. Until this notice disappears from the help files, you should assume that everything in the package is subject to change. Backwards compatibility is NOT guaranteed. Functions may be deleted in future versions and new syntax may be inconsistent with earlier versions. For the moment at least, this package should be treated with extreme caution.}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{matrix}}, \code{\link{rbind}}, \code{\link{replicate}}
}

\examples{

#### Example 1: basic usage 
data <- c(3,1,4,1,5)
rowCopy( data, 4 ) 
colCopy( data, 4 )

#### Example 2: attach dimension names
dnames <- list( rows = c("r1","r2","r3"), cols = c("c1","c2","c3","c4","c5") )
rowCopy( data,3,dnames )



}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
