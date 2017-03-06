\name{rmAll}

\alias{rmAll}

\title{Remove all objects}

\description{Removes all objects from the workspace}

\usage{
rmAll(ask = TRUE)
}


\arguments{
  \item{ask}{Logical value indicating whether to ask user to confirm deletions. Default is \code{TRUE}}
}

\details{The \code{rmAll} function provides a simple way of deleting all objects from the workspace. It is almost equivalent to the usual \code{rm(list = objects())} command. The only difference that it requires the user to confirm the deletions first if \code{ask = TRUE}, after displaying a list of the current objects in the worspace. I find it personally convenient primarily because it involves fewer keystrokes, but more generally I find it useful for teaching purposes, since I usually want to show people how to clear the workspace before I have discussed named arguments to functions.}

\value{Invisibly returns 0 if no deletions are made, 1 if at least one deletion is made.}

%\references{}

\section{Warning}{This package is under development, and has been released only due to teaching constraints. Until this notice disappears from the help files, you should assume that everything in the package is subject to change. Backwards compatibility is NOT guaranteed. Functions may be deleted in future versions and new syntax may be inconsistent with earlier versions. For the moment at least, this package should be treated with extreme caution.}

\author{Daniel Navarro}

%\note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{rm}} }

\examples{
# equivalent to rm(list = objects())
rmAll(ask = FALSE) 
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
