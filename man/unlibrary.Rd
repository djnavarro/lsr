\name{unlibrary}
\alias{unlibrary}

\title{
Unload a package
}

\description{
A wrapper function to \code{\link{detach}} that removes a package from the search path, but takes a package name as input similar to \code{\link{library}}.
}

\usage{
unlibrary(package)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{package}{
A package name, which may be specified with or without quotes. 
}
}

\details{
Unloads a package. This is just a wrapper for the \code{detach} function. However, the \code{package} argument is just the name of the package (rather than the longer string that is required by the \code{detach} function), and -- like the \code{library} function -- can be specified without quote marks. The \code{unlibrary} function does not unload dependencies, only the named package.

The name "unlibrary" is a bit of an abuse of both R terminology (in which one has a library of packages) and the English language, but I think it helps convey that the goal of the \code{unlibrary} function is to do the opposite of what the \code{library} function does. 
}

\value{
Identical to \code{detach}.
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
\code{\link{library}}, \code{\link{require}}, \code{\link{detach}}
}


\examples{
unlibrary( lsr )  # unload the lsr package
library( lsr )    # reload it
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
