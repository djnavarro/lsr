\name{tFrame}
\alias{tFrame}

\title{
Transpose a data frame
}

\description{Transposes a data frame, converting variables to cases and vice versa}

\usage{
tFrame(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The data frame to be transposed.}
}

\details{
The \code{tFrame} function is a convenience function that simply transposes the input data frame and coerces the result back to a data frame. Apart from a very small amount of exception handling, it is equivalent to \code{as.data.frame(t(x))}. It exists simply because I sometimes find it convenient when teaching statistics to discuss simple data handling before going into details regarding coercion; similarly, since I generally have students work with data frames before exposing them to matrices, it is convenient to have a transpose function that returns a data frame as output. 

Naturally, the \code{tFrame} function should only be used when it is actually sensible to think of the cases of \code{x} as variables in their own right. In real life I expect that this maps almost perfectly onto those cases where \code{x} could be a matrix just as easily as a data frame, so I don't believe that \code{tFrame} is useful in real world data analysis. It is intended as a teaching tool. 
}

\value{
The transposed data frame
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
\code{\link{t}}
}


\examples{# Create a data frame that could sensibly be transposed...	
Gf <- c(105, 119, 121, 98)   # fluid intelligence for 4 people
Gc <- c(110, 115, 119, 103)  # crystallised intelligence 
Gs <- c(112, 102, 108, 99)   # speed of processing
dataset <- data.frame( Gf, Gc, Gs ) 
rownames(dataset) <- paste( "person", 1:4, sep="" )
print(dataset)	

# Now transpose it...
tFrame( dataset )
	 	
	
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
