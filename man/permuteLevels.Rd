\name{permuteLevels}

\alias{permuteLevels}

\title{Permute the levels of a factor}

\description{
Apply an arbitrary permutation to the ordering of levels within a factor
}

\usage{
	permuteLevels(x,perm,ordered = is.ordered(x),invert=FALSE)
}

\arguments{
  \item{x}{The factor to be permuted}
  \item{perm}{A vector specifying the permutation}
  \item{ordered}{Should the output be an ordered factor?}
  \item{invert}{Use the inverse of \code{perm} to specify the permutation}
}

\details{
This is a convenience function used to shuffle the order in which the levels of a factor are specified. It is similar in spirit to the \code{relevel} function, but more general. The \code{relevel} function only changes the first level of the factor, whereas \code{permuteLevels} can apply an arbitrary permutation. This can be useful for plotting data, because some plotting functions will display the factor levels in the same order that they appear within the factor. 

The \code{perm} argument is a vector of the same length as \code{levels(x)}, such that \code{perm[k]} is an integer that indicates which of the old levels should be moved to position k. However, if \code{invert=TRUE}, the inverse permutation is applied: that is, \code{perm[k]} is an integer specifying where to move the k-th level of the original factor. See the Examples for more details.
}

\value{
Returns a factor with identical values, but with the ordering of the factor levels shuffled.
}

\section{Warning}{This package is under development, and has been released only due to teaching constraints. Until this notice disappears from the help files, you should assume that everything in the package is subject to change. Backwards compatibility is NOT guaranteed. Functions may be deleted in future versions and new syntax may be inconsistent with earlier versions. For the moment at least, this package should be treated with extreme caution.}

%\references{
%% ~put references to the literature/web site here ~
%}

\author{Daniel Navarro}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{factor}}, \code{\link{order}}, \code{\link{relevel}}
}

\examples{
#### Example 

# original factor specifies the levels in order: a,b,c,d,e,f
x <- factor( c(1,4,2,2,3,3,5,5,6,6), labels=letters[1:6] )
print(x)

# apply permutation (5 3 2 1 4 6)... i.e., move 5th factor level (e)
# into position 1, move 3rd factor level (c) into position 2, etc
permuteLevels(x,perm = c(5,3,2,1,4,6))

# apply the inverse of permutation (5 3 2 1 4 6)... i.e., move 1st
# level (a) into position 5, move 2nd level (b) into position 3, etc
permuteLevels(x,perm = c(5,3,2,1,4,6),invert=TRUE)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
