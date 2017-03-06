\name{importList}
\alias{importList}

\title{
Import a list
}

\description{Creates variables in the workspace corresponding to the elements of a list}

\usage{
importList(x, ask = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{List to be imported}
  \item{ask}{Should R ask the user to confirm the new variables before creating them? (default is \code{TRUE})}
}

\details{
The \code{importList} function creates variables in the parent environment (generally the global workspace) corresponding to each of the elements of the list object \code{x}. If the names of these elements do not correspond to legitimate variables names they are converted using the \code{\link{make.names}} functions to valid variables names. 
}

\value{Invisibly returns \code{0} if the user chooses not to import the variables, otherwise invisibly returns \code{1}.}


%\references{
%% ~put references to the literature/web site here ~
%}

\section{Warning}{This package is under development, and has been released only due to teaching constraints. Until this notice disappears from the help files, you should assume that everything in the package is subject to change. Backwards compatibility is NOT guaranteed. Functions may be deleted in future versions and new syntax may be inconsistent with earlier versions. For the moment at least, this package should be treated with extreme caution.}

\author{
Daniel Navarro
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{unlist}}, \code{\link{attach}} }


\examples{# data set organised into two groups
data <- c(1,2,3,4,5)
group <- c("group A","group A","group B","group B","group B")

# the split function creates a list with two elements
# named "group A" and "group B", each containing the 
# data for the respective groups
data.list <- split( data, group )

# The data.list variable looks like this:

#   $`group A`
#   [1] 1 2
#
#   $`group B`
#   [1] 3 4 5

# importing the list with the default value of ask = TRUE will
# cause R to wait on the user's approval. Typing this:

#   importList( data.list )

# would produce the following output:

#   Names of variables to be created:
#   [1] "group.A" "group.B"
#   Create these variables? [y/n] 

# If the user then types y, the new variables are created.

# this version will silently import the variables.
importList( x = data.list, ask = FALSE )

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
