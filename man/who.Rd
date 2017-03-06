\name{who}

\alias{who}

\title{Contents of workspace}

\description{Prints out a simple summary of all the objects in the workspace}

\usage{
who( expand = FALSE )
}


\arguments{
\item{expand}{Should R "expand" data frames when listing variables? If \code{expand = TRUE}, variables inside a data frame are included in the output. The default is \code{FALSE}}
}


\details{The \code{who} function prints out some basic information about all variables in the workspace. Specifically, it lists the names of all variables, what class they are, and how big they are (see below for specifics). If the \code{expand} argument is \code{TRUE} it will also print out the same information about variables within data frames. See the examples below to see what the output looks like.

The purpose for the function is to show more information than the \code{objects} function (especially as regards the names of variables inside data frames), but not to show as much detail as the \code{ls.str} function, which is generally too verbose for novice users. 

The "size" of an object is only reported for some kinds of object: specifically, only those objects whose \code{\link{mode}} is either \code{numeric}, \code{character}, \code{logical}, \code{complex} or \code{list}. Nothing is printed for any other kind of object. If the object has explicit dimensions (e.g., data frames or matrices) then \code{who} prints out the dimension sizes (e.g., "2 x 3" ). Otherwise the length of the object is printed. 
}

\value{\code{who} returns an object of class \code{whoList} which is just a data frame with a dedicated print method.}

\section{Warning}{This package is under development, and has been released only due to teaching constraints. Until this notice disappears from the help files, you should assume that everything in the package is subject to change. Backwards compatibility is NOT guaranteed. Functions may be deleted in future versions and new syntax may be inconsistent with earlier versions. For the moment at least, this package should be treated with extreme caution.}

%\references{}

\author{Daniel Navarro}


\seealso{
\code{\link{objects}}, \code{\link{ls.str}}
}
\examples{
### Example 1 ###

my.number <- 4
my.name <- "Dan"

# display the workspace:
who()
#
#  -- Name --      -- Class --   -- Size --
#  my.name         character     1         
#  my.number       numeric       1
#


### Example 2 ###

# create a data frame
dataset <- data.frame( hi = c( "hello","cruel","world" ), 
                       pi = c( 3,1,4 ) )

# now display the workspace:
who()
#
#  -- Name --      -- Class --   -- Size --
#  dataset         data.frame    3 x 2     
#  my.name         character     1         
#  my.number       numeric       1
#

# expand the data frame:
who( expand = TRUE )
#
#  -- Name --      -- Class --   -- Size --
#  dataset         data.frame    3 x 2     
#   $hi            factor        3         
#   $pi            numeric       3
#  my.name         character     1         
#  my.number       numeric       1
#

### Example 3 ###

# add a function to the workspace
quadruple <- function(x) { x*4 }

# add a formula to the workspace
my.formula <- blah ~ blah + blah 

# now display the workspace:
who()
#
#  -- Name --      -- Class --   -- Size --
#  dataset         data.frame    3 x 2               
#  my.formula      formula                 
#  my.name         character     1         
#  my.number       numeric       1         
#  quadruple       function                
#

}
