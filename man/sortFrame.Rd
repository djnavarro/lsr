\name{sortFrame}
\alias{sortFrame}

\title{
Sort a data frame
}

\description{Sorts a data frame using one or more variables.
}

\usage{
sortFrame(x,...,alphabetical = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Data frame to be sorted}
  \item{...}{A list of sort terms (see below)}
  \item{alphabetical}{Should character vectors be sorted alphabetically?}
}

\details{The simplest use of this function is to sort a data frame \code{x} in terms of one or more of the variables it contains. If for instance, the data frame \code{x} contains two variables \code{a} and \code{b}, then the command \code{sortFrame(x,a,b)} sorts by variable \code{a}, breaking ties using variable \code{b}. Numeric variables are sorted in ascending order: to sort in descending order of \code{a} and then ascending order of \code{b}, use the command \code{sortFrame(x,-a,b)}. Factors are treated as numeric variables, and are sorted by the internal codes (i.e., the first factor level equals 1, the second factor levels equals 2 and so on). Character vectors are sorted in alphabetical order, which differs from the ordering used by the \code{\link{sort}} function; to use the default 'ascii' ordering, specify \code{alphabetical=FALSE}. Minus signs can be used in conjunction with character vectors in order to sort in reverse alphabetical order. If \code{c} represents a character variable, then \code{sortFrame(x,c)} sorts in alphabetical order, whereas \code{sortFrame(x,-c)} sorts in reverse alphabetical order.

It is also possible to specify more complicated sort terms by including expressions using multiple variables within a single term, but care is required. For instance, it is possible to sort the data frame by the sum of two variables, using the command \code{sortFrame(x, a+b)}. For numeric variables expressions of this kind should work in the expected manner, but this is not always the case for non-numeric variables: \code{sortFrame} uses  the \code{\link{xtfrm}} function to provide, for every variable referred to in the list of sort terms (\code{...}) a numeric vector that sorts in the same order as the original variable. This reliance is what makes reverse alphabetical order (e.g., \code{sortFrame(x,-c)}) work. However, it also means that it is possible to specify somewhat nonsensical sort terms for character vectors by abusing the numerical coding (e.g. \code{sortFrame(x,(c-3)^2)}; see the examples section). It also means that sorting in terms of string operation functions (e.g., \code{nchar}) do not work as expected. See examples section. Future versions of \code{sortFrame} will (hopefully) address this, possibly by allowing the user to "switch off" the internal use of \code{xtfrm}, or else by allowing \code{\link{AsIs}} expressions to be used in sort terms. 
}

\value{The sorted data frame}

\section{Warning}{This package is under development, and has been released only due to teaching constraints. Until this notice disappears from the help files, you should assume that everything in the package is subject to change. Backwards compatibility is NOT guaranteed. Functions may be deleted in future versions and new syntax may be inconsistent with earlier versions. For the moment at least, this package should be treated with extreme caution.}

%\references{
%% ~put references to the literature/web site here ~
%}

\author{
Daniel Navarro
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sort}}, \code{\link{order}}, \code{\link{xtfrm}}
}


\examples{# first, create a data frame to be sorted...
txt <- c("bob","Clare","clare","bob","eve","eve")
num1 <- c(3,1,2,0,0,2)
num2 <- c(1,1,3,0,3,2)
etc <- c("not","used","as","a","sort","term")
dataset <- data.frame( txt, num1, num2, etc, stringsAsFactors=FALSE )
#       txt num1 num2  etc
#   1   bob    3    1  not
#   2 Clare    1    1 used
#   3 clare    2    3   as
#   4   bob    0    0    a
#   5   eve    0    3 sort
#   6   eve    2    2 term

#### Sorting by numeric variables ####

# Simplest use of the function is to sort the data frame by a single
# numeric variable, with the results to be returned in increasing 
# order, and ties to be kept in their original order:

sortFrame( dataset, num1 )
#       txt num1 num2  etc
#   4   bob    0    0    a
#   5   eve    0    3 sort
#   2 Clare    1    1 used
#   3 clare    2    3   as
#   6   eve    2    2 term
#   1   bob    3    1  not

# Specifying a second sort term will break ties using the second 
# term. For instance, we can sort by 'num1' (ascending order), 
# breaking ties by 'num2' (ascending order):

sortFrame( dataset, num1, num2 )
#       txt num1 num2  etc
#   4   bob    0    0    a
#   5   eve    0    3 sort
#   2 Clare    1    1 used
#   6   eve    2    2 term
#   3 clare    2    3   as
#   1   bob    3    1  not

# Specifying negative numbers sorts in descending order. For 
# instance, to sort by 'num1' in descending order, but break
# ties by 'num2' in ascending order, use the following:

sortFrame( dataset, -num1, num2 )
#       txt num1 num2  etc
#   1   bob    3    1  not
#   6   eve    2    2 term
#   3 clare    2    3   as
#   2 Clare    1    1 used
#   4   bob    0    0    a
#   5   eve    0    3 sort


#### Sorting by character variables ####

# When sorting by character variables (but not factors - see
# below), the default is to sort alphabetically, with upper
# case preceding lowr case. For example:

sortFrame( dataset, txt )
#       txt num1 num2  etc
#   1   bob    3    1  not
#   4   bob    0    0    a
#   2 Clare    1    1 used
#   3 clare    2    3   as
#   5   eve    0    3 sort
#   6   eve    2    2 term

# Sorting into reverse alphabetical order is possible using
# minus signs. For example:

sortFrame( dataset, -txt )
#       txt num1 num2  etc
#   5   eve    0    3 sort
#   6   eve    2    2 term
#   3 clare    2    3   as
#   2 Clare    1    1 used
#   1   bob    3    1  not
#   4   bob    0    0    a	


#### Other examples #####

# If alphabetical order is not desired for character variables
# it is possible to force sortFrame to use the default 'ASCII'
# ordering, in which all upper case letters precede all lower 
# case ones, by specifying alphabetical = FALSE, as follows:
 
sortFrame( dataset, txt, alphabetical = FALSE )
#       txt num1 num2  etc
#   2 Clare    1    1 used
#   1   bob    3    1  not
#   4   bob    0    0    a
#   3 clare    2    3   as
#   5   eve    0    3 sort
#   6   eve    2    2 term

# Note that factors are treated differently to character vectors. 
# They are not sorted alphabetically: instead they are sorted in 
# factor level order. For example, if we construct a data frame in
# which 'txt' is a factor, the results follow the order of the levels

dataset2 <- dataset
dataset2$txt <- factor( dataset2$txt, levels = c('bob','eve','clare','Clare'))

sortFrame( dataset2, txt )
#       txt num1 num2  etc
#   1   bob    3    1  not
#   4   bob    0    0    a
#   5   eve    0    3 sort
#   6   eve    2    2 term
#   3 clare    2    3   as
#   2 Clare    1    1 used

# Sorting by functions of multible variables is also possible.
# Note that this functionality is intended to be applied to numeric 
# variables only. It does work for non-numeric variables because of 
# the internal reliance on the xtfrm function, but the results may
# be unexpected. 

sortFrame( dataset, num1+num2 )
#       txt num1 num2  etc
#   4   bob    0    0    a
#   2 Clare    1    1 used
#   5   eve    0    3 sort
#   1   bob    3    1  not
#   6   eve    2    2 term
#   3 clare    2    3   as


# An example of a nonsensical application of mathematical operations 
# when sorting by character vector. It works, since the character 
# vector is treated as a numeric equivalent internally, but the 
# output does not make a great deal of sense:

sortFrame( dataset, (txt-3)^2 )
#       txt num1 num2  etc
#   2 Clare    1    1 used
#   3 clare    2    3   as
#   1   bob    3    1  not
#   4   bob    0    0    a
#   5   eve    0    3 sort
#   6   eve    2    2 term

# An example where sorting by text processing operations fails because 
# the xtfrm function converts it to a numerical vector before the text 
# processing operation is applied:

sortFrame( dataset, nchar(txt) )
#       txt num1 num2  etc
#   1   bob    3    1  not
#   2 Clare    1    1 used
#   3 clare    2    3   as
#   4   bob    0    0    a
#   5   eve    0    3 sort
#   6   eve    2    2 term

# ... no sorting has occurred here. Future releases may allow "as is" 
# terms to be included, which would allow something along the following 
# lines: sortFrame( dataset, nchar(I(txt)) ), and would produce the 
# desired output where the longer strings are sorted to the bottom of the
# data frame. However, no such functionality currently exists. 


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
