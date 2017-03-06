\name{wideToLong}
\alias{wideToLong}


\title{
Reshape from wide to long
}

\description{
Reshape a data frame from wide form to long form using the variable names
}

\usage{
wideToLong( data, within="within", sep="_", split=TRUE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{The data frame.}
  \item{within}{Name to give to the long-form within-subject factor(s)}
  \item{sep}{Separator string used in wide-form variable names}
  \item{split}{Should multiple within-subject factors be split into multiple variables?}
}

\details{
The \code{wideToLong} function is the companion function to \code{longToWide}. The \code{data} argument is a "wide form" data frame, in which each row corresponds to a single experimental unit (e.g., a single subject).  The output is a "long form" data frame, in which each row corresponds to a single observation.

The \code{wideToLong} function relies on the variable names to determine how the data should be reshaped. The naming scheme for these variables places the name of the measured variable first, followed by the levels of the within-subjects variable(s), separated by the separator string \code{sep} (default is \code{_}) The separator string cannot appear anywhere else in the variable names: variables without the separator string are assumed to be between-subject variables. 

If the experiment measured the \code{accuracy} of participants at some task at two different points in time, then the wide form data frame would contain variables of the form \code{accuracy_t1} and \code{accuracy_t2}. After reshaping, the long form data frame would contain one measured variable called \code{accuracy}, and a within-subjects factor with levels \code{t1} and \code{t2}. The name of the within-subjects factor is the \code{within} argument. 

The function supports experimental designs with multiple within-subjects factors and multi-variable observations. For example, suppose each experimental subject is tested in two \code{conditions} (\code{cond1} and \code{cond2}), on each of two \code{days} (\code{day1} and \code{day2}), yielding an experimental design in which four observations are made for each subject. For each such observation, we record the mean response time \code{MRT} for and proportion of correct responses \code{PC} for the participant.  The variable names needed for a design such as this one would be \code{MRT_cond1_day1}, \code{MRT_cond1_day2}, \code{PC_cond1_day1}, etc. The \code{within} argument should be a vector of names for the within-subject factors: in this case, \code{within = c("condition","day")}. 

By default, if there are multiple within-subject factors implied by the existence of multiple separators, the output will keep these as distinct variables in the long form data frame (\code{split=FALSE}). If \code{split=TRUE}, the within-subject factors will be collapsed into a single variable. 
}

\value{A data frame containing the reshaped data}

%\references{
%% ~put references to the literature/web site here ~
%}

\section{Warning}{This package is under development, and has been released only due to teaching constraints. Until this notice disappears from the help files, you should assume that everything in the package is subject to change. Backwards compatibility is NOT guaranteed. Functions may be deleted in future versions and new syntax may be inconsistent with earlier versions. For the moment at least, this package should be treated with extreme caution.}

\author{
Daniel Navarro
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{longToWide}}, \code{\link{reshape}}, \code{\link{stack}}, \code{cast} and \code{melt} (in the \pkg{reshape} package)
}


\examples{
# Outcome measure is mean response time (MRT), measured in two conditions
# with 4 participants. All participants participate in both conditions. 

wide <- data.frame( accuracy_t1 = c( .15,.50,.78,.55 ),  # accuracy at time point 1
                    accuracy_t2 = c( .55,.32,.99,.60 ),  # accuracy at time point 2
                    id = 1:4 )                           # id variable

# convert to long form
wideToLong( wide, "time" )


# A more complex design with multiple within-subject factors. Again, we have only
# four participants, but now we have two different outcome measures, mean response
# time (MRT) and the proportion of correct responses (PC). Additionally, we have two
# different repeated measures variables. As before, we have the experimental condition
# (cond1, cond2), but this time each participant does both conditions on two different
# days (day1, day2). Finally, we have multiple between-subject variables too, namely
# id and gender.

wide2 <- data.frame( id = 1:4,
                     gender = factor( c("male","male","female","female") ),
                     MRT_cond1_day1 = c( 415,500,478,550 ),
                     MRT_cond2_day1 = c( 455,532,499,602 ),
                     MRT_cond1_day2 = c( 400,490,468,502 ),
                     MRT_cond2_day2 = c( 450,518,474,588 ),
                     PC_cond1_day1 = c( 79,83,91,75 ),
                     PC_cond2_day1 = c( 82,86,90,78 ),
                     PC_cond1_day2 = c( 88,92,98,89 ),
                     PC_cond2_day2 = c( 93,97,100,95 ) )

# conversion to long form:
wideToLong( wide2 )
wideToLong( wide2, within = c("condition","day") )

# treat "condition x day" as a single repeated measures variable:
wideToLong( wide2, split = FALSE)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
