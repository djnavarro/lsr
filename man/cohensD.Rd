\name{cohensD}
\alias{cohensD}

\title{Cohen's d}
\description{Calculates the Cohen's d measure of effect size.}

\usage{ cohensD( x = NULL, y = NULL, data = NULL, method = "pooled", mu = 0, formula = NULL ) }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A numeric variable containing the data for group 1, or possibly a formula of the form \code{outcome ~ group}}
  \item{y}{If \code{x} is a numeric variable, the \code{y} argument should be a numeric variable containing the data for group 2. If a one-sample calculation is desired, then no value for \code{y} should be specified.}
  \item{data}{If \code{x} is a formula, then \code{data} is an optional argument specifying data frame containing the variables in the formula.}
  \item{method}{Which version of the d statistic should we calculate? Possible values are \code{"pooled"} (the default), \code{"x.sd"}, \code{"y.sd"}, \code{"corrected"}, \code{"raw"}, \code{"paired"} and \code{"unequal"}. See below for specifics. }
  \item{mu}{The "null" value against which the effect size should be measured. This is almost always 0 (the default), so this argument is rarely specified.}
  \item{formula}{An alias for \code{x} if a formula input is used. Included for the sake of consistency with the \code{t.test} function.}
}

\details{The \code{cohensD} function calculates the Cohen's d measure of effect size in one of several different formats. The function is intended to be called in one of two different ways, mirroring the \code{t.test} function. That is, the first input argument \code{x} is a formula, then a command of the form \code{cohensD(x = outcome~group, data = data.frame)} is expected, whereas if \code{x} is a numeric variable, then a command of the form \code{cohensD(x = group1, y = group2)} is expected. Note that \code{cohensD} is not a generic function.

The \code{method} argument allows the user to select one of several different variants of Cohen's d. Assuming that the original t-test for which an effect size is desired was an independent samples t-test (i.e., not one sample or paired samples t-test), then there are several possibilities for how the normalising term (i.e., the standard deviation estimate) in Cohen's d should be calculated. The most commonly used method is to use the same pooled standard deviation estimate that is used in a Student t-test (\code{method = "pooled"}, the default). If \code{method = "raw"} is used, then the same pooled standard deviation estimate is used, except that the sample standard deviation is used (divide by N) rather than the unbiased estimate of the population standard deviation (divide by N-2). Alternatively, there may be reasons to use only one of the two groups to estimate the standard deviation. To do so, use \code{method = "x.sd"} to select the \code{x} variable, or the first group listed in the grouping factor; and \code{method = "y.sd"} to normalise by \code{y}, or the second group listed in the grouping factor. The last of the "Student t-test" based measures is the unbiased estimator of d (\code{method = "corrected"}), which multiplies the "pooled" version by (N-3)/(N-2.25). 

For other versions of the t-test, there are two possibilities implemented. If the original t-test did not make a homogeneity of variance assumption, as per the Welch test, the normalising term should mirror the Welch test (\code{method = "unequal"}). Or, if the original t-test was a paired samples t-test, and the effect size desired is intended to be based on the standard deviation of the differences, then \code{method = "paired"} should be used. 

The last argument to \code{cohensD} is \code{mu}, which represents the mean against which one sample Cohen's d calculation should be assessed. Note that this is a slightly narrower usage of \code{mu} than the \code{t.test} function allows. \code{cohensD} does not currently support the use of a non-zero \code{mu} value for a paired-samples calculation. 
}


\section{Argument checking}{

\code{cohensD} checks whether the arguments specified by the user make sense. For instance, specifying numeric variables \code{x} and \code{y} together with a mean \code{mu} does not make sense, since the \code{x} and \code{y} values imply a two-sample calculation, but \code{mu} implies a one sample calculation. The cases that the function is "intended" to support are listed below.

The following produce a one-sample Cohen's d:

\enumerate{
  \item numeric \code{x}
  \item numeric \code{x}, numeric \code{mu} of length 1
}


The following produce a paired-sample Cohen's d:

\enumerate{
  \item numeric \code{x}, numeric \code{y}, \code{method="paired"}
}


The following produce a two-sample Cohen's d:

\enumerate{
  \item numeric \code{x}, numeric \code{y}
  \item numeric \code{x}, numeric \code{y}, valid value for \code{method} (except "paired") 
  \item formula \code{formula}, data frame \code{data} 
  \item formula \code{formula}, data frame \code{data}, valid value for \code{method} (except "paired")
  \item formula \code{formula}, valid value for \code{method} (except "paired")
  \item formula \code{formula}
}

In a perfect world, these would be the only input combinations allowed. However, because it is commonplace for people to drop argument names, and because I don't want to break backwards compatibility for \code{cohensD} too much, there are a number of cases where the function attempts to guess the user's intention.

The following produce a paired-sample Cohen's d:

\enumerate{
  \item formula \code{x}, \code{method="paired"} [issues warning]
  \item formula \code{x}, data frame \code{y}, \code{method="paired"} [issues warning]
  \item formula \code{x}, data frame \code{data}, \code{method="paired"} [issues warning]
  \item formula \code{formula}, data frame \code{x}, \code{method="paired"} [issues warning]
  \item formula \code{formula}, data frame \code{data}, \code{method="paired"} [issues warning]
  \item formula \code{formula}, \code{method="paired"} [issues warning]
}

The following produce a two-sample Cohen's d:

\enumerate{
  \item formula \code{x}
  \item formula \code{x}, data frame \code{y}
  \item formula \code{x}, data frame \code{data}
  \item formula \code{x}, valid value for \code{method} (except "paired")
  \item formula \code{x}, data frame \code{y}, valid value for \code{method} (except "paired")
  \item formula \code{x}, data frame \code{data}, valid value for \code{method} (except "paired")
  \item formula \code{formula}, data frame \code{x}
  \item formula \code{formula}, data frame \code{x}, valid value for \code{method} (except "paired")
}

}

\value{Numeric variable containing the effect size, d. Note that it does not show the direction of the effect, only the magnitude. That is, the value of d returned by the function is always positive or zero.
}


\section{Warning}{This package is under development, and has been released only due to teaching constraints. Until this notice disappears from the help files, you should assume that everything in the package is subject to change. Backwards compatibility is NOT guaranteed. Functions may be deleted in future versions and new syntax may be inconsistent with earlier versions. For the moment at least, this package should be treated with extreme caution.}

\references{
Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale, NJ: Lawrence Erlbaum Associates.
}
\author{
Daniel Navarro
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{t.test}}, \code{\link{oneSampleTTest}}, \code{\link{pairedSamplesTTest}}, \code{\link{independentSamplesTTest}} 
}

\examples{
# calculate Cohen's d for two independent samples:
gradesA <- c(55, 65, 65, 68, 70) # 5 students with teacher A
gradesB <- c(56, 60, 62, 66)     # 4 students with teacher B
cohensD(gradesA, gradesB)

# calculate Cohen's d for the same data, described differently:
grade <- c(55, 65, 65, 68, 70, 56, 60, 62, 66) # grades for all students
teacher <- c("A", "A", "A", "A", "A", "B", "B", "B", "B") # teacher for each student
cohensD(grade ~ teacher)

# calculate Cohen's d for two paired samples:
pre  <- c(100, 122, 97, 25, 274) # a pre-treatment measure for 5 cases
post <- c(104, 125, 99, 29, 277) # the post-treatment measure for the same 5 cases
cohensD(pre, post, method = "paired") # ... explicitly indicate that it's paired, or else
cohensD(post - pre)  # ... do a "single-sample" calculation on the difference

# support for data frames:
exams <- data.frame(grade, teacher)
cohensD(exams$grade ~ exams$teacher)    # using $ 
cohensD(grade ~ teacher, data = exams)  # using the 'data' argument 

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
