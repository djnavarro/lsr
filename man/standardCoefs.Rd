\name{standardCoefs}
\alias{standardCoefs}

\title{
Standardised regression coefficients
}

\description{
Calculates the standardised regression coefficients for a linear model.
}

\usage{
standardCoefs(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A linear model object (i.e. class \code{lm}).}
}

\details{Calculates the standardised regression coefficients (beta-weights), namely the values of the regression coefficients that would have been observed has all regressors and the outcome variable been scaled to have mean 0 and variance 1 before fitting the regression model. Standardised coefficients are often useful in some applied contexts since there is some sense in which all beta values are "on the same scale", though this is not entirely unproblematic. 
}

\value{A matrix with the regressors as rows, and the two different regression coefficients (unstandardised and standardised) as the two columns. The columns are labeled b (unstandardised) and beta (standardised).  
}

\section{Warning}{This package is under development, and has been released only due to teaching constraints. Until this notice disappears from the help files, you should assume that everything in the package is subject to change. Backwards compatibility is NOT guaranteed. Functions may be deleted in future versions and new syntax may be inconsistent with earlier versions. For the moment at least, this package should be treated with extreme caution.

(This function warrants special care: it has not been tested on as many cases as I would like)}


%\references{
%% ~put references to the literature/web site here ~
%}

\author{
Daniel Navarro
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%}


\examples{
### Example 1: simple linear regression ###
	
# data	
X1 <- c(0.69, 0.77, 0.92, 1.72, 1.79, 2.37, 2.64, 2.69, 2.84, 3.41)
Y  <- c(3.28, 4.23, 3.34, 3.73, 5.33, 6.02, 5.16, 6.49, 6.49, 6.05)

model1 <- lm( Y ~ X1 )  # run a simple linear regression
coefficients( model1 )  # extract the raw regression coefficients
standardCoefs( model1 ) # extract standardised coefficients


### Example 2: multiple linear regression ###

X2 <- c(0.19, 0.22, 0.95, 0.43, 0.51, 0.04, 0.12, 0.44, 0.38, 0.33) 
model2 <- lm( Y ~ X1 + X2 )   # new model
standardCoefs( model2 )       # standardised coefficients


### Example 3: interaction terms ### 

model3 <- lm( Y ~ X1 * X2 )
coefficients( model3 )
standardCoefs( model3 )

# Note that these beta values are equivalent to standardising all 
# three *regressors* including the interaction term X1:X2, not merely 
# standardising the two predictors X1 and X2.  

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
