# Confidence interval around the mean

Calculates confidence intervals for the mean of a normally-distributed
variable.

## Usage

``` r
ciMean(x, conf = 0.95, na.rm = FALSE)
```

## Arguments

- x:

  A numeric vector, data frame or matrix containing the observations.

- conf:

  The level of confidence desired. Defaults to a 95% confidence interval

- na.rm:

  Logical value indicating whether missing values are to be removed.
  Defaults to `FALSE`.

## Value

The output is a matrix containing the lower and upper ends of the
confidence interval for each variable. If a data frame is specified as
input and contains non-numeric variables, the corresponding rows in the
output matrix have NA values.

## Details

This function calculates the confidence interval for the mean of a
variable (or set of variables in a data frame or matrix), under the
standard assumption that the data are normally distributed. By default
it returns a 95% confidence interval (`conf = 0.95`) and does not remove
missing values (`na.rm = FALSE`).

## Examples

``` r
X <- c(1, 3, 6)          # data
ciMean(X)                # 95 percent confidence interval
#>        2.5%    97.5%
#> X -2.918276 9.584943
ciMean(X, conf = .8)     # 80 percent confidence interval
#>         10%      90%
#> X 0.5935938 6.073073

confint( lm(X ~ 1) )     # for comparison purposes
#>                 2.5 %   97.5 %
#> (Intercept) -2.918276 9.584943

X <- c(1, 3, NA, 6)      # data with missing values
ciMean(X, na.rm = TRUE)  # remove missing values
#>        2.5%    97.5%
#> X -2.918276 9.584943
```
