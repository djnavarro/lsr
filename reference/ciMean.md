# Confidence interval around the mean

Calculates a confidence interval for the mean of a numeric variable (or
each numeric variable in a data frame or matrix).

## Usage

``` r
ciMean(x, conf = 0.95, na.rm = FALSE)
```

## Arguments

- x:

  A numeric vector, matrix, or data frame.

- conf:

  The confidence level. Defaults to `0.95` for a 95% interval.

- na.rm:

  Set to `TRUE` to remove missing values before computing the interval.
  Defaults to `FALSE`.

## Value

A matrix with one row per variable and two columns giving the lower and
upper bounds of the confidence interval. Column names reflect the
confidence level (e.g., `"2.5%"` and `"97.5%"` for a 95% interval).

## Details

Calculates a confidence interval for the mean under the standard
assumption that the data are normally distributed. When `x` is a matrix
or data frame, a separate interval is computed for each column.
Non-numeric columns in a data frame produce `NA` rows in the output.

## See also

[`t.test`](https://rdrr.io/r/stats/t.test.html),
[`mean`](https://rdrr.io/r/base/mean.html)

## Examples

``` r
x <- c(1, 3, 6)
ciMean(x) # 95% confidence interval
#>        2.5%    97.5%
#> x -2.918276 9.584943
ciMean(x, conf = 0.80) # 80% confidence interval
#>         10%      90%
#> x 0.5935938 6.073073

# for comparison: equivalent result via lm
confint(lm(x ~ 1))
#>                 2.5 %   97.5 %
#> (Intercept) -2.918276 9.584943

# missing values
x <- c(1, 3, NA, 6)
ciMean(x, na.rm = TRUE)
#>        2.5%    97.5%
#> x -2.918276 9.584943
```
