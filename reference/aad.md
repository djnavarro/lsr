# Mean (average) absolute deviation from the mean

Calculates the mean absolute deviation from the sample mean

## Usage

``` r
aad(x, na.rm = FALSE)
```

## Arguments

- x:

  A vector containing the observations.

- na.rm:

  A logical value indicating whether or not missing values should be
  removed. Defaults to `FALSE`

## Value

Numeric

## Details

The `aad` function calculates the average (i.e. mean) absolute deviation
from the mean value of `x`, removing `NA` values if requested by the
user. It exists primarily to simplify the discussion of descriptive
statistics during an introductory stats class.

## Examples

``` r
# basic usage
X <- c(1, 3, 6)  # data
aad(X)           # returns a value of 1.777
#> [1] 1.777778

# removing missing data
X <- c(1, 3, NA, 6)   # data
aad(X)                # returns NA
#> [1] NA
aad(X, na.rm = TRUE)  # returns 1.777
#> [1] 1.777778
```
