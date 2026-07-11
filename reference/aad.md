# Mean absolute deviation

Calculates the mean absolute deviation from the sample mean.

## Usage

``` r
aad(x, na.rm = FALSE)
```

## Arguments

- x:

  A numeric vector containing the observations.

- na.rm:

  Set to `TRUE` to remove missing values before computing the deviation.
  Defaults to `FALSE`.

## Value

A single number giving the mean absolute deviation.

## Details

Computes the average of the absolute differences between each
observation and the sample mean of `x`, i.e. `mean(abs(x - mean(x)))`.

## See also

[`mean`](https://rdrr.io/r/base/mean.html),
[`sd`](https://rdrr.io/r/stats/sd.html),
[`var`](https://rdrr.io/r/stats/cor.html)

## Examples

``` r
x <- c(1, 3, 6)
aad(x)
#> [1] 1.777778

# missing values
x <- c(1, 3, NA, 6)
aad(x) # returns NA
#> [1] NA
aad(x, na.rm = TRUE) # ignores the missing value
#> [1] 1.777778
```
