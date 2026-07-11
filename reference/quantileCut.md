# Cut by quantiles

Divides a numeric variable into `n` categories that each contain
approximately the same number of observations.

## Usage

``` r
quantileCut(x, n, ...)
```

## Arguments

- x:

  A numeric vector.

- n:

  The number of categories to create.

- ...:

  Additional arguments passed to
  [`cut`](https://rdrr.io/r/base/cut.html), such as `labels`.

## Value

A factor with `n` levels. Level labels follow the same convention as
[`cut`](https://rdrr.io/r/base/cut.html) and can be overridden with the
`labels` argument.

## Details

Unlike [`cut`](https://rdrr.io/r/base/cut.html), which creates
categories of equal width, `quantileCut` uses
[`quantile`](https://rdrr.io/r/stats/quantile.html) to find breakpoints
that produce roughly equal-sized groups. This can be useful in
exploratory analysis, but the resulting categories are data-driven and
may not have a clear interpretation. Using them as grouping variables in
an ANOVA is generally not recommended, as the breakpoints are arbitrary
and the groups will typically not have equal variances.

## See also

[`cut`](https://rdrr.io/r/base/cut.html),
[`quantile`](https://rdrr.io/r/stats/quantile.html)

## Examples

``` r
# the data are unevenly spread, so equal-width bins would be unbalanced
x <- c(0, 1, 2, 3, 4, 5, 7, 10, 15)

# quantileCut creates equal-frequency bins
bins_eq_freq <- quantileCut(x, 3)
table(bins_eq_freq)
#> bins_eq_freq
#> (-0.015,2.67]   (2.67,5.67]     (5.67,15] 
#>             3             3             3 

# compare to cut(), which creates equal-width bins
bins_eq_width <- cut(x, 3)
table(bins_eq_width)
#> bins_eq_width
#> (-0.015,5]     (5,10]    (10,15] 
#>          6          2          1 
```
