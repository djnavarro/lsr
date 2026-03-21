# Cut by quantiles

Cuts a variable into equal sized categories

It is sometimes convenient (though not always wise) to split a
continuous numeric variable `x` into a set of `n` discrete categories
that contain an approximately equal number of cases. The `quantileCut`
function does exactly this. The actual categorisation is done by the
[`cut`](https://rdrr.io/r/base/cut.html) function. However, instead of
selecting ranges of equal sizes (the default behaviour in `cut`), the
`quantileCut` function uses the
[`quantile`](https://rdrr.io/r/stats/quantile.html) function to select
unequal sized ranges so as to ensure that each of the categories
contains the same number of observations. The intended purpose of the
function is to assist in exploratory data analysis; it is not generally
a good idea to use the output of `quantileCut` function as a factor in
an analysis of variance, for instance, since the factor levels are not
interpretable and will almost certainly violate homogeneity of variance.

## Usage

``` r
quantileCut(x, n, ...)
```

## Arguments

- x:

  A vector containing the observations.

- n:

  Number of categories

- ...:

  Additional arguments to cut

## Value

A factor containing `n` levels. The factor levels are determined in the
same way as for the `cut` function, and can be specified manually using
the `labels` argument, which is passed to the `cut` function.

## See also

[`cut`](https://rdrr.io/r/base/cut.html),
[`quantile`](https://rdrr.io/r/stats/quantile.html)

## Examples

``` r
# An example illustrating why care is needed

dataset <- c( 0,1,2, 3,4,5, 7,10,15 )       # note the uneven spread of data
x <- quantileCut( dataset, 3 )              # cut into 3 equally frequent bins
table(x)                                    # tabulate
#> x
#> (-0.015,2.67]   (2.67,5.67]     (5.67,15] 
#>             3             3             3 

# For comparison purposes, here is the behaviour of the more standard cut
# function when applied to the same data:
y <- cut( dataset, 3 )
table(y)
#> y
#> (-0.015,5]     (5,10]    (10,15] 
#>          6          2          1 
```
