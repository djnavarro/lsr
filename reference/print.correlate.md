# Print correlation matrix results

Prints the results of a correlation analysis in a readable format. This
function is called automatically whenever a result from
[`correlate`](https://lsr.djnavarro.net/reference/correlate.md) is
displayed.

## Usage

``` r
# S3 method for class 'correlate'
print(x, ...)
```

## Arguments

- x:

  A correlation result, as returned by
  [`correlate`](https://lsr.djnavarro.net/reference/correlate.md).

- ...:

  Additional arguments (unused, included for compatibility).

## Value

Invisibly returns `x` unchanged.
