# Print goodness of fit test results

Prints the results of a chi-square goodness of fit test in a readable
format. This function is called automatically whenever a result from
[`goodnessOfFitTest`](https://lsr.djnavarro.net/reference/goodnessOfFitTest.md)
is displayed.

## Usage

``` r
# S3 method for class 'gofTest'
print(x, ...)
```

## Arguments

- x:

  A goodness of fit test result, as returned by
  [`goodnessOfFitTest`](https://lsr.djnavarro.net/reference/goodnessOfFitTest.md).

- ...:

  Additional arguments (unused, included for compatibility).

## Value

Invisibly returns `x` unchanged.
