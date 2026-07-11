# Print chi-square association test results

Prints the results of a chi-square test of association in a readable
format. This function is called automatically whenever a result from
[`associationTest`](https://lsr.djnavarro.net/reference/associationTest.md)
is displayed.

## Usage

``` r
# S3 method for class 'assocTest'
print(x, ...)
```

## Arguments

- x:

  An association test result, as returned by
  [`associationTest`](https://lsr.djnavarro.net/reference/associationTest.md).

- ...:

  Additional arguments (unused, included for compatibility).

## Value

Invisibly returns `x` unchanged.
