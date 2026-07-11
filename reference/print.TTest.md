# Print t-test results

Prints the results of a t-test in a readable, beginner-friendly format.
This function is called automatically whenever a result from
[`oneSampleTTest`](https://lsr.djnavarro.net/reference/oneSampleTTest.md),
[`independentSamplesTTest`](https://lsr.djnavarro.net/reference/independentSamplesTTest.md),
or
[`pairedSamplesTTest`](https://lsr.djnavarro.net/reference/pairedSamplesTTest.md)
is displayed.

## Usage

``` r
# S3 method for class 'TTest'
print(x, ...)
```

## Arguments

- x:

  A t-test result, as returned by
  [`oneSampleTTest`](https://lsr.djnavarro.net/reference/oneSampleTTest.md),
  [`independentSamplesTTest`](https://lsr.djnavarro.net/reference/independentSamplesTTest.md),
  or
  [`pairedSamplesTTest`](https://lsr.djnavarro.net/reference/pairedSamplesTTest.md).

- ...:

  Additional arguments (unused, included for compatibility).

## Value

Invisibly returns `x` unchanged.
