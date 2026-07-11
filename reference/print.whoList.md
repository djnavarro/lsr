# Print workspace summary

Prints a workspace summary in a readable format. This function is called
automatically whenever a result from
[`who`](https://lsr.djnavarro.net/reference/who.md) is displayed.

## Usage

``` r
# S3 method for class 'whoList'
print(x, ...)
```

## Arguments

- x:

  A workspace summary, as returned by
  [`who`](https://lsr.djnavarro.net/reference/who.md).

- ...:

  Additional arguments (unused, included for compatibility).

## Value

Invisibly returns `x` unchanged.
