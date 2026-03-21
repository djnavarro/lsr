# Copies a vector into a matrix

Copies a vector into a matrix

## Usage

``` r
colCopy(x, times, dimnames = NULL)

rowCopy(x, times, dimnames = NULL)
```

## Arguments

- x:

  The vector to be copied

- times:

  Number of copies of the vector to bind together

- dimnames:

  List specifying row and column names

## Value

For `rowCopy`, the output is a matrix with `times` rows and `length(x)`
columns, in which each row contains the vector `x`. For `colCopy`, each
column corresponds to the vector `x`.

## Details

This is a convenience function for binding together multiple copies of
the same vector. The intended usage is for situations where one might
ordinarily use `rbind` or `cbind`, but the work is done by the `matrix`
function. Instead of needing to input multiple copies of the input
vector `x` (as one would for `rbind`), one only needs to specify the
number of `times` that the vector should be copied.

## Examples

``` r
#Example 1: basic usage
data <- c(3,1,4,1,5)
rowCopy( data, 4 )
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    3    1    4    1    5
#> [2,]    3    1    4    1    5
#> [3,]    3    1    4    1    5
#> [4,]    3    1    4    1    5
colCopy( data, 4 )
#>      [,1] [,2] [,3] [,4]
#> [1,]    3    3    3    3
#> [2,]    1    1    1    1
#> [3,]    4    4    4    4
#> [4,]    1    1    1    1
#> [5,]    5    5    5    5

#Example 2: attach dimension names
dnames <- list( rows = c("r1","r2","r3"), cols = c("c1","c2","c3","c4","c5") )
rowCopy( data,3,dnames )
#>     cols
#> rows c1 c2 c3 c4 c5
#>   r1  3  1  4  1  5
#>   r2  3  1  4  1  5
#>   r3  3  1  4  1  5
```
