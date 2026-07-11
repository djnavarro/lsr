# Copy a vector into a matrix

Creates a matrix by stacking multiple copies of a vector as rows
(`rowCopy`) or as columns (`colCopy`).

## Usage

``` r
colCopy(x, times, dimnames = NULL)

rowCopy(x, times, dimnames = NULL)
```

## Arguments

- x:

  A vector to be copied.

- times:

  The number of copies to stack together.

- dimnames:

  An optional list specifying row and column names for the output
  matrix.

## Value

For `rowCopy`, a matrix with `times` rows and `length(x)` columns, where
each row is `x`. For `colCopy`, a matrix with `length(x)` rows and
`times` columns, where each column is `x`.

## Details

These functions are shortcuts for building a matrix where every row (or
every column) is the same vector. They are equivalent to calling
[`matrix`](https://rdrr.io/r/base/matrix.html) with appropriate `byrow`
and dimension arguments.

## See also

[`matrix`](https://rdrr.io/r/base/matrix.html),
[`rbind`](https://rdrr.io/r/base/cbind.html),
[`cbind`](https://rdrr.io/r/base/cbind.html)

## Examples

``` r
x <- c(3, 1, 4, 1, 5)

# stack x as rows
rowCopy(x, 4)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    3    1    4    1    5
#> [2,]    3    1    4    1    5
#> [3,]    3    1    4    1    5
#> [4,]    3    1    4    1    5

# stack x as columns
colCopy(x, 4)
#>      [,1] [,2] [,3] [,4]
#> [1,]    3    3    3    3
#> [2,]    1    1    1    1
#> [3,]    4    4    4    4
#> [4,]    1    1    1    1
#> [5,]    5    5    5    5

# with custom dimension names
dnames <- list(rows = c("r1", "r2", "r3"), cols = c("c1", "c2", "c3", "c4", "c5"))
rowCopy(x, 3, dnames)
#>     cols
#> rows c1 c2 c3 c4 c5
#>   r1  3  1  4  1  5
#>   r2  3  1  4  1  5
#>   r3  3  1  4  1  5
```
