# Sort a data frame

Sorts a data frame by one or more of its variables.

## Usage

``` r
sortFrame(x, ..., alphabetical = TRUE)
```

## Arguments

- x:

  A data frame to be sorted.

- ...:

  One or more unquoted variable names to sort by, in order of priority.
  Prefix a variable name with `-` to sort in descending order (e.g.,
  `sortFrame(x, -a, b)` sorts descending by `a`, then ascending by `b`).

- alphabetical:

  Set to `TRUE` (the default) to sort character variables
  case-insensitively in alphabetical order. Set to `FALSE` to use the
  locale-dependent ordering used by
  [`sort`](https://rdrr.io/r/base/sort.html).

## Value

The sorted data frame.

## Details

Sorts the rows of `x` by the variables listed in `...`. Numeric
variables and factors are sorted by their numeric values (for factors,
this corresponds to level order). Character variables are sorted
alphabetically by default, ignoring case; prefix with `-` for reverse
alphabetical order.

Simple expressions combining variables are also accepted (e.g.,
`sortFrame(x, a + b)` sorts by the sum of `a` and `b`), though care is
required: the sort uses [`xtfrm`](https://rdrr.io/r/base/xtfrm.html)
internally to convert variables to sortable numeric codes, which can
produce unexpected results for character variables when expressions
other than `-` are used.

## See also

[`order`](https://rdrr.io/r/base/order.html),
[`sort`](https://rdrr.io/r/base/sort.html),
[`xtfrm`](https://rdrr.io/r/base/xtfrm.html)

## Examples

``` r
dataset <- data.frame(
  txt = c("bob", "Clare", "clare", "bob", "eve", "eve"),
  num1 = c(3, 1, 2, 0, 0, 2),
  num2 = c(1, 1, 3, 0, 3, 2),
  stringsAsFactors = FALSE
)

sortFrame(dataset, num1) # sort by num1 ascending
#>     txt num1 num2
#> 4   bob    0    0
#> 5   eve    0    3
#> 2 Clare    1    1
#> 3 clare    2    3
#> 6   eve    2    2
#> 1   bob    3    1
sortFrame(dataset, num1, num2) # sort by num1 then num2
#>     txt num1 num2
#> 4   bob    0    0
#> 5   eve    0    3
#> 2 Clare    1    1
#> 6   eve    2    2
#> 3 clare    2    3
#> 1   bob    3    1
sortFrame(dataset, -num1) # sort by num1 descending
#>     txt num1 num2
#> 1   bob    3    1
#> 3 clare    2    3
#> 6   eve    2    2
#> 2 Clare    1    1
#> 4   bob    0    0
#> 5   eve    0    3
sortFrame(dataset, txt) # sort alphabetically (case-insensitive)
#>     txt num1 num2
#> 1   bob    3    1
#> 4   bob    0    0
#> 2 Clare    1    1
#> 3 clare    2    3
#> 5   eve    0    3
#> 6   eve    2    2
```
