# Sort a data frame

Sorts a data frame using one or more variables.

## Usage

``` r
sortFrame(x, ..., alphabetical = TRUE)
```

## Arguments

- x:

  Data frame to be sorted

- ...:

  A list of sort terms (see below)

- alphabetical:

  Should character vectors be sorted alphabetically?

## Value

The sorted data frame

## Details

The simplest use of this function is to sort a data frame `x` in terms
of one or more of the variables it contains. If for instance, the data
frame `x` contains two variables `a` and `b`, then the command
`sortFrame(x,a,b)` sorts by variable `a`, breaking ties using variable
`b`. Numeric variables are sorted in ascending order: to sort in
descending order of `a` and then ascending order of `b`, use the command
`sortFrame(x,-a,b)`. Factors are treated as numeric variables, and are
sorted by the internal codes (i.e., the first factor level equals 1, the
second factor levels equals 2 and so on). Character vectors are sorted
in alphabetical order, which differs from the ordering used by the
[`sort`](https://rdrr.io/r/base/sort.html) function; to use the default
'ascii' ordering, specify `alphabetical=FALSE`. Minus signs can be used
in conjunction with character vectors in order to sort in reverse
alphabetical order. If `c` represents a character variable, then
`sortFrame(x,c)` sorts in alphabetical order, whereas `sortFrame(x,-c)`
sorts in reverse alphabetical order.

It is also possible to specify more complicated sort terms by including
expressions using multiple variables within a single term, but care is
required. For instance, it is possible to sort the data frame by the sum
of two variables, using the command `sortFrame(x, a+b)`. For numeric
variables expressions of this kind should work in the expected manner,
but this is not always the case for non-numeric variables: `sortFrame`
uses the [`xtfrm`](https://rdrr.io/r/base/xtfrm.html) function to
provide, for every variable referred to in the list of sort terms
(`...`) a numeric vector that sorts in the same order as the original
variable. This reliance is what makes reverse alphabetical order (e.g.,
`sortFrame(x,-c)`) work. However, it also means that it is possible to
specify somewhat nonsensical sort terms for character vectors by abusing
the numerical coding (e.g. `sortFrame(x,(c-3)^2)`; see the examples
section). It also means that sorting in terms of string operation
functions (e.g., `nchar`) do not work as expected. See examples section.
Future versions of `sortFrame` will (hopefully) address this, possibly
by allowing the user to "switch off" the internal use of `xtfrm`, or
else by allowing [`AsIs`](https://rdrr.io/r/base/AsIs.html) expressions
to be used in sort terms.

## See also

[`sort`](https://rdrr.io/r/base/sort.html),
[`order`](https://rdrr.io/r/base/order.html),
[`xtfrm`](https://rdrr.io/r/base/xtfrm.html)

## Examples

``` r
txt <- c("bob","Clare","clare","bob","eve","eve")
num1 <- c(3,1,2,0,0,2)
num2 <- c(1,1,3,0,3,2)
etc <- c("not","used","as","a","sort","term")
dataset <- data.frame( txt, num1, num2, etc, stringsAsFactors=FALSE )

sortFrame( dataset, num1 )
#>     txt num1 num2  etc
#> 4   bob    0    0    a
#> 5   eve    0    3 sort
#> 2 Clare    1    1 used
#> 3 clare    2    3   as
#> 6   eve    2    2 term
#> 1   bob    3    1  not
sortFrame( dataset, num1, num2 )
#>     txt num1 num2  etc
#> 4   bob    0    0    a
#> 5   eve    0    3 sort
#> 2 Clare    1    1 used
#> 6   eve    2    2 term
#> 3 clare    2    3   as
#> 1   bob    3    1  not
sortFrame( dataset, txt )
#>     txt num1 num2  etc
#> 1   bob    3    1  not
#> 4   bob    0    0    a
#> 2 Clare    1    1 used
#> 3 clare    2    3   as
#> 5   eve    0    3 sort
#> 6   eve    2    2 term
```
