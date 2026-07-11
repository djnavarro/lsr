# Sample mode

Calculate the most frequently occurring value(s) in a sample (`modeOf`)
or the frequency of the most common value (`maxFreq`).

## Usage

``` r
modeOf(x, na.rm = TRUE)

maxFreq(x, na.rm = TRUE)
```

## Arguments

- x:

  A vector or factor containing the observations.

- na.rm:

  Set to `TRUE` (the default) to remove missing values before computing
  the mode. Set to `FALSE` to treat `NA` as a possible modal value (see
  Details).

## Value

`modeOf` returns the most frequently observed value. If multiple values
are tied for the highest frequency, all of them are returned as a
vector. If the input has no non-missing values, `modeOf` issues a
warning and returns `NA`. `maxFreq` returns the modal frequency as a
single number, or `NA` with a warning if the input has no non-missing
values.

## Details

When `na.rm = FALSE`, missing values are treated as a distinct value
that can itself be the mode. If the number of `NA`s exceeds the
frequency of every other value, `modeOf` returns `NA` and `maxFreq`
returns the count of missing values.

Because of this ambiguity, the default is `na.rm = TRUE`, unlike most
other functions in this package.

## See also

[`mean`](https://rdrr.io/r/base/mean.html),
[`median`](https://rdrr.io/r/stats/median.html),
[`table`](https://rdrr.io/r/base/table.html)

## Examples

``` r
eyes <- c("green", "green", "brown", "brown", "blue")
modeOf(eyes) # returns c("green", "brown") -- a tie
#> [1] "green" "brown"
maxFreq(eyes) # returns 2
#> [1] 2

# with missing data
eyes <- c("green", "green", "brown", "brown", "blue", NA, NA, NA)

# na.rm = FALSE: NA is the most frequent "value"
modeOf(eyes, na.rm = FALSE)
#> [1] NA
maxFreq(eyes, na.rm = FALSE)
#> [1] 3

# na.rm = TRUE: missing values ignored
modeOf(eyes, na.rm = TRUE)
#> [1] "green" "brown"
maxFreq(eyes, na.rm = TRUE)
#> [1] 2
NULL
#> NULL
```
