# Sample mode

Calculate the mode of a sample: both modal value(s) and the
corresponding frequency

## Usage

``` r
maxFreq(x, na.rm = TRUE)

modeOf(x, na.rm = TRUE)
```

## Arguments

- x:

  A vector containing the observations.

- na.rm:

  Logical value indicating whether NA values should be removed.

## Value

The `modeOf` function returns the mode of `x`. If there are ties, it
returns a vector containing all values of `x` that have the modal
frequency. The `maxFreq` function returns the modal frequency as a
numeric value.

## Details

These two functions can be used to calculate the mode (most frequently
observed value) of a sample, and the actual frequency of the modal
value. The only complication is in respect to missing data. If
`na.rm = FALSE`, then there are multiple possibilities for how to
calculate the mode. One possibility is to treat `NA` as another possible
value for the elements of `x`, and therefore if `NA` is more frequent
than any other value, then `NA` is the mode; and the modal frequency is
equal to the number of missing values. This is the version that is
currently implemented.

Another possibility is to treat `NA` as meaning "true value unknown",
and to the mode of `x` is itself known only if the number of missing
values is small enough that – regardless of what value they have – they
cannot alter the sample mode. For instance, if `x` were
`c(1,1,1,1,2,2,NA)`, we know that the mode of `x` is `1` regardless of
what the true value is for the one missing datum; and we know that the
modal frequency is between 4 and 5. This is also a valid interpretation,
depending on what precisely it is the user wants, but is not currently
implemented.

Because of the ambiguity of how `na.rm = FALSE` should be interpreted,
the default value has been set to `na.rm = TRUE`, which differs from the
default value used elsewhere in the package.

## See also

[`mean`](https://rdrr.io/r/base/mean.html),
[`median`](https://rdrr.io/r/stats/median.html),
[`table`](https://rdrr.io/r/base/table.html)

## Examples

``` r
# simple example
eyes <- c("green","green","brown","brown","blue")
modeOf(eyes)
#> [1] "green" "brown"
maxFreq(eyes)
#> [1] 2

# vector with missing data
eyes <- c("green","green","brown","brown","blue",NA,NA,NA)

# returns NA as the modal value.
modeOf(eyes, na.rm = FALSE)
#> [1] NA
maxFreq(eyes, na.rm = FALSE)
#> [1] 3

# returns c("green", "brown") as the modes, as before
modeOf(eyes, na.rm = TRUE)
#> [1] "green" "brown"
maxFreq(eyes, na.rm = TRUE)
#> [1] 2
```
