# Reshape from long to wide

Reshapes a data frame from long form (one row per observation) to wide
form (one row per subject), using a formula to specify the structure.

## Usage

``` r
longToWide(data, formula, sep = "_")
```

## Arguments

- data:

  A long-form data frame with one row per observation.

- formula:

  A two-sided formula of the form `measure ~ within`, listing the
  measured variable(s) on the left and the within-subject variable(s) on
  the right. All other variables in `data` are treated as
  between-subject variables. Multiple variables are supported on each
  side, e.g. `rt + accuracy ~ day + session`.

- sep:

  The separator string used to construct wide-form variable names.
  Defaults to `"_"`. For example, with `sep = "_"` and a measure called
  `accuracy` at levels `t1` and `t2`, the output columns are named
  `accuracy_t1` and `accuracy_t2`.

## Value

A wide-form data frame with one row per subject (or experimental unit).
Column names for the repeated measures follow the naming convention used
by [`wideToLong`](https://lsr.djnavarro.net/reference/wideToLong.md):
the measure name followed by the within-subject factor level(s),
separated by `sep`.

## Details

This function is the companion to
[`wideToLong`](https://lsr.djnavarro.net/reference/wideToLong.md). It
reshapes a long-form data frame into wide form by spreading the
within-subject observations across columns, with column names
constructed from the measure name and factor level(s) joined by `sep`.

## See also

[`wideToLong`](https://lsr.djnavarro.net/reference/wideToLong.md),
[`reshape`](https://rdrr.io/r/stats/reshape.html)

## Examples

``` r
long <- data.frame(
  id       = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
  time     = c("t1", "t1", "t1", "t2", "t2", "t2", "t3", "t3", "t3"),
  accuracy = c(.50, .03, .72, .94, .63, .49, .78, .71, .16)
)

longToWide(long, accuracy ~ time)
#>   id accuracy_t1 accuracy_t2 accuracy_t3
#> 1  1        0.50        0.94        0.78
#> 2  2        0.03        0.63        0.71
#> 3  3        0.72        0.49        0.16
```
