# Reshape from wide to long

Reshapes a data frame from wide form (one row per subject) to long form
(one row per observation), using variable names to determine the
structure.

## Usage

``` r
wideToLong(data, within = "within", sep = "_", split = TRUE)
```

## Arguments

- data:

  A wide-form data frame with one row per subject (or experimental
  unit). Variables whose names contain `sep` are treated as repeated
  measures; all others are treated as between-subject variables.

- within:

  A character string, or vector of strings, giving the name(s) to use
  for the within-subject factor column(s) in the output. Defaults to
  `"within"`.

- sep:

  The separator string used in the wide-form variable names to separate
  the measure name from the factor level(s). Defaults to `"_"`. The
  separator must not appear anywhere else in the variable names.

- split:

  Set to `TRUE` (the default) to split multiple within-subject factors
  into separate columns in the output. Set to `FALSE` to keep them
  combined into a single column.

## Value

A long-form data frame with one row per observation.

## Details

This function is the companion to
[`longToWide`](https://lsr.djnavarro.net/reference/longToWide.md). It
determines the reshape structure from the variable names rather than
requiring an explicit formula.

The naming scheme for repeated-measures variables places the measure
name first, followed by the factor level(s), all joined by `sep`. For
example, variables named `accuracy_t1` and `accuracy_t2` indicate a
measure called `accuracy` recorded at two time points (`t1` and `t2`).
After reshaping, the long-form output contains one column called
`accuracy` and a factor column (named by the `within` argument) with
levels `t1` and `t2`.

Designs with multiple within-subject factors are supported. For example,
`MRT_cond1_day1` encodes measure `MRT` at level `cond1` of one factor
and `day1` of another. Supply `within = c("condition", "day")` to name
both output columns. Multiple measured variables per observation (e.g.,
both `MRT` and `PC`) are also supported.

## See also

[`longToWide`](https://lsr.djnavarro.net/reference/longToWide.md),
[`reshape`](https://rdrr.io/r/stats/reshape.html)

## Examples

``` r
# simple design: accuracy measured at two time points for 4 participants
wide <- data.frame(
  id          = 1:4,
  accuracy_t1 = c(.15, .50, .78, .55),
  accuracy_t2 = c(.55, .32, .99, .60)
)
wideToLong(wide, "time")
#>   id time accuracy
#> 1  1   t1     0.15
#> 2  2   t1     0.50
#> 3  3   t1     0.78
#> 4  4   t1     0.55
#> 5  1   t2     0.55
#> 6  2   t2     0.32
#> 7  3   t2     0.99
#> 8  4   t2     0.60

# complex design: two measures (MRT, PC), two conditions, two days
wide2 <- data.frame(
  id             = 1:4,
  gender         = factor(c("male", "male", "female", "female")),
  MRT_cond1_day1 = c(415, 500, 478, 550),
  MRT_cond2_day1 = c(455, 532, 499, 602),
  MRT_cond1_day2 = c(400, 490, 468, 502),
  MRT_cond2_day2 = c(450, 518, 474, 588),
  PC_cond1_day1  = c(79, 83, 91, 75),
  PC_cond2_day1  = c(82, 86, 90, 78),
  PC_cond1_day2  = c(88, 92, 98, 89),
  PC_cond2_day2  = c(93, 97, 100, 95)
)

# default: condition and day become separate columns
wideToLong(wide2, within = c("condition", "day"))
#>    id gender MRT  PC condition  day
#> 1   1   male 415  79     cond1 day1
#> 2   2   male 500  83     cond1 day1
#> 3   3 female 478  91     cond1 day1
#> 4   4 female 550  75     cond1 day1
#> 5   1   male 455  82     cond2 day1
#> 6   2   male 532  86     cond2 day1
#> 7   3 female 499  90     cond2 day1
#> 8   4 female 602  78     cond2 day1
#> 9   1   male 400  88     cond1 day2
#> 10  2   male 490  92     cond1 day2
#> 11  3 female 468  98     cond1 day2
#> 12  4 female 502  89     cond1 day2
#> 13  1   male 450  93     cond2 day2
#> 14  2   male 518  97     cond2 day2
#> 15  3 female 474 100     cond2 day2
#> 16  4 female 588  95     cond2 day2

# alternative: keep condition and day as one combined column
wideToLong(wide2, split = FALSE)
#>    id gender     within MRT  PC
#> 1   1   male cond1_day1 415  79
#> 2   2   male cond1_day1 500  83
#> 3   3 female cond1_day1 478  91
#> 4   4 female cond1_day1 550  75
#> 5   1   male cond2_day1 455  82
#> 6   2   male cond2_day1 532  86
#> 7   3 female cond2_day1 499  90
#> 8   4 female cond2_day1 602  78
#> 9   1   male cond1_day2 400  88
#> 10  2   male cond1_day2 490  92
#> 11  3 female cond1_day2 468  98
#> 12  4 female cond1_day2 502  89
#> 13  1   male cond2_day2 450  93
#> 14  2   male cond2_day2 518  97
#> 15  3 female cond2_day2 474 100
#> 16  4 female cond2_day2 588  95
```
