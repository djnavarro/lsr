# Reshape from long to wide

Reshape a data frame from long form to wide form

## Usage

``` r
longToWide(data, formula, sep = "_")
```

## Arguments

- data:

  The data frame.

- formula:

  A two-sided formula specifying measure variables and within-subject
  variables

- sep:

  Separator string used in wide-form variable names

## Value

The output is a "wide form" data frame in containing one row per subject
(or experimental unit, more generally), with each observation of that
subject corresponding to a separate variable. The naming scheme for
these variables places the name of the measured variable first, followed
by the levels of within-subjects variable(s), separated by the separator
string `sep`. In the example above where the reshaping formula was
`accuracy ~ time`, if the default separator of `sep="_"` was used, and
the levels of the `time` variable are `t1`, `t2` and `t3`, then the
output would include the variables `accuracy_t1`, `accuracy_t2` and
`accuracy_t3`.

In the second example listed above, where the reshaping formula was
`rt + accuracy ~ days + sessions`, the output variables would refer to
levels of both within-subjects variables. For instance,
`rt_day1_session1`, and `accuracy_day2_session1` might be the names of
two of the variables in the wide form data frame.

## Details

The `longToWide` function is the companion function to `wideToLong`. The
`data` argument is a "long form" data frame, in which each row
corresponds to a single observation. The output is a "wide form" data
frame, in which each row corresponds to a single experimental unit
(e.g., a single subject).

The reshaping formula should list all of the measure variables on the
left hand side, and all of the within-subject variables on the right
hand side. All other variables are assumed to be between-subject
variables. For example, if the `accuracy` of a participant's performance
is measured at multiple `time` points, then the formula would be
`accuracy ~ time`.

Multiple variables are supported on both sides of the formula. For
example, suppose we measured the response time `rt` and `accuracy` of
participants, across three separate `days`, and across three separate
`sessions` within each day. In this case the formula would be
`rt + accuracy ~ days + sessions`.

## See also

[`wideToLong`](https://lsr.djnavarro.net/reference/wideToLong.md)

## Examples

``` r
long <- data.frame(
  id = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
  time = c("t1", "t1", "t1", "t2", "t2", "t2", "t3", "t3", "t3"),
  accuracy = c(.50, .03, .72, .94, .63, .49, .78, .71, .16)
)

longToWide(long, accuracy ~ time)
#>   id accuracy_t1 accuracy_t2 accuracy_t3
#> 1  1        0.50        0.94        0.78
#> 2  2        0.03        0.63        0.71
#> 3  3        0.72        0.49        0.16
```
