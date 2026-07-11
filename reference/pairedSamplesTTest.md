# Paired samples t-test

Runs a paired-samples t-test and prints the results in a readable
format.

## Usage

``` r
pairedSamplesTTest(
  formula,
  data = NULL,
  id = NULL,
  one.sided = FALSE,
  conf.level = 0.95
)
```

## Arguments

- formula:

  A formula describing the data. For wide-format data use a one-sided
  formula such as `~ time1 + time2`. For long-format data use
  `outcome ~ group + (id)`, or `outcome ~ group` together with the `id`
  argument.

- data:

  An optional data frame containing the variables named in `formula`.
  Tibbles are accepted and converted automatically. If `data` is omitted
  the variables are looked up in the workspace.

- id:

  The name of the participant ID variable as a character string (e.g.,
  `id = "subject"`). Required when using long-format data with a plain
  `outcome ~ group` formula instead of `outcome ~ group + (id)`.

- one.sided:

  Set to `FALSE` (default) for a two-sided test. Set to the name of the
  group or variable expected to have the larger mean for a one-sided
  test (e.g., `one.sided = "time2"`).

- conf.level:

  The confidence level for the confidence interval. The default is
  `0.95` for a 95% interval.

## Value

Prints a summary showing the variable names, descriptive statistics
(including the mean and standard deviation of the differences), null and
alternative hypotheses, test results (t-statistic, degrees of freedom,
p-value), a confidence interval, and Cohen's d as a measure of effect
size. The underlying results are also returned as a list, so the output
can be assigned to a variable and inspected if needed.

## Details

Runs a paired-samples t-test and prints the results in a
beginner-friendly format. The calculations are done by
[`t.test`](https://rdrr.io/r/stats/t.test.html) and
[`cohensD`](https://lsr.djnavarro.net/reference/cohensD.md).

There are two ways to supply data. If the data are in **wide format**
(one row per participant, with the two measurements in separate
columns), use a one-sided formula such as `~ time1 + time2`. The first
row of `time1` is paired with the first row of `time2`, and so on.

If the data are in **long format** (two rows per participant), use a
two-sided formula. The recommended style is `outcome ~ group + (id)`,
where the participant ID variable is enclosed in parentheses.
Alternatively, use the plain formula `outcome ~ group` and supply the ID
variable name via the `id` argument. The lme4-style notation
`outcome ~ group + (1|id)` is also accepted as equivalent to
`outcome ~ group + (id)`.

Participants with missing measurements are removed with a warning.

## See also

[`t.test`](https://rdrr.io/r/stats/t.test.html),
[`oneSampleTTest`](https://lsr.djnavarro.net/reference/oneSampleTTest.md),
[`independentSamplesTTest`](https://lsr.djnavarro.net/reference/independentSamplesTTest.md),
[`cohensD`](https://lsr.djnavarro.net/reference/cohensD.md)

## Examples

``` r
# long-format data: one row per participant per time point
df <- data.frame(
  id = factor(
    x = c(1, 1, 2, 2, 3, 3, 4, 4),
    labels = c("alice", "bob", "chris", "diana")
  ),
  time = factor(
    x = c(1, 2, 1, 2, 1, 2, 1, 2),
    labels = c("time1", "time2")
  ),
  wm = c(3, 4, 6, 6, 9, 12, 7, 9)
)

# wide-format data: one row per participant
df2 <- longToWide(df, wm ~ time)

# three equivalent ways to run the same test
pairedSamplesTTest(formula = wm ~ time, data = df, id = "id")
#> 
#>    Paired samples t-test 
#> 
#> Outcome variable:   wm 
#> Grouping variable:  time 
#> ID variable:        id 
#> 
#> Descriptive statistics: 
#>             time1 time2 difference
#>    mean     6.250 7.750     -1.500
#>    std dev. 2.500 3.500      1.291
#> 
#> Hypotheses: 
#>    null:        population means equal for both measurements
#>    alternative: different population means for each measurement
#> 
#> Test results: 
#>    t-statistic:  -2.324 
#>    degrees of freedom:  3 
#>    p-value:  0.103 
#> 
#> Other information: 
#>    two-sided 95% confidence interval:  [-3.554, 0.554] 
#>    estimated effect size (Cohen's d):  1.162 
#> 
pairedSamplesTTest(formula = wm ~ time + (id), data = df)
#> 
#>    Paired samples t-test 
#> 
#> Outcome variable:   wm 
#> Grouping variable:  time 
#> ID variable:        id 
#> 
#> Descriptive statistics: 
#>             time1 time2 difference
#>    mean     6.250 7.750     -1.500
#>    std dev. 2.500 3.500      1.291
#> 
#> Hypotheses: 
#>    null:        population means equal for both measurements
#>    alternative: different population means for each measurement
#> 
#> Test results: 
#>    t-statistic:  -2.324 
#>    degrees of freedom:  3 
#>    p-value:  0.103 
#> 
#> Other information: 
#>    two-sided 95% confidence interval:  [-3.554, 0.554] 
#>    estimated effect size (Cohen's d):  1.162 
#> 
pairedSamplesTTest(formula = ~ wm_time1 + wm_time2, data = df2)
#> 
#>    Paired samples t-test 
#> 
#> Variables:  wm_time1 , wm_time2 
#> 
#> Descriptive statistics: 
#>             wm_time1 wm_time2 difference
#>    mean        6.250    7.750     -1.500
#>    std dev.    2.500    3.500      1.291
#> 
#> Hypotheses: 
#>    null:        population means equal for both measurements
#>    alternative: different population means for each measurement
#> 
#> Test results: 
#>    t-statistic:  -2.324 
#>    degrees of freedom:  3 
#>    p-value:  0.103 
#> 
#> Other information: 
#>    two-sided 95% confidence interval:  [-3.554, 0.554] 
#>    estimated effect size (Cohen's d):  1.162 
#> 

# one-sided test: is time2 larger than time1?
pairedSamplesTTest(formula = wm ~ time, data = df, id = "id", one.sided = "time2")
#> 
#>    Paired samples t-test 
#> 
#> Outcome variable:   wm 
#> Grouping variable:  time 
#> ID variable:        id 
#> 
#> Descriptive statistics: 
#>             time1 time2 difference
#>    mean     6.250 7.750     -1.500
#>    std dev. 2.500 3.500      1.291
#> 
#> Hypotheses: 
#>    null:        population means are equal, or smaller for measurement 'time2' 
#>    alternative: population mean is larger for measurement 'time2' 
#> 
#> Test results: 
#>    t-statistic:  -2.324 
#>    degrees of freedom:  3 
#>    p-value:  0.051 
#> 
#> Other information: 
#>    one-sided 95% confidence interval:  [-Inf, 0.019] 
#>    estimated effect size (Cohen's d):  1.162 
#> 

# missing value: that participant is removed with a warning
df$wm[1] <- NA
pairedSamplesTTest(formula = wm ~ time, data = df, id = "id")
#> Warning: 1 case(s) removed due to missingness
#> 
#>    Paired samples t-test 
#> 
#> Outcome variable:   wm 
#> Grouping variable:  time 
#> ID variable:        id 
#> 
#> Descriptive statistics: 
#>             time1 time2 difference
#>    mean     7.333 9.000     -1.667
#>    std dev. 1.528 3.000      1.528
#> 
#> Hypotheses: 
#>    null:        population means equal for both measurements
#>    alternative: different population means for each measurement
#> 
#> Test results: 
#>    t-statistic:  -1.89 
#>    degrees of freedom:  2 
#>    p-value:  0.199 
#> 
#> Other information: 
#>    two-sided 95% confidence interval:  [-5.461, 2.128] 
#>    estimated effect size (Cohen's d):  1.091 
#> 

# missing row: that participant is also removed with a warning
df <- df[-1, ]
pairedSamplesTTest(formula = wm ~ time, data = df, id = "id")
#> Warning: 1 case(s) removed due to missingness
#> 
#>    Paired samples t-test 
#> 
#> Outcome variable:   wm 
#> Grouping variable:  time 
#> ID variable:        id 
#> 
#> Descriptive statistics: 
#>             time1 time2 difference
#>    mean     7.333 9.000     -1.667
#>    std dev. 1.528 3.000      1.528
#> 
#> Hypotheses: 
#>    null:        population means equal for both measurements
#>    alternative: different population means for each measurement
#> 
#> Test results: 
#>    t-statistic:  -1.89 
#>    degrees of freedom:  2 
#>    p-value:  0.199 
#> 
#> Other information: 
#>    two-sided 95% confidence interval:  [-5.461, 2.128] 
#>    estimated effect size (Cohen's d):  1.091 
#> 
```
