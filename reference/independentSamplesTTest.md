# Independent samples t-test

Runs an independent-samples t-test and prints the results in a readable
format.

## Usage

``` r
independentSamplesTTest(
  formula,
  data = NULL,
  var.equal = FALSE,
  one.sided = FALSE,
  conf.level = 0.95
)
```

## Arguments

- formula:

  A formula of the form `outcome ~ group`, where `outcome` is the
  numeric variable being measured and `group` is a factor with exactly
  two levels.

- data:

  An optional data frame containing the variables named in `formula`.
  Tibbles are accepted and converted automatically. If `data` is omitted
  the variables are looked up in the workspace.

- var.equal:

  Set to `TRUE` to run Student's t-test, which assumes equal group
  variances. The default (`FALSE`) runs Welch's t-test, which is safer
  when variances may differ between groups.

- one.sided:

  Set to `FALSE` (default) for a two-sided test. Set to the name of the
  group expected to have the larger mean for a one-sided test (e.g.,
  `one.sided = "group2"`).

- conf.level:

  The confidence level for the confidence interval. The default is
  `0.95` for a 95% interval.

## Value

Prints a summary showing the outcome and grouping variable names, group
means and standard deviations, null and alternative hypotheses, test
results (t-statistic, degrees of freedom, p-value), a confidence
interval, and Cohen's d as a measure of effect size. The underlying
results are also returned as a list, so the output can be assigned to a
variable and inspected if needed.

## Details

Runs an independent-samples t-test comparing the means of two groups,
and prints the results in a beginner-friendly format. The calculations
are done by [`t.test`](https://rdrr.io/r/stats/t.test.html) and
[`cohensD`](https://lsr.djnavarro.net/reference/cohensD.md). When
`var.equal = TRUE`, Cohen's d uses a pooled standard deviation; when
`var.equal = FALSE` (Welch's test), it uses the "unequal" method. Cases
with missing values are removed with a warning.

## See also

[`t.test`](https://rdrr.io/r/stats/t.test.html),
[`oneSampleTTest`](https://lsr.djnavarro.net/reference/oneSampleTTest.md),
[`pairedSamplesTTest`](https://lsr.djnavarro.net/reference/pairedSamplesTTest.md),
[`cohensD`](https://lsr.djnavarro.net/reference/cohensD.md)

## Examples

``` r
df <- data.frame(
  rt = c(451, 562, 704, 324, 505, 600, 829),
  cond = factor(x = c(1, 1, 1, 2, 2, 2, 2), labels = c("group1", "group2"))
)

# Welch's t-test (the default, does not assume equal variances)
independentSamplesTTest(rt ~ cond, df)
#> 
#>    Welch's independent samples t-test 
#> 
#> Outcome variable:   rt 
#> Grouping variable:  cond 
#> 
#> Descriptive statistics: 
#>              group1  group2
#>    mean     572.333 564.500
#>    std dev. 126.816 210.239
#> 
#> Hypotheses: 
#>    null:        population means equal for both groups
#>    alternative: different population means in each group
#> 
#> Test results: 
#>    t-statistic:  0.061 
#>    degrees of freedom:  4.89 
#>    p-value:  0.954 
#> 
#> Other information: 
#>    two-sided 95% confidence interval:  [-323.703, 339.37] 
#>    estimated effect size (Cohen's d):  0.045 
#> 

# Student's t-test (assumes equal variances)
independentSamplesTTest(rt ~ cond, df, var.equal = TRUE)
#> 
#>    Student's independent samples t-test 
#> 
#> Outcome variable:   rt 
#> Grouping variable:  cond 
#> 
#> Descriptive statistics: 
#>              group1  group2
#>    mean     572.333 564.500
#>    std dev. 126.816 210.239
#> 
#> Hypotheses: 
#>    null:        population means equal for both groups
#>    alternative: different population means in each group
#> 
#> Test results: 
#>    t-statistic:  0.056 
#>    degrees of freedom:  5 
#>    p-value:  0.957 
#> 
#> Other information: 
#>    two-sided 95% confidence interval:  [-348.567, 364.234] 
#>    estimated effect size (Cohen's d):  0.043 
#> 

# one-sided test: is group1 larger?
independentSamplesTTest(rt ~ cond, df, one.sided = "group1")
#> 
#>    Welch's independent samples t-test 
#> 
#> Outcome variable:   rt 
#> Grouping variable:  cond 
#> 
#> Descriptive statistics: 
#>              group1  group2
#>    mean     572.333 564.500
#>    std dev. 126.816 210.239
#> 
#> Hypotheses: 
#>    null:        population means are equal, or smaller for group 'group1' 
#>    alternative: population mean is larger for group 'group1' 
#> 
#> Test results: 
#>    t-statistic:  0.061 
#>    degrees of freedom:  4.89 
#>    p-value:  0.477 
#> 
#> Other information: 
#>    one-sided 95% confidence interval:  [-251.588, Inf] 
#>    estimated effect size (Cohen's d):  0.045 
#> 

# missing values are removed with a warning
df$rt[1] <- NA
df$cond[7] <- NA
independentSamplesTTest(rt ~ cond, df)
#> Warning: 2 case(s) removed due to missingness
#> 
#>    Welch's independent samples t-test 
#> 
#> Outcome variable:   rt 
#> Grouping variable:  cond 
#> 
#> Descriptive statistics: 
#>              group1  group2
#>    mean     633.000 476.333
#>    std dev. 100.409 140.215
#> 
#> Hypotheses: 
#>    null:        population means equal for both groups
#>    alternative: different population means in each group
#> 
#> Test results: 
#>    t-statistic:  1.455 
#>    degrees of freedom:  2.867 
#>    p-value:  0.246 
#> 
#> Other information: 
#>    two-sided 95% confidence interval:  [-195.166, 508.499] 
#>    estimated effect size (Cohen's d):  1.285 
#> 
```
