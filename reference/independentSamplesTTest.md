# Independent samples t-test

Convenience function that runs an independent samples t-test. This is a
wrapper function intended to be used for pedagogical purposes only.

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

  Formula specifying the outcome and the groups (required).

- data:

  Optional data frame containing the variables.

- var.equal:

  Should the test assume equal variances (default = `FALSE`).

- one.sided:

  One sided or two sided hypothesis test (default = `FALSE`)

- conf.level:

  The confidence level for the confidence interval (default = .95).

## Value

An object of class 'TTest'. When printed, the output is organised into
five short sections. The first section lists the name of the test and
the variables included. The second provides means and standard
deviations. The third states explicitly what the null and alternative
hypotheses were. The fourth contains the test results: t-statistic,
degrees of freedom and p-value. The final section includes the relevant
confidence interval and an estimate of the effect size (i.e., Cohen's d)

## Details

The `independentSamplesTTest` function runs an independent-samples
t-test and prints the results in a format that is easier for novices to
handle than the output of `t.test`. All the actual calculations are done
by the `t.test` and `cohensD` functions. The `formula` argument must be
a two-sided formula of the form `outcome ~ group`. When
`var.equal=TRUE`, a Student's t-test is run and the estimate of Cohen's
d uses a pooled estimate of standard deviation. When `var.equal=FALSE`,
the Welch test is used, and the estimate of Cohen's d uses the "unequal"
method.

As with the `t.test` function, the default test is two sided,
corresponding to a default value of `one.sided = FALSE`. To specify a
one sided test, the `one.sided` argument must specify the name of the
factor level that is hypothesised (under the alternative) to have the
larger mean. For instance, if the outcome for "group2" is expected to be
higher than for "group1", then the corresponding one sided test is
specified by `one.sided = "group2"`.

## See also

[`t.test`](https://rdrr.io/r/stats/t.test.html),
[`oneSampleTTest`](https://lsr.djnavarro.net/reference/oneSampleTTest.md),
[`pairedSamplesTTest`](https://lsr.djnavarro.net/reference/pairedSamplesTTest.md),
[`cohensD`](https://lsr.djnavarro.net/reference/cohensD.md)

## Examples

``` r

df <- data.frame(
  rt = c(451, 562, 704, 324, 505, 600, 829),
  cond = factor( x=c(1,1,1,2,2,2,2), labels=c("group1","group2")))

# Welch t-test
independentSamplesTTest( rt ~ cond, df )
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

# Student t-test
independentSamplesTTest( rt ~ cond, df, var.equal=TRUE )
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

# one sided test
independentSamplesTTest( rt ~ cond, df, one.sided="group1" )
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

# missing data
df$rt[1] <- NA
df$cond[7] <- NA
independentSamplesTTest( rt ~ cond, df )
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
