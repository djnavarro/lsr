# Paired samples t-test

Convenience function that runs a paired samples t-test. This is a
wrapper function intended to be used for pedagogical purposes only.

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

  Formula specifying the outcome and the groups (required).

- data:

  Optional data frame containing the variables.

- id:

  The name of the id variable (must be a character string).

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

The `pairedSamplesTTest` function runs a paired-sample t-test, and
prints the results in a format that is easier for novices to handle than
the output of `t.test`. All the actual calculations are done by the
`t.test` and `cohensD` functions.

There are two different ways of specifying the formula, depending on
whether the data are in wide form or long form. If the data are in wide
form, then the input should be a one-sided formula of the form
`~ variable1 + variable2`. The `id` variable is not required: the first
element of `variable1` is paired with the first element of `variable2`
and so on. Both `variable1` and `variable2` must be numeric.

If the data are in long form, a two sided formula is required. The
simplest way to specify the test is to input a formula of the form
`outcome ~ group + (id)`. The term in parentheses is assumed to be the
`id` variable, and must be a factor. The `group` variable must be a
factor with two levels (if there are more than two levels but only two
are used in the data, a warning is given). The `outcome` variable must
be numeric.

The reason for using the `outcome ~ group + (id)` format is that it is
broadly consistent with the way repeated measures analyses are specified
in the `lme4` package. However, this format may not appeal to some
people for teaching purposes. Given this, the `pairedSamplesTTest` also
supports a simpler formula of the form `outcome ~ group`, so long as the
user specifies the `id` argument: this must be a character vector
specifying the name of the id variable

As with the `t.test` function, the default test is two sided,
corresponding to a default value of `one.sided = FALSE`. To specify a
one sided test, the `one.sided` argument must specify the name of the
factor level (long form data) or variable (wide form data) that is
hypothesised (under the alternative) to have the larger mean. For
instance, if the outcome at "time2" is expected to be higher than at
"time1", then the corresponding one sided test is specified by
`one.sided = "time2"`.

## See also

[`t.test`](https://rdrr.io/r/stats/t.test.html),
[`oneSampleTTest`](https://lsr.djnavarro.net/reference/oneSampleTTest.md),
[`independentSamplesTTest`](https://lsr.djnavarro.net/reference/independentSamplesTTest.md),
[`cohensD`](https://lsr.djnavarro.net/reference/cohensD.md)

## Examples

``` r
# long form data frame
df <- data.frame(
  id = factor( x=c(1, 1, 2, 2, 3, 3, 4, 4),
               labels=c("alice","bob","chris","diana") ),
  time = factor( x=c(1,2,1,2,1,2,1,2),
                 labels=c("time1","time2")),
  wm = c(3, 4, 6, 6, 9, 12,7,9)
)

# wide form
df2 <- longToWide( df, wm ~ time )

# basic test, run from long form or wide form data
pairedSamplesTTest( formula= wm ~ time, data=df, id="id" )
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
pairedSamplesTTest( formula= wm ~ time + (id), data=df )
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
pairedSamplesTTest( formula= ~wm_time1 + wm_time2, data=df2 )
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

# one sided test
pairedSamplesTTest( formula= wm~time, data=df, id="id", one.sided="time2" )
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

# missing data because of NA values
df$wm[1] <- NA
pairedSamplesTTest( formula= wm~time, data=df, id="id" )
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

# missing data because of missing cases from the long form data frame
df <- df[-1,]
pairedSamplesTTest( formula= wm~time, data=df, id="id" )
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
