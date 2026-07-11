# One sample t-test

Convenience function that runs a one sample t-test. This is a wrapper
function intended to be used for pedagogical purposes only.

## Usage

``` r
oneSampleTTest(x, mu, one.sided = FALSE, conf.level = 0.95)
```

## Arguments

- x:

  The variable to be tested (required).

- mu:

  The value against which the mean should be tested (required).

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
confidence interval and an estimate of the effect size (i.e., Cohen's
d).

## Details

The `oneSampleTTest` function runs a one-sample t-test on the data in
`x`, and prints the results in a format that is easier for novices to
handle than the output of `t.test`. All the actual calculations are done
by the `t.test` and `cohensD` functions.

As with the `t.test` function, the default test is two sided,
corresponding to a default value of `one.sided = FALSE`. To specify a
one sided test in which the alternative hypothesis is that `x` is larger
than `mu`, the input must be `one.sided = "greater"`. Similarly, if
`one.sided="less"`, then the alternative hypothesis is that the mean of
`x` is smaller than `mu`.

## See also

[`t.test`](https://rdrr.io/r/stats/t.test.html),
[`pairedSamplesTTest`](https://lsr.djnavarro.net/reference/pairedSamplesTTest.md),
[`independentSamplesTTest`](https://lsr.djnavarro.net/reference/independentSamplesTTest.md),
[`cohensD`](https://lsr.djnavarro.net/reference/cohensD.md)

## Examples

``` r

likert <- c(3,1,4,1,4,6,7,2,6,6,7)

oneSampleTTest( x = likert, mu = 4 )
#> 
#>    One sample t-test 
#> 
#> Data variable:   likert 
#> 
#> Descriptive statistics: 
#>             likert
#>    mean      4.273
#>    std dev.  2.284
#> 
#> Hypotheses: 
#>    null:        population mean equals 4 
#>    alternative: population mean not equal to 4 
#> 
#> Test results: 
#>    t-statistic:  0.396 
#>    degrees of freedom:  10 
#>    p-value:  0.7 
#> 
#> Other information: 
#>    two-sided 95% confidence interval:  [2.738, 5.807] 
#>    estimated effect size (Cohen's d):  0.119 
#> 
oneSampleTTest( x = likert, mu = 4, one.sided = "greater" )
#> 
#>    One sample t-test 
#> 
#> Data variable:   likert 
#> 
#> Descriptive statistics: 
#>             likert
#>    mean      4.273
#>    std dev.  2.284
#> 
#> Hypotheses: 
#>    null:        population mean less than or equal to 4 
#>    alternative: population mean greater than 4 
#> 
#> Test results: 
#>    t-statistic:  0.396 
#>    degrees of freedom:  10 
#>    p-value:  0.35 
#> 
#> Other information: 
#>    one-sided 95% confidence interval:  [3.024, Inf] 
#>    estimated effect size (Cohen's d):  0.119 
#> 
oneSampleTTest( x = likert, mu = 4, conf.level=.99 )
#> 
#>    One sample t-test 
#> 
#> Data variable:   likert 
#> 
#> Descriptive statistics: 
#>             likert
#>    mean      4.273
#>    std dev.  2.284
#> 
#> Hypotheses: 
#>    null:        population mean equals 4 
#>    alternative: population mean not equal to 4 
#> 
#> Test results: 
#>    t-statistic:  0.396 
#>    degrees of freedom:  10 
#>    p-value:  0.7 
#> 
#> Other information: 
#>    two-sided 99% confidence interval:  [2.738, 5.807] 
#>    estimated effect size (Cohen's d):  0.119 
#> 

likert <- c(3,NA,4,NA,4,6,7,NA,6,6,7)
oneSampleTTest( x = likert, mu = 4 )
#> Warning: 3 case(s) removed due to missingness
#> 
#>    One sample t-test 
#> 
#> Data variable:   likert 
#> 
#> Descriptive statistics: 
#>             likert
#>    mean      5.375
#>    std dev.  1.506
#> 
#> Hypotheses: 
#>    null:        population mean equals 4 
#>    alternative: population mean not equal to 4 
#> 
#> Test results: 
#>    t-statistic:  2.582 
#>    degrees of freedom:  7 
#>    p-value:  0.036 
#> 
#> Other information: 
#>    two-sided 95% confidence interval:  [4.116, 6.634] 
#>    estimated effect size (Cohen's d):  0.913 
#> 
```
