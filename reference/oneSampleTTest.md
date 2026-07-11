# One sample t-test

Runs a one-sample t-test and prints the results in a readable format.

## Usage

``` r
oneSampleTTest(x, mu, one.sided = FALSE, conf.level = 0.95)
```

## Arguments

- x:

  A numeric vector containing the data to be tested.

- mu:

  The hypothesised population mean to test against.

- one.sided:

  Set to `FALSE` (default) for a two-sided test. Set to `"greater"` if
  you expect the population mean to be above `mu`, or `"less"` if you
  expect it to be below.

- conf.level:

  The confidence level for the confidence interval. The default is
  `0.95` for a 95% interval.

## Value

Prints a summary showing the variable name, descriptive statistics, null
and alternative hypotheses, test results (t-statistic, degrees of
freedom, p-value), a confidence interval, and Cohen's d as a measure of
effect size. The underlying results are also returned as a list, so the
output can be assigned to a variable and inspected if needed.

## Details

Runs a one-sample t-test comparing the mean of `x` to the hypothesised
value `mu`, and prints the results in a beginner-friendly format. The
calculations are done by [`t.test`](https://rdrr.io/r/stats/t.test.html)
and [`cohensD`](https://lsr.djnavarro.net/reference/cohensD.md). Missing
values in `x` are removed with a warning.

## See also

[`t.test`](https://rdrr.io/r/stats/t.test.html),
[`pairedSamplesTTest`](https://lsr.djnavarro.net/reference/pairedSamplesTTest.md),
[`independentSamplesTTest`](https://lsr.djnavarro.net/reference/independentSamplesTTest.md),
[`cohensD`](https://lsr.djnavarro.net/reference/cohensD.md)

## Examples

``` r
likert <- c(3, 1, 4, 1, 4, 6, 7, 2, 6, 6, 7)

# two-sided test (the default)
oneSampleTTest(x = likert, mu = 4)
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

# one-sided test: is the mean greater than 4?
oneSampleTTest(x = likert, mu = 4, one.sided = "greater")
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

# wider confidence interval
oneSampleTTest(x = likert, mu = 4, conf.level = 0.99)
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
#>    two-sided 99% confidence interval:  [2.09, 6.456] 
#>    estimated effect size (Cohen's d):  0.119 
#> 

# missing values are removed with a warning
likert <- c(3, NA, 4, NA, 4, 6, 7, NA, 6, 6, 7)
oneSampleTTest(x = likert, mu = 4)
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
