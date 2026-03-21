# Chi-square test against specified probabilities

Convenience function that runs a chi-square goodness of fit test against
specified probabilities. This is a wrapper function intended to be used
for pedagogical purposes only.

## Usage

``` r
goodnessOfFitTest(x, p = NULL)
```

## Arguments

- x:

  Factor variable containing the raw outcomes.

- p:

  Numeric variable containing the null-hypothesis probabilities (default
  = all outcomes equally likely)

## Value

An object of class 'gofTest'. When printed, the output is organised into
four short sections. The first section lists the name of the test and
the variables included. The second lists the null and alternative
hypotheses for the test. The third shows the observed frequency table,
the expected frequency table under the null hypothesis, and the
probabilities specified by the null. The fourth prints out the test
results.

## Details

The `goodnessOfFitTest` function runs the chi-square goodness of fit
test of the hypothesis that the outcomes in the factor `x` were
generated according to the probabilities in the vector `p`. The
probability vector `p` must be a numeric variable of length
`nlevels(x)`. If no probabilities are specified, all outcomes are
assumed to be equally likely.

## See also

[`chisq.test`](https://rdrr.io/r/stats/chisq.test.html),
[`associationTest`](https://lsr.djnavarro.net/reference/associationTest.md),
[`cramersV`](https://lsr.djnavarro.net/reference/cramersV.md)

## Examples

``` r
# raw data
gender <- factor(
  c( "male","male","male","male","female","female",
     "female","male","male","male" ))

# goodness of fit test against the hypothesis that males and
# females occur with equal frequency
goodnessOfFitTest( gender )
#> 
#>      Chi-square test against specified probabilities
#> 
#> Data variable:   gender 
#> 
#> Hypotheses: 
#>    null:        true probabilities are as specified
#>    alternative: true probabilities differ from those specified
#> 
#> Descriptives: 
#>        observed freq. expected freq. specified prob.
#> female              3              5             0.5
#> male                7              5             0.5
#> 
#> Test results: 
#>    X-squared statistic:  1.6 
#>    degrees of freedom:  1 
#>    p-value:  0.206 
#> 

# goodness of fit test against the hypothesis that males appear
# with probability .6 and females with probability .4.
goodnessOfFitTest( gender, p=c(.4,.6) )
#> Warning: Expected frequencies too small: chi-squared approximation may be incorrect
#> 
#>      Chi-square test against specified probabilities
#> 
#> Data variable:   gender 
#> 
#> Hypotheses: 
#>    null:        true probabilities are as specified
#>    alternative: true probabilities differ from those specified
#> 
#> Descriptives: 
#>        observed freq. expected freq. specified prob.
#> female              3              4             0.4
#> male                7              6             0.6
#> 
#> Test results: 
#>    X-squared statistic:  0.417 
#>    degrees of freedom:  1 
#>    p-value:  0.519 
#>    warning: expected frequencies too small, results may be inaccurate
#> 
goodnessOfFitTest( gender, p=c(female=.4,male=.6) )
#> Warning: Expected frequencies too small: chi-squared approximation may be incorrect
#> 
#>      Chi-square test against specified probabilities
#> 
#> Data variable:   gender 
#> 
#> Hypotheses: 
#>    null:        true probabilities are as specified
#>    alternative: true probabilities differ from those specified
#> 
#> Descriptives: 
#>        observed freq. expected freq. specified prob.
#> female              3              4             0.4
#> male                7              6             0.6
#> 
#> Test results: 
#>    X-squared statistic:  0.417 
#>    degrees of freedom:  1 
#>    p-value:  0.519 
#>    warning: expected frequencies too small, results may be inaccurate
#> 
goodnessOfFitTest( gender, p=c(male=.6,female=.4) )
#> Warning: Expected frequencies too small: chi-squared approximation may be incorrect
#> 
#>      Chi-square test against specified probabilities
#> 
#> Data variable:   gender 
#> 
#> Hypotheses: 
#>    null:        true probabilities are as specified
#>    alternative: true probabilities differ from those specified
#> 
#> Descriptives: 
#>        observed freq. expected freq. specified prob.
#> female              3              4             0.4
#> male                7              6             0.6
#> 
#> Test results: 
#>    X-squared statistic:  0.417 
#>    degrees of freedom:  1 
#>    p-value:  0.519 
#>    warning: expected frequencies too small, results may be inaccurate
#> 
```
