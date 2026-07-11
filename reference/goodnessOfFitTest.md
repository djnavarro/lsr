# Chi-square goodness of fit test

Runs a chi-square goodness of fit test to check whether the observed
frequencies in a categorical variable match a set of hypothesised
probabilities.

## Usage

``` r
goodnessOfFitTest(x, p = NULL)
```

## Arguments

- x:

  A factor variable containing the observed outcomes.

- p:

  A numeric vector of hypothesised probabilities, one per level of `x`.
  The values must sum to 1. If named, the names must match the levels of
  `x` (order does not matter). If omitted, all outcomes are assumed to
  be equally likely.

## Value

Prints a summary of the test showing the variable name, null and
alternative hypotheses, a table of observed frequencies, expected
frequencies, and hypothesised probabilities, and the test results
(chi-square statistic, degrees of freedom, p-value). The underlying
results are also returned as a list, so the output can be assigned to a
variable and inspected if needed.

## Details

The test checks whether the observed frequencies for a categorical
variable are consistent with the probabilities specified in `p`.

Missing values in `x` are removed before the test is run, and a warning
is issued if any cases are dropped. If the probabilities in `p` do not
sum exactly to 1, they are rescaled with a warning.

If `x` has unused factor levels (levels with zero observed cases), a
warning is issued. Those levels are included in the test with zero
observed cases, which changes the degrees of freedom and may give
misleading results. Call
[`droplevels`](https://rdrr.io/r/base/droplevels.html) on the data first
if this is not intended.

## See also

[`chisq.test`](https://rdrr.io/r/stats/chisq.test.html),
[`associationTest`](https://lsr.djnavarro.net/reference/associationTest.md),
[`cramersV`](https://lsr.djnavarro.net/reference/cramersV.md)

## Examples

``` r
# raw data
gender <- factor(
  c(
    "male", "male", "male", "male", "female", "female",
    "female", "male", "male", "male"
  )
)

# goodness of fit test against the hypothesis that males and
# females occur with equal frequency
goodnessOfFitTest(gender)
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
goodnessOfFitTest(gender, p = c(.4, .6))
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
goodnessOfFitTest(gender, p = c(female = .4, male = .6))
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
goodnessOfFitTest(gender, p = c(male = .6, female = .4))
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
