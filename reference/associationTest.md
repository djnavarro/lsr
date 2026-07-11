# Chi-square test of association / independence

Runs a chi-square test to check whether two categorical variables are
independent of one another.

## Usage

``` r
associationTest(formula, data = NULL)
```

## Arguments

- formula:

  A one-sided formula of the form `~var1 + var2`, specifying the two
  variables to be tested. Both variables must be factors.

- data:

  An optional data frame containing the variables named in `formula`. If
  omitted, the variables are looked up in the workspace.

## Value

Prints a summary of the test showing the variable names, null and
alternative hypotheses, observed and expected frequency tables, test
results (chi-square statistic, degrees of freedom, p-value), and
Cramer's V as a measure of effect size. The underlying results are also
returned as a list, so the output can be assigned to a variable and
inspected if needed.

## Details

The test checks whether two categorical variables are statistically
independent. Both variables must be factors, and the formula must be
one-sided with exactly two variables, e.g. `~gender + answer`.

Missing values are removed before the test is run, and a warning is
issued if any cases are dropped. When both variables have only two
levels, Yates' continuity correction is applied automatically.

## See also

[`chisq.test`](https://rdrr.io/r/stats/chisq.test.html),
[`goodnessOfFitTest`](https://lsr.djnavarro.net/reference/goodnessOfFitTest.md),
[`cramersV`](https://lsr.djnavarro.net/reference/cramersV.md)

## Examples

``` r
df <- data.frame(
  gender = factor(c("male", "male", "male", "male", "female", "female", "female")),
  answer = factor(c("heads", "heads", "heads", "heads", "tails", "tails", "heads"))
)

associationTest(~gender + answer, df)
#> Warning: Expected frequencies too small: chi-squared approximation may be incorrect
#> 
#>      Chi-square test of categorical association
#> 
#> Variables:   gender, answer 
#> 
#> Hypotheses: 
#>    null:        variables are independent of one another
#>    alternative: some contingency exists between variables
#> 
#> Observed contingency table:
#>         answer
#> gender   heads tails
#>   female     1     2
#>   male       4     0
#> 
#> Expected contingency table under the null hypothesis:
#>         answer
#> gender   heads tails
#>   female  2.14 0.857
#>   male    2.86 1.143
#> 
#> Test results: 
#>    X-squared statistic:  1.181 
#>    degrees of freedom:  1 
#>    p-value:  0.277 
#> 
#> Other information: 
#>    estimated effect size (Cramer's v):  0.411 
#>    Yates' continuity correction has been applied
#>    warning: expected frequencies too small, results may be inaccurate
#> 
```
