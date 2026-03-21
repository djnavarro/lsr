# Chi-square test of association / independence

Convenience function that runs a chi-square test of
association/independence. This is a wrapper function intended to be used
for pedagogical purposes only.

## Usage

``` r
associationTest(formula, data = NULL)
```

## Arguments

- formula:

  One-sided formula specifying the two variables (required).

- data:

  Optional data frame containing the variables.

## Value

An object of class 'assocTest'. When printed, the output is organised
into six short sections. The first section lists the name of the test
and the variables included. The second lists the null and alternative
hypotheses for the test. The third shows the observed contingency table,
and the fourth shows the expected contingency table under the null. The
fifth prints out the test results, and the sixth reports an estimate of
effect size.

## Details

The `associationTest` function runs the chi-square test of association
on the variables specified in the `formula` argument. The formula must
be a one-sided formula of the form `~variable1 + variable2`, and both
variables must be factors.

## Examples

``` r
df <- data.frame(
gender=factor(c("male","male","male","male","female","female","female")),
answer=factor(c("heads","heads","heads","heads","tails","tails","heads"))
)

associationTest( ~ gender + answer, df )
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
