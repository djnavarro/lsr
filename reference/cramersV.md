# Cramer's V

Calculates Cramer's V, a measure of the strength of association for
chi-square tests.

## Usage

``` r
cramersV(...)
```

## Arguments

- ...:

  Arguments passed to
  [`chisq.test`](https://rdrr.io/r/stats/chisq.test.html), in the same
  format accepted by that function. The `correct` argument is always set
  to `FALSE` internally and cannot be overridden.

## Value

A single number giving the value of Cramer's V.

## Details

Cramer's V summarises the strength of association from a chi-square
test. It is appropriate for both tests of association (two categorical
variables) and goodness of fit tests (one variable versus hypothesised
probabilities). Values range from 0 (no association) to 1 (perfect
association).

Yates' continuity correction is never applied, regardless of the table
dimensions. This is intentional: applying the correction reduces the
chi-squared statistic, which causes V to fall below 1 even for perfectly
associated 2x2 tables — inconsistent with its definition as an effect
size on the \[0, 1\] scale.

## See also

[`chisq.test`](https://rdrr.io/r/stats/chisq.test.html),
[`associationTest`](https://lsr.djnavarro.net/reference/associationTest.md),
[`goodnessOfFitTest`](https://lsr.djnavarro.net/reference/goodnessOfFitTest.md)

## Examples

``` r
# frequency table for two groups, each choosing from three options
condition1 <- c(30, 20, 50)
condition2 <- c(35, 30, 35)
X <- cbind(condition1, condition2)
rownames(X) <- c("choice1", "choice2", "choice3")

# chi-square test of association
chisq.test(X)
#> 
#>  Pearson's Chi-squared test
#> 
#> data:  X
#> X-squared = 5.0317, df = 2, p-value = 0.0808
#> 

# effect size estimate
cramersV(X)
#> [1] 0.1586139
```
