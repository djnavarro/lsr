# Post-hoc pairwise t-tests for ANOVA

Runs pairwise t-tests for a one-way analysis of variance, with
corrections for multiple comparisons.

## Usage

``` r
posthocPairwiseT(x, ...)
```

## Arguments

- x:

  An `aov` object, as returned by
  [`aov`](https://rdrr.io/r/stats/aov.html). Only one-way ANOVA models
  are supported.

- ...:

  Additional arguments passed to
  [`pairwise.t.test`](https://rdrr.io/r/stats/pairwise.t.test.html),
  such as `p.adjust.method`.

## Value

Prints a table of p-values for all pairwise group comparisons. The
underlying result is also returned as a list (with the same structure as
[`pairwise.t.test`](https://rdrr.io/r/stats/pairwise.t.test.html)) so it
can be assigned to a variable and inspected if needed.

## Details

Takes a fitted one-way ANOVA object and runs pairwise t-tests for all
pairs of groups, applying a correction for multiple comparisons. This is
a simpler alternative to
[`TukeyHSD`](https://rdrr.io/r/stats/TukeyHSD.html) that uses the same
correction methods (e.g., Holm, Bonferroni) as
[`pairwise.t.test`](https://rdrr.io/r/stats/pairwise.t.test.html).

## See also

[`pairwise.t.test`](https://rdrr.io/r/stats/pairwise.t.test.html),
[`TukeyHSD`](https://rdrr.io/r/stats/TukeyHSD.html),
[`aov`](https://rdrr.io/r/stats/aov.html)

## Examples

``` r
dataset <- data.frame(
  outcome = c(1, 2, 3, 2, 3, 4, 5, 6, 7),
  group = factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c"))
)

anova1 <- aov(outcome ~ group, data = dataset)
summary(anova1)
#>             Df Sum Sq Mean Sq F value  Pr(>F)   
#> group        2     26      13      13 0.00659 **
#> Residuals    6      6       1                   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# post-hoc pairwise comparisons with Holm correction (the default)
posthocPairwiseT(anova1)
#> 
#>  Pairwise comparisons using t tests with pooled SD 
#> 
#> data:  outcome and group 
#> 
#>   a      b     
#> b 0.2666 -     
#> c 0.0081 0.0208
#> 
#> P value adjustment method: holm 

# Bonferroni correction instead
posthocPairwiseT(anova1, p.adjust.method = "bonferroni")
#> 
#>  Pairwise comparisons using t tests with pooled SD 
#> 
#> data:  outcome and group 
#> 
#>   a      b     
#> b 0.7997 -     
#> c 0.0081 0.0312
#> 
#> P value adjustment method: bonferroni 
```
