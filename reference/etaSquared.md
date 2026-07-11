# Effect size for ANOVAs

Calculates eta-squared and partial eta-squared effect sizes for an
analysis of variance.

## Usage

``` r
etaSquared(x, type = 2, anova = FALSE)
```

## Arguments

- x:

  An `aov` object, as returned by
  [`aov`](https://rdrr.io/r/stats/aov.html).

- type:

  Which type of sums of squares to use: `1` for Type I, `2` for Type II
  (the default), or `3` for Type III. Type II is recommended for most
  unbalanced designs.

- anova:

  Set to `TRUE` to include the full ANOVA table alongside the effect
  sizes. Defaults to `FALSE`.

## Value

A matrix with one row per term in the ANOVA model and columns for
eta-squared (`eta.sq`) and partial eta-squared (`eta.sq.part`). If
`anova = TRUE`, additional columns show the sums of squares, mean
squares, degrees of freedom, F-statistics, and p-values.

## Details

Calculates eta-squared and partial eta-squared, two commonly used
measures of effect size in analysis of variance. The input `x` should be
an ANOVA fitted with [`aov`](https://rdrr.io/r/stats/aov.html).

For unbalanced designs, Type II sums of squares (`type = 2`) are
recommended and are the default, consistent with the `Anova` function in
the car package. Type I (`type = 1`) matches the output of
[`anova`](https://rdrr.io/r/stats/anova.html) but tests hypotheses that
are often not of interest in unbalanced designs. Type III (`type = 3`)
is also available.

## See also

[`aov`](https://rdrr.io/r/stats/aov.html),
[`summary.aov`](https://rdrr.io/r/stats/summary.aov.html)

## Examples

``` r
outcome <- c(1.4, 2.1, 3.0, 2.1, 3.2, 4.7, 3.5, 4.5, 5.4)
treatment1 <- factor(c(1, 1, 1, 2, 2, 2, 3, 3, 3))

# one-way ANOVA
anova1 <- aov(outcome ~ treatment1)
summary(anova1)
#>             Df Sum Sq Mean Sq F value Pr(>F)  
#> treatment1   2  7.936   3.968   3.663 0.0913 .
#> Residuals    6  6.500   1.083                 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
etaSquared(anova1)
#>               eta.sq eta.sq.part
#> treatment1 0.5497229   0.5497229

# include the full ANOVA table
etaSquared(anova1, anova = TRUE)
#>               eta.sq eta.sq.part       SS df       MS        F          p
#> treatment1 0.5497229   0.5497229 7.935556  2 3.967778 3.662564 0.09129344
#> Residuals  0.4502771          NA 6.500000  6 1.083333       NA         NA

# two-way ANOVA
treatment2 <- factor(c(1, 2, 3, 1, 2, 3, 1, 2, 3))
anova2 <- aov(outcome ~ treatment1 + treatment2)
etaSquared(anova2)
#>               eta.sq eta.sq.part
#> treatment1 0.5497229   0.9653961
#> treatment2 0.4305727   0.9562393
```
