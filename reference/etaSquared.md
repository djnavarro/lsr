# Effect size calculations for ANOVAs

Calculates eta-squared and partial eta-squared

## Usage

``` r
etaSquared(x, type = 2, anova = FALSE)
```

## Arguments

- x:

  An analysis of variance (aov) object.

- type:

  What type of sum of squares to calculate?

- anova:

  Should the full ANOVA table be printed out in addition to the effect
  sizes?

## Value

If `anova=FALSE`, the output is an M x 2 matrix. Each of the M rows
corresponds to one of the terms in the ANOVA (e.g., main effect 1, main
effect 2, interaction, etc), and each of the columns corresponds to a
different measure of effect size. Column 1 contains the eta-squared
values, and column 2 contains partial eta-squared values. If
`anova=TRUE`, the output contains additional columns containing the sums
of squares, mean squares, degrees of freedom, F-statistics and p-values.

## Details

Calculates the eta-squared and partial eta-squared measures of effect
size that are commonly used in analysis of variance. The input `x`
should be the analysis of variance object itself.

For unbalanced designs, the default in `etaSquared` is to compute Type
II sums of squares (`type=2`), in keeping with the `Anova` function in
the `car` package. It is possible to revert to the Type I SS values
(`type=1`) to be consistent with `anova`, but this rarely tests
hypotheses of interest. Type III SS values (`type=3`) can also be
computed.

## Examples

``` r
# Example 1: one-way ANOVA

outcome <- c( 1.4,2.1,3.0,2.1,3.2,4.7,3.5,4.5,5.4 )  # data
treatment1 <- factor( c( 1,1,1,2,2,2,3,3,3 ))        # grouping variable
anova1 <- aov( outcome ~ treatment1 )                # run the ANOVA
summary( anova1 )                                    # print the ANOVA table
#>             Df Sum Sq Mean Sq F value Pr(>F)  
#> treatment1   2  7.936   3.968   3.663 0.0913 .
#> Residuals    6  6.500   1.083                 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
etaSquared( anova1 )                                 # effect size
#>               eta.sq eta.sq.part
#> treatment1 0.5497229   0.5497229

# Example 2: two-way ANOVA

treatment2 <- factor( c( 1,2,3,1,2,3,1,2,3 ))      # second grouping variable
anova2 <- aov( outcome ~ treatment1 + treatment2 ) # run the ANOVA
summary( anova2 )                                  # print the ANOVA table
#>             Df Sum Sq Mean Sq F value  Pr(>F)   
#> treatment1   2  7.936   3.968    55.8 0.00120 **
#> treatment2   2  6.216   3.108    43.7 0.00191 **
#> Residuals    4  0.284   0.071                   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
etaSquared( anova2 )                               # effect size
#>               eta.sq eta.sq.part
#> treatment1 0.5497229   0.9653961
#> treatment2 0.4305727   0.9562393
```
