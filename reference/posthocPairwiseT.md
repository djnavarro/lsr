# Post-hoc pairwise t-tests for ANOVA

Performs pairwise t-tests for an analysis of variance, making
corrections for multiple comparisons.

## Usage

``` r
posthocPairwiseT(x, ...)
```

## Arguments

- x:

  An `aov` object

- ...:

  Arguments to be passed to `pairwise.t.test`

## Value

As per `pairwise.t.test`

## Details

The intention behind this function is to allow users to use simple tools
for multiple corrections (e.g., Bonferroni, Holm) as post hoc
corrections in an ANOVA context, using the fitted model object (i.e., an
`aov` object) as the input. The reason for including this function is
that Tukey / Scheffe methods for constructing simultaneous confidence
intervals (as per [`TukeyHSD`](https://rdrr.io/r/stats/TukeyHSD.html))
are not often discussed in the context of an introductory class, and the
more powerful tools provided by the `multcomp` package are not
appropriate for students just beginning to learn statistics.

This function is currently just a wrapper function for
[`pairwise.t.test`](https://rdrr.io/r/stats/pairwise.t.test.html), and
it only works for one-way ANOVA, but this may change in future versions.

## See also

[`pairwise.t.test`](https://rdrr.io/r/stats/pairwise.t.test.html),
[`TukeyHSD`](https://rdrr.io/r/stats/TukeyHSD.html)

## Examples

``` r
# create the data set to analyse:
dataset <- data.frame(
  outcome = c( 1,2,3, 2,3,4, 5,6,7 ),
  group = factor(c( "a","a","a", "b","b","b","c","c","c"))
)

# run the ANOVA and print out the ANOVA table:
anova1 <- aov( outcome ~ group, data = dataset )
summary(anova1)
#>             Df Sum Sq Mean Sq F value  Pr(>F)   
#> group        2     26      13      13 0.00659 **
#> Residuals    6      6       1                   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Currently, the following two commands are equivalent:
posthocPairwiseT( anova1 )
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
pairwise.t.test( dataset$outcome, dataset$group )
#> 
#>  Pairwise comparisons using t tests with pooled SD 
#> 
#> data:  dataset$outcome and dataset$group 
#> 
#>   a      b     
#> b 0.2666 -     
#> c 0.0081 0.0208
#> 
#> P value adjustment method: holm 
```
