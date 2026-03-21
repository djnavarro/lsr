# Expand factors to a set of contrasts

Substitutes all factors in a data frame with the set of contrasts with
which that factor is associated

## Usage

``` r
expandFactors(data, ...)
```

## Arguments

- data:

  A data frame.

- ...:

  Additional arguments to be passed to model.matrix

## Value

A data frame.

## Details

The `expandFactors` function replaces all of the factors in a data frame
with the set of contrasts output by the `contrasts` function or
`model.matrix`. It may be useful for teaching purposes when explaining
relationship between ANOVA and regression.

## Examples

``` r
grading <- data.frame( teacher = factor( c("Amy","Amy","Ben","Ben","Cat") ),
                       gender = factor( c("male","female","female","male","male") ),
                       grade = c(75,80,45,50,65) )

# expand factors using the default contrasts (usually treatment contrasts)
expandFactors( grading )
#>   teacherBen teacherCat gendermale grade
#> 1          0          0          1    75
#> 2          0          0          0    80
#> 3          1          0          0    45
#> 4          1          0          1    50
#> 5          0          1          1    65

# specify the contrasts using the contrasts.arg argument to model.matrix
my.contrasts <- list( teacher = "contr.helmert", gender = "contr.treatment" )
expandFactors( grading, contrasts.arg = my.contrasts )
#>   teacher1 teacher2 gendermale grade
#> 1       -1       -1          1    75
#> 2       -1       -1          0    80
#> 3        1       -1          0    45
#> 4        1       -1          1    50
#> 5        0        2          1    65
```
