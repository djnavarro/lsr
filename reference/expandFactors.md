# Expand factors to a set of contrasts

Replaces each factor variable in a data frame with its contrast-coded
columns, leaving numeric variables unchanged.

## Usage

``` r
expandFactors(data, ...)
```

## Arguments

- data:

  A data frame.

- ...:

  Additional arguments passed to
  [`model.matrix`](https://rdrr.io/r/stats/model.matrix.html), such as
  `contrasts.arg` for specifying non-default contrast schemes.

## Value

A data frame with factor columns replaced by numeric contrast columns.

## Details

Each factor in `data` is replaced by the numeric contrast columns that
[`model.matrix`](https://rdrr.io/r/stats/model.matrix.html) would
generate for that factor (using treatment contrasts by default). Numeric
variables pass through unchanged. This can be helpful when illustrating
the connection between ANOVA and regression.

## See also

[`model.matrix`](https://rdrr.io/r/stats/model.matrix.html),
[`contrasts`](https://rdrr.io/r/stats/contrasts.html)

## Examples

``` r
grading <- data.frame(
  teacher = factor(c("Amy", "Amy", "Ben", "Ben", "Cat")),
  gender  = factor(c("male", "female", "female", "male", "male")),
  grade   = c(75, 80, 45, 50, 65)
)

# expand using the default contrasts (treatment contrasts)
expandFactors(grading)
#>   teacherBen teacherCat gendermale grade
#> 1          0          0          1    75
#> 2          0          0          0    80
#> 3          1          0          0    45
#> 4          1          0          1    50
#> 5          0          1          1    65

# specify different contrasts via contrasts.arg
my.contrasts <- list(teacher = "contr.helmert", gender = "contr.treatment")
expandFactors(grading, contrasts.arg = my.contrasts)
#>   teacher1 teacher2 gendermale grade
#> 1       -1       -1          1    75
#> 2       -1       -1          0    80
#> 3        1       -1          0    45
#> 4        1       -1          1    50
#> 5        0        2          1    65
```
