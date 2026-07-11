# Standardised regression coefficients

Calculates standardised regression coefficients (beta weights) for a
linear model.

## Usage

``` r
standardCoefs(x)
```

## Arguments

- x:

  A linear model, as returned by
  [`lm`](https://rdrr.io/r/stats/lm.html).

## Value

A matrix with one row per predictor (excluding the intercept) and two
columns: `b` (unstandardised coefficient) and `beta` (standardised
coefficient).

## Details

Standardised coefficients are the regression coefficients that would
result from fitting the model after scaling all predictors and the
outcome to have mean 0 and variance 1. They can be useful for comparing
the relative magnitude of predictors measured on different scales,
though this comparison should be interpreted with care.

Note that when a model contains interaction terms, the interaction
column is also standardised as a whole, rather than being constructed
from standardised versions of the constituent predictors.

## See also

[`lm`](https://rdrr.io/r/stats/lm.html),
[`coefficients`](https://rdrr.io/r/stats/coef.html)

## Examples

``` r
X1 <- c(0.69, 0.77, 0.92, 1.72, 1.79, 2.37, 2.64, 2.69, 2.84, 3.41)
Y <- c(3.28, 4.23, 3.34, 3.73, 5.33, 6.02, 5.16, 6.49, 6.49, 6.05)

# simple linear regression
model1 <- lm(Y ~ X1)
coefficients(model1) # unstandardised
#> (Intercept)          X1 
#>    2.717160    1.156674 
standardCoefs(model1) # unstandardised and standardised side by side
#>           b      beta
#> X1 1.156674 0.8674478

# multiple regression
X2 <- c(0.19, 0.22, 0.95, 0.43, 0.51, 0.04, 0.12, 0.44, 0.38, 0.33)
model2 <- lm(Y ~ X1 + X2)
standardCoefs(model2)
#>             b        beta
#> X1  1.1278652  0.84584304
#> X2 -0.4428046 -0.08903252

# model with an interaction term
model3 <- lm(Y ~ X1 * X2)
standardCoefs(model3)
#>                b       beta
#> X1     0.7945328  0.5958602
#> X2    -1.8409489 -0.3701504
#> X1:X2  1.0332433  0.3562668
```
