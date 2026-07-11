# Standardised regression coefficients

Calculates the standardised regression coefficients for a linear model.

## Usage

``` r
standardCoefs(x)
```

## Arguments

- x:

  A linear model object (i.e. class `lm`)

## Value

A matrix with the regressors as rows, and the two different regression
coefficients (unstandardised and standardised) as the two columns. The
columns are labeled b (unstandardised) and beta (standardised).

## Details

Calculates the standardised regression coefficients (beta-weights),
namely the values of the regression coefficients that would have been
observed has all regressors and the outcome variable been scaled to have
mean 0 and variance 1 before fitting the regression model. Standardised
coefficients are sometimes useful in some applied contexts since there
is a sense in which all beta values are "on the same scale", though this
is not entirely unproblematic.

## Examples

``` r

# Example 1: simple linear regression

# data
X1 <- c(0.69, 0.77, 0.92, 1.72, 1.79, 2.37, 2.64, 2.69, 2.84, 3.41)
Y  <- c(3.28, 4.23, 3.34, 3.73, 5.33, 6.02, 5.16, 6.49, 6.49, 6.05)

model1 <- lm( Y ~ X1 )  # run a simple linear regression
coefficients( model1 )  # extract the raw regression coefficients
#> (Intercept)          X1 
#>    2.717160    1.156674 
standardCoefs( model1 ) # extract standardised coefficients
#>           b      beta
#> X1 1.156674 0.8674478


# Example 2: multiple linear regression

X2 <- c(0.19, 0.22, 0.95, 0.43, 0.51, 0.04, 0.12, 0.44, 0.38, 0.33)
model2 <- lm( Y ~ X1 + X2 )   # new model
standardCoefs( model2 )       # standardised coefficients
#>             b        beta
#> X1  1.1278652  0.84584304
#> X2 -0.4428046 -0.08903252

#Example 3: interaction terms

model3 <- lm( Y ~ X1 * X2 )
coefficients( model3 )
#> (Intercept)          X1          X2       X1:X2 
#>   3.4155302   0.7945328  -1.8409489   1.0332433 
standardCoefs( model3 )
#>                b       beta
#> X1     0.7945328  0.5958602
#> X2    -1.8409489 -0.3701504
#> X1:X2  1.0332433  0.3562668

# Note that these beta values are equivalent to standardising all
# three regressors including the interaction term X1:X2, not merely
# standardising the two predictors X1 and X2.
```
