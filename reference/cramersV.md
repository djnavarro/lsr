# Cramer's V

Calculate the Cramer's V measure of association

## Usage

``` r
cramersV(...)
```

## Arguments

- ...:

  Arguments to be passed to the `chisq.test` function.

## Value

A numeric variable with a single element corresponding to the value of
V.

## Details

Calculates the Cramer's V measure of effect size for chi-square tests of
association and goodness of fit. The arguments to the `cramersV`
function are all passed straight to the `chisq.test` function, and
should have the same format.

## Examples

``` r
# Consider an experiment with two conditions, each with 100
# participants. Each participant chooses between one of three
# options. Possible data for this experiment:

condition1 <- c(30, 20, 50)
condition2 <- c(35, 30, 35)
X <- cbind( condition1, condition2 )
rownames(X) <- c( 'choice1', 'choice2', 'choice3' )
print(X)
#>         condition1 condition2
#> choice1         30         35
#> choice2         20         30
#> choice3         50         35

# To test the null hypothesis that the distribution of choices
# is identical in the two conditions, we would run a chi-square
# test:
chisq.test(X)
#> 
#>  Pearson's Chi-squared test
#> 
#> data:  X
#> X-squared = 5.0317, df = 2, p-value = 0.0808
#> 

# To estimate the effect size we can use Cramer's V:
cramersV( X )  # returns a value of 0.159
#> [1] 0.1586139
```
