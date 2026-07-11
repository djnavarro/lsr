# Correlation matrices

Computes a correlation matrix, optionally with hypothesis tests and
corrections for multiple comparisons.

## Usage

``` r
correlate(
  x,
  y = NULL,
  test = FALSE,
  corr.method = "pearson",
  p.adjust.method = "holm"
)
```

## Arguments

- x:

  A numeric vector, matrix, or data frame containing the variables to be
  correlated.

- y:

  An optional second matrix or data frame. If provided, the variables in
  `x` are correlated with the variables in `y` rather than with each
  other.

- test:

  Set to `TRUE` to display p-values and sample sizes alongside the
  correlations. Defaults to `FALSE`.

- corr.method:

  The type of correlation to compute: `"pearson"` (the default),
  `"spearman"`, or `"kendall"`.

- p.adjust.method:

  The method used to correct p-values for multiple comparisons. Defaults
  to `"holm"`. All methods supported by
  [`p.adjust`](https://rdrr.io/r/stats/p.adjust.html) are accepted.

## Value

Prints the correlation matrix. If `test = TRUE`, also prints a matrix of
adjusted p-values and a matrix of sample sizes. The results are also
returned as a list with four elements: `correlation` (the correlation
matrix), `p.value` (the matrix of p-values), `sample.size` (the matrix
of sample sizes), and `args` (a record of the options used). The list
can be assigned to a variable and inspected if needed.

## Details

Calculates a correlation matrix between all pairs of numeric variables.
If only `x` is supplied, all pairwise correlations among the variables
in `x` are computed. If both `x` and `y` are supplied, variables in `x`
are correlated with variables in `y`.

Non-numeric variables (e.g., factors) are silently ignored: they appear
in the output with `NA` in place of correlation values. This makes it
convenient to pass an entire data frame without first removing
categorical columns.

When `test = TRUE`, hypothesis tests are run for every pair of numeric
variables. To reduce the risk of false positives from testing many pairs
at once, p-values are adjusted using the Holm method by default. See
[`p.adjust`](https://rdrr.io/r/stats/p.adjust.html) for other available
methods.

Missing data are handled using pairwise complete cases, so sample sizes
may differ across pairs of variables. If a particular pair of variables
has too few complete observations for
[`cor.test`](https://rdrr.io/r/stats/cor.test.html) to run, the
corresponding cell in the correlation matrix is left as `NA` rather than
causing the whole call to fail.

## See also

[`cor`](https://rdrr.io/r/stats/cor.html),
[`cor.test`](https://rdrr.io/r/stats/cor.test.html),
[`p.adjust`](https://rdrr.io/r/stats/p.adjust.html)

## Examples

``` r
# data frame with factors and missing values
data <- data.frame(
  anxiety = c(1.31, 2.72, 3.18, 4.21, 5.55, NA),
  stress = c(2.01, 3.45, 1.99, 3.25, 4.27, 6.80),
  depression = c(2.51, 1.77, 3.34, 5.83, 9.01, 7.74),
  happiness = c(4.02, 3.66, 5.23, 6.37, 7.83, 1.18),
  gender = factor(c("male", "female", "female", "male", "female", "female")),
  ssri = factor(c("no", "no", "no", NA, "yes", "yes"))
)

# Pearson correlation matrix (the default)
correlate(data)
#> 
#> CORRELATIONS
#> ============
#> - correlation type:  pearson 
#> - correlations shown only when both variables are numeric
#> 
#>            anxiety stress depression happiness gender ssri
#> anxiety          .  0.784      0.906     0.924      .    .
#> stress       0.784      .      0.693    -0.453      .    .
#> depression   0.906  0.693          .     0.247      .    .
#> happiness    0.924 -0.453      0.247         .      .    .
#> gender           .      .          .         .      .    .
#> ssri             .      .          .         .      .    .

# Spearman correlations
correlate(data, corr.method = "spearman")
#> 
#> CORRELATIONS
#> ============
#> - correlation type:  spearman 
#> - correlations shown only when both variables are numeric
#> 
#>            anxiety stress depression happiness gender ssri
#> anxiety          .  0.500      0.900     0.900      .    .
#> stress       0.500      .      0.543    -0.257      .    .
#> depression   0.900  0.543          .     0.429      .    .
#> happiness    0.900 -0.257      0.429         .      .    .
#> gender           .      .          .         .      .    .
#> ssri             .      .          .         .      .    .

# correlate two subsets of variables with each other
nervous <- data[, c("anxiety", "stress")]
happy <- data[, c("happiness", "depression")]
correlate(nervous, happy)
#> 
#> CORRELATIONS
#> ============
#> - correlation type:  pearson 
#> - correlations shown only when both variables are numeric
#> 
#>         happiness depression
#> anxiety     0.924      0.906
#> stress     -0.453      0.693

# include Holm-corrected p-values and sample sizes
correlate(data, test = TRUE)
#> 
#> CORRELATIONS
#> ============
#> - correlation type:  pearson 
#> - correlations shown only when both variables are numeric
#> 
#>            anxiety    stress    depression    happiness    gender    ssri   
#> anxiety          .     0.784         0.906        0.924         .       .   
#> stress       0.784         .         0.693       -0.453         .       .   
#> depression   0.906     0.693             .        0.247         .       .   
#> happiness    0.924    -0.453         0.247            .         .       .   
#> gender           .         .             .            .         .       .   
#> ssri             .         .             .            .         .       .   
#> 
#> ---
#> Signif. codes: . = p < .1, * = p<.05, ** = p<.01, *** = p<.001
#> 
#> 
#> p-VALUES
#> ========
#> - total number of tests run:  6 
#> - correction for multiple testing:  holm 
#> 
#>            anxiety stress depression happiness gender ssri
#> anxiety          .  0.467      0.171     0.150      .    .
#> stress       0.467      .      0.467     0.733      .    .
#> depression   0.171  0.467          .     0.733      .    .
#> happiness    0.150  0.733      0.733         .      .    .
#> gender           .      .          .         .      .    .
#> ssri             .      .          .         .      .    .
#> 
#> 
#> SAMPLE SIZES
#> ============
#> 
#>            anxiety stress depression happiness gender ssri
#> anxiety          5      5          5         5      5    4
#> stress           5      6          6         6      6    5
#> depression       5      6          6         6      6    5
#> happiness        5      6          6         6      6    5
#> gender           5      6          6         6      6    5
#> ssri             4      5          5         5      5    5
```
