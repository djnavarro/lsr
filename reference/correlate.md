# Correlation matrices

Computes a correlation matrix and runs hypothesis tests with corrections
for multiple comparisons

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

  Matrix or data frame containing variables to be correlated

- y:

  Optionally, a second set of variables to be correlated with those in
  `x`

- test:

  Should hypothesis tests be displayed? (Default=`FALSE`)

- corr.method:

  What kind of correlations should be computed? Default is `"pearson"`,
  but `"spearman"` and `"kendall"` are also supported

- p.adjust.method:

  What method should be used to correct for multiple comparisons.
  Default value is `"holm"`, and the allowable values are the same as
  for [`p.adjust`](https://rdrr.io/r/stats/p.adjust.html)

## Value

The printed output shows the correlation matrix, and if tests are
requested it also reports a matrix of p-values and sample sizes
associated with each correlation (these can vary if there are missing
data). The underlying data structure is an object of class `correlate`
(an S3 class). It is effectively a list containing four elements:
`correlation` is the correlation matrix, `p.value` is the matrix of
p-values, `sample.size` is the matrix of sample sizes, and `args` is a
vector that stores information about what the user requested.

## Details

The `correlate` function calculates a correlation matrix between all
pairs of variables. Much like the `cor` function, if the user inputs
only one set of variables (`x`) then it computes all pairwise
correlations between the variables in `x`. If the user specifies both
`x` and `y` it correlates the variables in `x` with the variables in
`y`.

Unlike the `cor` function, `correlate` does not generate an error if
some of the variables are categorical (i.e., factors). Variables that
are not numeric (or integer) class are simply ignored. They appear in
the output, but no correlations are reported for those variables. The
decision to have the `correlate` function allow the user a little
leniency when the input contains non-numeric variables should be
explained. The motivation is pedagogical rather than statistical. It is
sometimes the case in psychology that students need to work with
correlation matrices before they are comfortable subsetting a data
frame, so it is convenient to allow them to type commands like
`correlate(data)` even when `data` contains variables for which
Pearson/Spearman correlations are not appropriate. (It is also useful to
use the output of `correlate` to illustrate the fact that Pearson
correlations should not be used for categorical variables).

A second difference between `cor` and `correlate` is that `correlate`
runs hypothesis tests for all correlations in the correlation matrix
(using the `cor.test` function to do the work). The results of the tests
are only displayed to the user if `test=TRUE`. This is a pragmatic
choice, given the (perhaps unfortunate) fact that psychologists often
want to see the results of these tests: it is probably not coincidental
that the `corr.test` function in the psych package already provides this
functionality (though the output is difficult for novices to read).

The concern with running hypothesis tests for all elements of a
correlation matrix is inflated Type I error rates. To minimise this
risk, reported p-values are adjusted using the Holm method. The user can
change this setting by specifying `p.adjust.method`. See
[`p.adjust`](https://rdrr.io/r/stats/p.adjust.html) for details.

Missing data are handled using pairwise complete cases.

## Examples

``` r
# data frame with factors and missing values
data <- data.frame(
  anxiety = c(1.31,2.72,3.18,4.21,5.55,NA),
  stress = c(2.01,3.45,1.99,3.25,4.27,6.80),
  depression = c(2.51,1.77,3.34,5.83,9.01,7.74),
  happiness = c(4.02,3.66,5.23,6.37,7.83,1.18),
  gender = factor( c("male","female","female","male","female","female") ),
  ssri = factor( c("no","no","no",NA,"yes","yes") )
)

# default output is just the (Pearson) correlation matrix
correlate( data )
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

# other types of correlation:
correlate( data, corr.method="spearman" )
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

# two meaningful subsets to be correlated:
nervous <- data[,c("anxiety","stress")]
happy <- data[,c("happiness","depression","ssri")]

# default output for two matrix input
correlate( nervous, happy )
#> 
#> CORRELATIONS
#> ============
#> - correlation type:  pearson 
#> - correlations shown only when both variables are numeric
#> 
#>         happiness depression ssri
#> anxiety     0.924      0.906    .
#> stress     -0.453      0.693    .

# the same examples, with Holm-corrected p-values
correlate( data, test=TRUE )
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
correlate( nervous, happy, test=TRUE )
#> 
#> CORRELATIONS
#> ============
#> - correlation type:  pearson 
#> - correlations shown only when both variables are numeric
#> 
#>         happiness    depression    ssri   
#> anxiety     0.924.        0.906       .   
#> stress     -0.453         0.693       .   
#> 
#> ---
#> Signif. codes: . = p < .1, * = p<.05, ** = p<.01, *** = p<.001
#> 
#> 
#> p-VALUES
#> ========
#> - total number of tests run:  4 
#> - correction for multiple testing:  holm 
#> 
#>         happiness depression ssri
#> anxiety     0.100      0.103    .
#> stress      0.367      0.255    .
#> 
#> 
#> SAMPLE SIZES
#> ============
#> 
#>         happiness depression ssri
#> anxiety         5          5    .
#> stress          6          6    .
```
