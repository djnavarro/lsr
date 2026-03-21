# Cohen's d

Calculates the Cohen's d measure of effect size.

## Usage

``` r
cohensD(
  x = NULL,
  y = NULL,
  data = NULL,
  method = "pooled",
  mu = 0,
  formula = NULL
)
```

## Arguments

- x:

  A numeric variable containing the data for group 1, or possibly a
  formula of the form `outcome ~ group`

- y:

  If `x` is a numeric variable, the `y` argument should be a numeric
  variable containing the data for group 2. If a one-sample calculation
  is desired, then no value for `y` should be specified.

- data:

  If `x` is a formula, then `data` is an optional argument specifying
  data frame containing the variables in the formula.

- method:

  Which version of the d statistic should we calculate? Possible values
  are `"pooled"` (the default), `"x.sd"`, `"y.sd"`, `"corrected"`,
  `"raw"`, `"paired"` and `"unequal"`. See below for specifics.

- mu:

  The "null" value against which the effect size should be measured.
  This is almost always 0 (the default), so this argument is rarely
  specified.

- formula:

  An alias for `x` if a formula input is used. Included for the sake of
  consistency with the `t.test` function.

## Value

Numeric variable containing the effect size, d. Note that it does not
show the direction of the effect, only the magnitude. That is, the value
of d returned by the function is always positive or zero.

## Details

The `cohensD` function calculates the Cohen's d measure of effect size
in one of several different formats. The function is intended to be
called in one of two different ways, mirroring the `t.test` function.
That is, the first input argument `x` is a formula, then a command of
the form `cohensD(x = outcome~group, data = data.frame)` is expected,
whereas if `x` is a numeric variable, then a command of the form
`cohensD(x = group1, y = group2)` is expected.

The `method` argument allows the user to select one of several different
variants of Cohen's d. Assuming that the original t-test for which an
effect size is desired was an independent samples t-test (i.e., not one
sample or paired samples t-test), then there are several possibilities
for how the normalising term (i.e., the standard deviation estimate) in
Cohen's d should be calculated. The most commonly used method is to use
the same pooled standard deviation estimate that is used in a Student
t-test (`method = "pooled"`, the default). If `method = "raw"` is used,
then the same pooled standard deviation estimate is used, except that
the sample standard deviation is used (divide by N) rather than the
unbiased estimate of the population standard deviation (divide by N-2).
Alternatively, there may be reasons to use only one of the two groups to
estimate the standard deviation. To do so, use `method = "x.sd"` to
select the `x` variable, or the first group listed in the grouping
factor; and `method = "y.sd"` to normalise by `y`, or the second group
listed in the grouping factor. The last of the "Student t-test" based
measures is the unbiased estimator of d (`method = "corrected"`), which
multiplies the "pooled" version by (N-3)/(N-2.25).

For other versions of the t-test, there are two possibilities
implemented. If the original t-test did not make a homogeneity of
variance assumption, as per the Welch test, the normalising term should
mirror the Welch test (`method = "unequal"`). Or, if the original t-test
was a paired samples t-test, and the effect size desired is intended to
be based on the standard deviation of the differences, then
`method = "paired"` should be used.

The last argument to `cohensD` is `mu`, which represents the mean
against which one sample Cohen's d calculation should be assessed. Note
that this is a slightly narrower usage of `mu` than the `t.test`
function allows. `cohensD` does not currently support the use of a
non-zero `mu` value for a paired-samples calculation.

## References

Cohen, J. (1988). Statistical power analysis for the behavioral sciences
(2nd ed.). Hillsdale, NJ: Lawrence Erlbaum Associates.

## Examples

``` r
# calculate Cohen's d for two independent samples:
gradesA <- c(55, 65, 65, 68, 70) # 5 students with teacher A
gradesB <- c(56, 60, 62, 66)     # 4 students with teacher B
cohensD(gradesA, gradesB)
#> [1] 0.699892

# calculate Cohen's d for the same data, described differently:
grade <- c(55, 65, 65, 68, 70, 56, 60, 62, 66) # grades for all students
teacher <- c("A", "A", "A", "A", "A", "B", "B", "B", "B") # teacher for each student
cohensD(grade ~ teacher)
#> [1] 0.699892

# calculate Cohen's d for two paired samples:
pre  <- c(100, 122, 97, 25, 274) # a pre-treatment measure for 5 cases
post <- c(104, 125, 99, 29, 277) # the post-treatment measure for the same 5 cases
cohensD(pre, post, method = "paired") # ... explicitly indicate that it's paired, or else
#> [1] 3.824732
cohensD(post - pre)  # ... do a "single-sample" calculation on the difference
#> [1] 3.824732

# support for data frames:
exams <- data.frame(grade, teacher)
cohensD(exams$grade ~ exams$teacher)    # using $
#> Error in eval(x[[2]], y): object 'exams' not found
cohensD(grade ~ teacher, data = exams)  # using the 'data' argument
#> [1] 0.699892
```
