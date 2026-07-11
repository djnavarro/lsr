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

  A numeric vector of data for group 1, or a formula of the form
  `outcome ~ group` (in which case `data` can be used to supply a data
  frame).

- y:

  A numeric vector of data for group 2. Omit for a one-sample
  calculation.

- data:

  An optional data frame containing the variables in `x` when `x` is a
  formula.

- method:

  Which version of Cohen's d to calculate. Options are `"pooled"`
  (default), `"x.sd"`, `"y.sd"`, `"corrected"`, `"raw"`, `"paired"`, and
  `"unequal"`. See Details.

- mu:

  The null value for a one-sample calculation. Almost always 0 (the
  default).

- formula:

  A formula of the form `outcome ~ group`. This is an alternative way to
  supply the formula instead of using `x`.

## Value

A single positive number: the magnitude of the effect size d. The sign
of the mean difference is dropped, so the value is always zero or
greater.

## Details

The function can be used in two main ways. For two separate numeric
vectors, call `cohensD(x = group1, y = group2)`. For data in a data
frame with a grouping variable, use a formula:
`cohensD(outcome ~ group, data = mydata)`.

The `method` argument controls how the standard deviation is estimated:

- `"pooled"`:

  Pooled SD from both groups (matches Student's t-test). This is the
  default.

- `"corrected"`:

  Bias-corrected version of `"pooled"`, multiplied by `(N-3)/(N-2.25)`.

- `"raw"`:

  Like `"pooled"` but divides by N rather than N-2.

- `"x.sd"`:

  SD of the first group only.

- `"y.sd"`:

  SD of the second group only.

- `"unequal"`:

  Square root of the average of the two group variances (matches Welch's
  t-test).

- `"paired"`:

  SD of the within-person differences (matches a paired-samples t-test).

For a one-sample calculation, supply only `x` (and optionally `mu`). The
result is `abs(mean(x) - mu) / sd(x)`.

## References

Cohen, J. (1988). Statistical power analysis for the behavioral sciences
(2nd ed.). Hillsdale, NJ: Lawrence Erlbaum Associates.

## Examples

``` r
# two independent groups supplied as separate vectors
gradesA <- c(55, 65, 65, 68, 70) # 5 students with teacher A
gradesB <- c(56, 60, 62, 66) # 4 students with teacher B
cohensD(gradesA, gradesB)
#> [1] 0.699892

# the same comparison using a formula and a data frame
grade <- c(55, 65, 65, 68, 70, 56, 60, 62, 66)
teacher <- c("A", "A", "A", "A", "A", "B", "B", "B", "B")
cohensD(grade ~ teacher)
#> [1] 0.699892

# paired samples: use method = "paired" (SD of within-person differences)
pre <- c(100, 122, 97, 25, 274)
post <- c(104, 125, 99, 29, 277)
cohensD(pre, post, method = "paired")
#> [1] 3.824732

# equivalent one-sample calculation on the difference scores
cohensD(post - pre)
#> [1] 3.824732

# formula interface with a data frame
exams <- data.frame(grade, teacher)
cohensD(grade ~ teacher, data = exams)
#> [1] 0.699892
```
