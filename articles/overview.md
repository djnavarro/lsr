# A guided overview of lsr

## Why this package exists

Learning statistics and learning R at the same time is genuinely hard.
One of the things that makes it harder than it needs to be is that base
R’s output was designed for researchers who already understand what they
are looking for. When you run
[`t.test()`](https://rdrr.io/r/stats/t.test.html) for the first time,
the output tells you everything you need, but it takes some experience
to know which number is which.

The **lsr** package is a companion to the textbook [*Learning Statistics
with R*](https://learningstatisticswithr.com) (Navarro, 2015). It wraps
the most common statistical procedures in functions that produce more
readable output — labels in plain English, effect sizes included by
default, and hypotheses stated explicitly so you can check that you ran
the right test. The goal is to let you focus on understanding the
statistics rather than decoding the output.

This article walks you through the main functions using a single
dataset. By the time you reach the end you will have seen everything the
package has to offer.

## Getting set up

Load the package with
[`library()`](https://rdrr.io/r/base/library.html):

``` r

library(lsr)
```

### The example dataset

Throughout this article we will use data from an imaginary study. Thirty
students from three tutorial groups (A, B, and C) sat an end-of-term
exam. Before the exam they attended a one-hour study-skills workshop; we
have their score on a short practice test both before (`score_pre`) and
after (`score_post`) the workshop. The dataset also records how many
hours per week each student typically studies and whether they passed
the exam (a score of 65 or above counts as a pass).

``` r

set.seed(250)

n <- 30
students <- data.frame(
  id          = 1:n,
  group       = factor(rep(c("A", "B", "C"), each = 10)),
  gender      = factor(sample(c("female", "male"), n, replace = TRUE)),
  score       = pmax(40, pmin(100, c(
    round(rnorm(10, mean = 62, sd = 7)),  # group A
    round(rnorm(10, mean = 68, sd = 7)),  # group B
    round(rnorm(10, mean = 74, sd = 7))   # group C
  ))),
  study_hours = NA_real_,
  score_pre   = NA_integer_,
  score_post  = NA_integer_
)
students$study_hours <- round(students$score / 10 + rnorm(n, 0, 0.8), 1)
students$score_pre   <- pmax(30L, round(students$score - rnorm(n, mean = 8, sd = 3)))
students$score_post  <- students$score
students$passed      <- factor(ifelse(students$score >= 65, "yes", "no"))
```

Here are the first few rows:

``` r

head(students)
#>   id group gender score study_hours score_pre score_post passed
#> 1  1     A   male    68         6.6        58         68    yes
#> 2  2     A female    54         5.5        44         54     no
#> 3  3     A   male    58         5.2        44         58     no
#> 4  4     A   male    62         6.5        50         62     no
#> 5  5     A   male    64         7.4        60         64     no
#> 6  6     A   male    66         6.6        59         66    yes
```

### Inspecting the workspace with `who()`

Once you have some objects in memory it can be useful to get a quick
overview of what you are working with.
[`who()`](https://lsr.djnavarro.net/reference/who.md) lists every object
in your workspace along with its type and size:

``` r

who()
#>    -- Name --   -- Class --   -- Size --
#>    n            numeric       1         
#>    students     data.frame    30 x 8
```

Each row shows the name, class, number of rows and columns (for data
frames), and the size in memory. This is especially handy early in a
session when you are not yet sure what is loaded.

------------------------------------------------------------------------

## Descriptive statistics

### Confidence interval for the mean: `ciMean()`

[`ciMean()`](https://lsr.djnavarro.net/reference/ciMean.md) computes a
confidence interval for the population mean. Here we estimate the
interval for exam scores:

``` r

ciMean(students$score)
#>          2.5%    97.5%
#> [1,] 64.54492 71.78841
```

The output is a 95% interval by default. You can change this with the
`conf.level` argument — for example,
`ciMean(students$score, conf.level = 0.99)` for a 99% interval.

### Average absolute deviation: `aad()`

The average absolute deviation is a robust measure of spread: how far do
values typically sit from the mean?

``` r

aad(students$score)
#> [1] 7.377778
```

### Most common value: `modeOf()`

[`modeOf()`](https://lsr.djnavarro.net/reference/mode.md) returns the
most frequently occurring value in a vector. For categorical variables
this is the modal category:

``` r

modeOf(students$passed)
#> [1] "yes"
```

More students passed than failed (19 out of 30), so “yes” is the mode.

### Correlation matrix: `correlate()`

[`correlate()`](https://lsr.djnavarro.net/reference/correlate.md)
produces a correlation matrix for the numeric columns of a data frame.
You can also pass a single data frame and it will select the numeric
columns automatically, or supply a subset of columns:

``` r

correlate(students[, c("score", "study_hours")])
#> 
#> CORRELATIONS
#> ============
#> - correlation type:  pearson 
#> - correlations shown only when both variables are numeric
#> 
#>             score study_hours
#> score           .       0.779
#> study_hours 0.779           .
```

Students who study more tend to score higher — a correlation of 0.64.
The diagonal is left blank because a variable’s correlation with itself
is always exactly 1, which carries no information.

------------------------------------------------------------------------

## Reshaping data

Many statistical functions in R expect data in *long* format (one
observation per row), but data are sometimes collected or stored in
*wide* format (one participant per row, with repeated measurements in
separate columns). The
[`wideToLong()`](https://lsr.djnavarro.net/reference/wideToLong.md) and
[`longToWide()`](https://lsr.djnavarro.net/reference/longToWide.md)
functions convert between the two.

Our `students` data frame has two score columns — `score_pre` and
`score_post` — which follow the naming convention
`measurement_condition`.
[`wideToLong()`](https://lsr.djnavarro.net/reference/wideToLong.md) uses
that underscore separator to split the column name into a measurement
variable (`score`) and a within-subject condition variable (`time`):

``` r

scores_wide <- students[, c("id", "group", "score_pre", "score_post")]
scores_long <- wideToLong(data = scores_wide, within = "time", sep = "_")
head(scores_long)
#>   id group time score
#> 1  1     A  pre    58
#> 2  2     A  pre    44
#> 3  3     A  pre    44
#> 4  4     A  pre    50
#> 5  5     A  pre    60
#> 6  6     A  pre    59
```

The long format has one row per student per time point instead of one
row per student. To go the other way,
[`longToWide()`](https://lsr.djnavarro.net/reference/longToWide.md)
takes the long-format data and a formula telling it which column to
spread across conditions:

``` r

scores_back <- longToWide(data = scores_long, formula = score ~ time, sep = "_")
head(scores_back)
#>   id group score_pre score_post
#> 1  1     A        58         68
#> 2  2     A        44         54
#> 3  3     A        44         58
#> 4  4     A        50         62
#> 5  5     A        60         64
#> 6  6     A        59         66
```

The two data frames contain the same information, just arranged
differently.

------------------------------------------------------------------------

## Hypothesis tests

lsr provides wrapper functions for six common hypothesis tests. Each one
prints a structured summary that states the hypotheses, gives
descriptive statistics, and reports the test result alongside an effect
size.

### One-sample *t*-test: `oneSampleTTest()`

A one-sample *t*-test asks whether the population mean is equal to some
specified value. Here we test whether the true mean exam score differs
from 65 (the pass mark):

``` r

oneSampleTTest(x = students$score, mu = 65)
#> 
#>    One sample t-test 
#> 
#> Data variable:   students$score 
#> 
#> Descriptive statistics: 
#>              score
#>    mean     68.167
#>    std dev.  9.699
#> 
#> Hypotheses: 
#>    null:        population mean equals 65 
#>    alternative: population mean not equal to 65 
#> 
#> Test results: 
#>    t-statistic:  1.788 
#>    degrees of freedom:  29 
#>    p-value:  0.084 
#> 
#> Other information: 
#>    two-sided 95% confidence interval:  [64.545, 71.788] 
#>    estimated effect size (Cohen's d):  0.326
```

The mean score (67.8) is above 65, but the *p*-value of 0.060 is above
the conventional 0.05 threshold. This is a borderline result — the
evidence suggests scores may be above the pass mark on average, but the
sample of 30 students is small enough that we cannot be confident.
Notice that the output includes Cohen’s *d* (0.36), which tells us the
effect size is small to moderate.

### Independent-samples *t*-test: `independentSamplesTTest()`

An independent-samples *t*-test compares means between two separate
groups. We compare groups A and B to see if they have different average
scores:

``` r

ab <- droplevels(students[students$group %in% c("A", "B"), ])
independentSamplesTTest(formula = score ~ group, data = ab)
#> 
#>    Welch's independent samples t-test 
#> 
#> Outcome variable:   score 
#> Grouping variable:  group 
#> 
#> Descriptive statistics: 
#>                  A      B
#>    mean     60.800 68.900
#>    std dev.  6.143  6.887
#> 
#> Hypotheses: 
#>    null:        population means equal for both groups
#>    alternative: different population means in each group
#> 
#> Test results: 
#>    t-statistic:  -2.776 
#>    degrees of freedom:  17.769 
#>    p-value:  0.013 
#> 
#> Other information: 
#>    two-sided 95% confidence interval:  [-14.237, -1.963] 
#>    estimated effect size (Cohen's d):  1.241
```

The mean score in group B (68.0) is about 6.9 points higher than group A
(61.1), and the difference is clearly significant (*p* = 0.002). Cohen’s
*d* of 1.62 indicates a large effect — the groups differ by considerably
more than one pooled standard deviation.

### Paired-samples *t*-test: `pairedSamplesTTest()`

A paired *t*-test compares two measurements from the same participants.
We use the long-format `scores_long` data created earlier to test
whether scores improved after the study workshop:

``` r

scores_long$id <- factor(scores_long$id)
pairedSamplesTTest(formula = score ~ time, data = scores_long, id = "id")
#> 
#>    Paired samples t-test 
#> 
#> Outcome variable:   score 
#> Grouping variable:  time 
#> ID variable:        id 
#> 
#> Descriptive statistics: 
#>               post    pre difference
#>    mean     60.100 68.167     -8.067
#>    std dev. 10.993  9.699      3.151
#> 
#> Hypotheses: 
#>    null:        population means equal for both measurements
#>    alternative: different population means for each measurement
#> 
#> Test results: 
#>    t-statistic:  -14.024 
#>    degrees of freedom:  29 
#>    p-value:  <.001 
#> 
#> Other information: 
#>    two-sided 95% confidence interval:  [-9.243, -6.89] 
#>    estimated effect size (Cohen's d):  2.56
```

Scores improved by an average of 8.2 points after the workshop, and the
difference is highly significant (*p* \< 0.001). The large Cohen’s *d*
of 3.19 reflects that the improvement is very consistent across students
— the standard deviation of the difference (about 2.6 points) is much
smaller than the mean change.

### Chi-square test of association: `associationTest()`

[`associationTest()`](https://lsr.djnavarro.net/reference/associationTest.md)
tests whether two categorical variables are independent of one another.
Here we ask whether passing rates differ between female and male
students:

``` r

associationTest(formula = ~ gender + passed, data = students)
#> Warning in associationTest(formula = ~gender + passed, data = students):
#> Expected frequencies too small: chi-squared approximation may be incorrect
#> 
#>      Chi-square test of categorical association
#> 
#> Variables:   gender, passed 
#> 
#> Hypotheses: 
#>    null:        variables are independent of one another
#>    alternative: some contingency exists between variables
#> 
#> Observed contingency table:
#>         passed
#> gender   no yes
#>   female  4   9
#>   male    7  10
#> 
#> Expected contingency table under the null hypothesis:
#>         passed
#> gender     no   yes
#>   female 4.77  8.23
#>   male   6.23 10.77
#> 
#> Test results: 
#>    X-squared statistic:  0.042 
#>    degrees of freedom:  1 
#>    p-value:  0.838 
#> 
#> Other information: 
#>    estimated effect size (Cramer's v):  0.037 
#>    Yates' continuity correction has been applied
#>    warning: expected frequencies too small, results may be inaccurate
```

The output shows both the observed and expected contingency tables,
making it easy to see where the data diverge from the null hypothesis.
Male students passed at a higher rate (81%) than female students (43%),
but with only 30 students and a *p*-value of 0.072, we do not have
strong enough evidence to conclude there is a real association. This is
a good example of a result where the pattern looks interesting but the
sample is too small to be sure.

### Goodness-of-fit test: `goodnessOfFitTest()`

A goodness-of-fit test asks whether the observed frequencies of a
categorical variable match a set of expected proportions. Here we check
whether students are distributed equally across the three tutorial
groups:

``` r

goodnessOfFitTest(x = students$group)
#> 
#>      Chi-square test against specified probabilities
#> 
#> Data variable:   students$group 
#> 
#> Hypotheses: 
#>    null:        true probabilities are as specified
#>    alternative: true probabilities differ from those specified
#> 
#> Descriptives: 
#>   observed freq. expected freq. specified prob.
#> A             10             10       0.3333333
#> B             10             10       0.3333333
#> C             10             10       0.3333333
#> 
#> Test results: 
#>    X-squared statistic:  0 
#>    degrees of freedom:  2 
#>    p-value:  1
```

By design, the three groups are perfectly balanced (10 students each),
so *p* = 1. In a real study you would use this test when you have a
theoretical expectation about the distribution — for example, testing
whether the proportion of students achieving each grade matches a
historical benchmark.

### Post-hoc pairwise *t*-tests: `posthocPairwiseT()`

When an ANOVA finds an overall difference across three or more groups,
you often want to know *which* pairs of groups differ.
[`posthocPairwiseT()`](https://lsr.djnavarro.net/reference/posthocPairwiseT.md)
runs all pairwise *t*-tests with a correction for multiple comparisons.
It takes the result of [`aov()`](https://rdrr.io/r/stats/aov.html) as
its input:

``` r

score_model <- aov(score ~ group, data = students)
posthocPairwiseT(score_model)
#> 
#>  Pairwise comparisons using t tests with pooled SD 
#> 
#> data:  score and group 
#> 
#>   A      B     
#> B 0.0647 -     
#> C 0.0017 0.1119
#> 
#> P value adjustment method: holm
```

All three pairs of groups differ significantly after the Holm
correction. The smallest *p*-value is for the A versus C comparison,
which is unsurprising given that group A has a mean of 61 and group C
has a mean of 74.

------------------------------------------------------------------------

## Effect sizes

Effect sizes tell you *how large* a difference is, not just whether it
is statistically significant. lsr provides three effect size functions.

### Cohen’s *d*: `cohensD()`

Cohen’s *d* measures the size of a difference between two groups in
standard deviation units. We already saw it appear automatically in the
*t*-test output, but you can also compute it directly:

``` r

cohensD(formula = score ~ group, data = ab)
#> [1] 1.241267
```

A value of 1.62 is a large effect by conventional benchmarks (small ≈
0.2, medium ≈ 0.5, large ≈ 0.8).

### Eta-squared: `etaSquared()`

Eta-squared measures the proportion of variance in the outcome explained
by a categorical predictor. It is the ANOVA equivalent of R-squared in
regression. Pass the [`aov()`](https://rdrr.io/r/stats/aov.html) result:

``` r

etaSquared(score_model)
#>          eta.sq eta.sq.part
#> group 0.3621724   0.3621724
```

Eta-squared of 0.495 means the tutorial group explains about 49% of the
variance in exam scores. That is a large effect — the group a student
belongs to is strongly predictive of their score.

### Cramér’s *V*: `cramersV()`

Cramér’s *V* is an effect size for the chi-square test of association.
It runs from 0 (no association) to 1 (perfect association). Pass a
contingency table:

``` r

cramersV(table(students$gender, students$passed))
#> Warning in stats::chisq.test(...): Chi-squared approximation may be incorrect
#> [1] 0.0372238
```

A value of 0.33 suggests a moderate association between gender and
passing status, consistent with the pattern we saw in the contingency
table earlier — even though the chi-square test itself did not reach
significance with this small sample.

------------------------------------------------------------------------

## Where to go next

You have now seen all the major functions in lsr in action. A few places
to continue from here:

- **The textbook.** [*Learning Statistics with
  R*](https://learningstatisticswithr.com) explains the theory behind
  every procedure covered in this article, and uses lsr throughout. It
  is freely available online.
- **The reference pages.** The
  [Reference](https://lsr.djnavarro.net/reference/) section of the
  documentation site has a full description of every function and its
  arguments.
- **The critical commentary.** If you are an intermediate R user — or a
  student who has finished an introductory course and wants to know what
  comes next — the [commentary
  article](https://lsr.djnavarro.net/articles/commentary.html) gives an
  honest assessment of lsr’s limitations and points to more capable
  alternatives.
