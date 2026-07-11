# lsr

**lsr** is a companion package to the textbook [*Learning Statistics
with R*](https://learningstatisticswithr.com). It provides
beginner-friendly wrappers for common statistical procedures — t-tests,
chi-square tests, effect sizes, correlation matrices, and basic data
manipulation — with output designed to be readable by students
encountering statistics for the first time.

## Example

Here is an independent-samples t-test comparing extra sleep between two
drug groups in the built-in `sleep` dataset:

``` r

library(lsr)
independentSamplesTTest(formula = extra ~ group, data = sleep)
#> 
#>    Welch's independent samples t-test 
#> 
#> Outcome variable:   extra 
#> Grouping variable:  group 
#> 
#> Descriptive statistics: 
#>                 1     2
#>    mean     0.750 2.330
#>    std dev. 1.789 2.002
#> 
#> Hypotheses: 
#>    null:        population means equal for both groups
#>    alternative: different population means in each group
#> 
#> Test results: 
#>    t-statistic:  -1.861 
#>    degrees of freedom:  17.776 
#>    p-value:  0.079 
#> 
#> Other information: 
#>    two-sided 95% confidence interval:  [-3.365, 0.205] 
#>    estimated effect size (Cohen's d):  0.832
```

Compared to base R’s [`t.test()`](https://rdrr.io/r/stats/t.test.html),
the output labels every component in plain English and automatically
reports Cohen’s *d* alongside the test result.

## Where to go next

- [**Guided
  overview**](https://lsr.djnavarro.net/articles/overview.html) — a
  hands-on introduction for students and beginners, working through
  descriptive statistics, reshaping data, and hypothesis testing with a
  single example dataset.
- [**Critical
  commentary**](https://lsr.djnavarro.net/articles/commentary.html) — an
  honest account of the package’s limitations and pointers to better
  tools for users who have outgrown it.
- [**Reference**](https://lsr.djnavarro.net/reference/) — documentation
  for all 29 functions, organised by topic.

## Installation

Install the released version from [CRAN](https://CRAN.R-project.org):

``` r

install.packages("lsr")
```

Or the development version from GitHub:

``` r

# install.packages("devtools")
devtools::install_github("djnavarro/lsr")
```
