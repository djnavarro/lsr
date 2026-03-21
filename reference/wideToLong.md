# Reshape from wide to long

Reshape a data frame from wide form to long form using the variable
names

## Usage

``` r
wideToLong(data, within = "within", sep = "_", split = TRUE)
```

## Arguments

- data:

  The data frame.

- within:

  Name to give to the long-form within-subject factor(s)

- sep:

  Separator string used in wide-form variable names

- split:

  Should multiple within-subject factors be split into multiple
  variables?

## Value

A data frame containing the reshaped data

## Details

The `wideToLong` function is the companion function to `longToWide`. The
`data` argument is a "wide form" data frame, in which each row
corresponds to a single experimental unit (e.g., a single subject). The
output is a "long form" data frame, in which each row corresponds to a
single observation.

The `wideToLong` function relies on the variable names to determine how
the data should be reshaped. The naming scheme for these variables
places the name of the measured variable first, followed by the levels
of the within-subjects variable(s), separated by the separator string
`sep` (default is `_`) The separator string cannot appear anywhere else
in the variable names: variables without the separator string are
assumed to be between-subject variables.

If the experiment measured the `accuracy` of participants at some task
at two different points in time, then the wide form data frame would
contain variables of the form `accuracy_t1` and `accuracy_t2`. After
reshaping, the long form data frame would contain one measured variable
called `accuracy`, and a within-subjects factor with levels `t1` and
`t2`. The name of the within-subjects factor is the `within` argument.

The function supports experimental designs with multiple within-subjects
factors and multi-variable observations. For example, suppose each
experimental subject is tested in two `conditions` (`cond1` and
`cond2`), on each of two `days` (`day1` and `day2`), yielding an
experimental design in which four observations are made for each
subject. For each such observation, we record the mean response time
`MRT` for and proportion of correct responses `PC` for the participant.
The variable names needed for a design such as this one would be
`MRT_cond1_day1`, `MRT_cond1_day2`, `PC_cond1_day1`, etc. The `within`
argument should be a vector of names for the within-subject factors: in
this case, `within = c("condition","day")`.

By default, if there are multiple within-subject factors implied by the
existence of multiple separators, the output will keep these as distinct
variables in the long form data frame (`split=FALSE`). If `split=TRUE`,
the within-subject factors will be collapsed into a single variable.

## See also

[`longToWide`](https://lsr.djnavarro.net/reference/longToWide.md),
[`reshape`](https://rdrr.io/r/stats/reshape.html)

## Examples

``` r
# Outcome measure is mean response time (MRT), measured in two conditions
# with 4 participants. All participants participate in both conditions.

wide <- data.frame( accuracy_t1 = c( .15,.50,.78,.55 ),  # accuracy at time point 1
                    accuracy_t2 = c( .55,.32,.99,.60 ),  # accuracy at time point 2
                    id = 1:4 )                           # id variable

# convert to long form
wideToLong( wide, "time" )
#>   id time accuracy
#> 1  1   t1     0.15
#> 2  2   t1     0.50
#> 3  3   t1     0.78
#> 4  4   t1     0.55
#> 5  1   t2     0.55
#> 6  2   t2     0.32
#> 7  3   t2     0.99
#> 8  4   t2     0.60


# A more complex design with multiple within-subject factors. Again, we have only
# four participants, but now we have two different outcome measures, mean response
# time (MRT) and the proportion of correct responses (PC). Additionally, we have two
# different repeated measures variables. As before, we have the experimental condition
# (cond1, cond2), but this time each participant does both conditions on two different
# days (day1, day2). Finally, we have multiple between-subject variables too, namely
# id and gender.

wide2 <- data.frame( id = 1:4,
                     gender = factor( c("male","male","female","female") ),
                     MRT_cond1_day1 = c( 415,500,478,550 ),
                     MRT_cond2_day1 = c( 455,532,499,602 ),
                     MRT_cond1_day2 = c( 400,490,468,502 ),
                     MRT_cond2_day2 = c( 450,518,474,588 ),
                     PC_cond1_day1 = c( 79,83,91,75 ),
                     PC_cond2_day1 = c( 82,86,90,78 ),
                     PC_cond1_day2 = c( 88,92,98,89 ),
                     PC_cond2_day2 = c( 93,97,100,95 ) )

# conversion to long form:
wideToLong( wide2 )
#>    id gender MRT  PC within1 within2
#> 1   1   male 415  79   cond1    day1
#> 2   2   male 500  83   cond1    day1
#> 3   3 female 478  91   cond1    day1
#> 4   4 female 550  75   cond1    day1
#> 5   1   male 455  82   cond2    day1
#> 6   2   male 532  86   cond2    day1
#> 7   3 female 499  90   cond2    day1
#> 8   4 female 602  78   cond2    day1
#> 9   1   male 400  88   cond1    day2
#> 10  2   male 490  92   cond1    day2
#> 11  3 female 468  98   cond1    day2
#> 12  4 female 502  89   cond1    day2
#> 13  1   male 450  93   cond2    day2
#> 14  2   male 518  97   cond2    day2
#> 15  3 female 474 100   cond2    day2
#> 16  4 female 588  95   cond2    day2
wideToLong( wide2, within = c("condition","day") )
#>    id gender MRT  PC condition  day
#> 1   1   male 415  79     cond1 day1
#> 2   2   male 500  83     cond1 day1
#> 3   3 female 478  91     cond1 day1
#> 4   4 female 550  75     cond1 day1
#> 5   1   male 455  82     cond2 day1
#> 6   2   male 532  86     cond2 day1
#> 7   3 female 499  90     cond2 day1
#> 8   4 female 602  78     cond2 day1
#> 9   1   male 400  88     cond1 day2
#> 10  2   male 490  92     cond1 day2
#> 11  3 female 468  98     cond1 day2
#> 12  4 female 502  89     cond1 day2
#> 13  1   male 450  93     cond2 day2
#> 14  2   male 518  97     cond2 day2
#> 15  3 female 474 100     cond2 day2
#> 16  4 female 588  95     cond2 day2

# treat "condition x day" as a single repeated measures variable:
wideToLong( wide2, split = FALSE)
#>    id gender     within MRT  PC
#> 1   1   male cond1_day1 415  79
#> 2   2   male cond1_day1 500  83
#> 3   3 female cond1_day1 478  91
#> 4   4 female cond1_day1 550  75
#> 5   1   male cond2_day1 455  82
#> 6   2   male cond2_day1 532  86
#> 7   3 female cond2_day1 499  90
#> 8   4 female cond2_day1 602  78
#> 9   1   male cond1_day2 400  88
#> 10  2   male cond1_day2 490  92
#> 11  3 female cond1_day2 468  98
#> 12  4 female cond1_day2 502  89
#> 13  1   male cond2_day2 450  93
#> 14  2   male cond2_day2 518  97
#> 15  3 female cond2_day2 474 100
#> 16  4 female cond2_day2 588  95
```
