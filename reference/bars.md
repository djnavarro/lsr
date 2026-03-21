# Grouped Bar Plots with Error Bars

Grouped bar plots with error bars

## Usage

``` r
bars(
  formula,
  data = NULL,
  heightFun = mean,
  errorFun = ciMean,
  yLabel = NULL,
  xLabels = NULL,
  main = "",
  ylim = NULL,
  barFillColour = NULL,
  barLineWidth = 2,
  barLineColour = "black",
  barSpaceSmall = 0.2,
  barSpaceBig = 1,
  legendLabels = NULL,
  legendDownShift = 0,
  legendLeftShift = 0,
  errorBarLineWidth = 1,
  errorBarLineColour = "grey40",
  errorBarWhiskerWidth = 0.2
)
```

## Arguments

- formula:

  A two-sided formula specifying the response variable and the grouping
  factors

- data:

  An optional data frame containing the variables

- heightFun:

  The function used to calculate the bar height for a group
  (default=mean)

- errorFun:

  The function used to calculate the error bar for a group
  (default=ciMean). No bars drawn if `errorFun=FALSE`

- yLabel:

  The y-axis label (defaults to the name of the response variable)

- xLabels:

  The x-axis bar labels (defaults to factor labels of the appropriate
  grouping variable)

- main:

  The plot title

- ylim:

  The y-axis limit: lower bound defaults to 0, default upper bound
  estimated

- barFillColour:

  The colours to fill the bars (defaults to a rainbow palette with
  saturation .3)

- barLineWidth:

  The width of the bar border lines (default=2)

- barLineColour:

  The colour of the bar border lines (default="black")

- barSpaceSmall:

  The size of the gap between bars within a cluster, as a proportion of
  bar width (default=.2)

- barSpaceBig:

  The size of the gap separating clusters of bars, as a proportion of
  bar width (default=1)

- legendLabels:

  The text for the legend (defaults to factor labels of the appropriate
  grouping variable). No legends drawn if `legendLabels=FALSE` or if
  only one grouping variable is specified

- legendDownShift:

  How far below the top is the legend, as proportion of plot height?
  (default=0)

- legendLeftShift:

  How far away from the right edge is the legend, as proportion of plot?
  (default=0)

- errorBarLineWidth:

  The line width for the error bars (default=1)

- errorBarLineColour:

  The colour of the error bars (default="grey40")

- errorBarWhiskerWidth:

  The width of error bar whiskers, as proportion of bar width
  (default=.2)

## Value

Invisibly returns a data frame containing the factor levels, group means
and confidence intervals. Note that this function is usually called for
its side effects.

## Details

Plots group means (or other function, if specified) broken down by one
or two grouping factors. Confidence intervals (or other function) are
plotted. User specifies a two sided formula of the form
`response ~ group1 + group2`, where `response` must be numeric and
`group1` and `group2` are factors. The `group1` variable defines the
primary separation on the x-axis, and the x-axis labels by default print
out the levels of this factor. The `group2` variable defines the finer
grain separation, and the legend labels correspond to the levels of this
factor. Note that `group2` is optional.
