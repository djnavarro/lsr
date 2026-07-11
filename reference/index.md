# Package index

## Hypothesis tests

Wrapper functions for common statistical tests used in introductory
courses, with output designed to be readable by beginners.

- [`oneSampleTTest()`](https://lsr.djnavarro.net/reference/oneSampleTTest.md)
  : One sample t-test
- [`independentSamplesTTest()`](https://lsr.djnavarro.net/reference/independentSamplesTTest.md)
  : Independent samples t-test
- [`pairedSamplesTTest()`](https://lsr.djnavarro.net/reference/pairedSamplesTTest.md)
  : Paired samples t-test
- [`associationTest()`](https://lsr.djnavarro.net/reference/associationTest.md)
  : Chi-square test of association / independence
- [`goodnessOfFitTest()`](https://lsr.djnavarro.net/reference/goodnessOfFitTest.md)
  : Chi-square goodness of fit test
- [`posthocPairwiseT()`](https://lsr.djnavarro.net/reference/posthocPairwiseT.md)
  : Post-hoc pairwise t-tests for ANOVA
- [`print(`*`<TTest>`*`)`](https://lsr.djnavarro.net/reference/print.TTest.md)
  : Print t-test results
- [`print(`*`<assocTest>`*`)`](https://lsr.djnavarro.net/reference/print.assocTest.md)
  : Print chi-square association test results
- [`print(`*`<gofTest>`*`)`](https://lsr.djnavarro.net/reference/print.gofTest.md)
  : Print goodness of fit test results

## Effect sizes and descriptive statistics

Functions for computing effect sizes and summarising data, intended as
companions to the hypothesis test functions above.

- [`cohensD()`](https://lsr.djnavarro.net/reference/cohensD.md) :
  Cohen's d
- [`etaSquared()`](https://lsr.djnavarro.net/reference/etaSquared.md) :
  Effect size for ANOVAs
- [`cramersV()`](https://lsr.djnavarro.net/reference/cramersV.md) :
  Cramer's V
- [`ciMean()`](https://lsr.djnavarro.net/reference/ciMean.md) :
  Confidence interval around the mean
- [`correlate()`](https://lsr.djnavarro.net/reference/correlate.md) :
  Correlation matrices
- [`print(`*`<correlate>`*`)`](https://lsr.djnavarro.net/reference/print.correlate.md)
  : Print correlation matrix results

## Data manipulation

Functions for reshaping, reordering, and recoding data frames and
factors.

- [`wideToLong()`](https://lsr.djnavarro.net/reference/wideToLong.md) :
  Reshape from wide to long
- [`longToWide()`](https://lsr.djnavarro.net/reference/longToWide.md) :
  Reshape from long to wide
- [`expandFactors()`](https://lsr.djnavarro.net/reference/expandFactors.md)
  : Expand factors to a set of contrasts
- [`permuteLevels()`](https://lsr.djnavarro.net/reference/permuteLevels.md)
  : Permute the levels of a factor
- [`quantileCut()`](https://lsr.djnavarro.net/reference/quantileCut.md)
  : Cut by quantiles
- [`sortFrame()`](https://lsr.djnavarro.net/reference/sortFrame.md) :
  Sort a data frame
- [`tFrame()`](https://lsr.djnavarro.net/reference/tFrame.md) :
  Transpose a data frame
- [`colCopy()`](https://lsr.djnavarro.net/reference/copy.md)
  [`rowCopy()`](https://lsr.djnavarro.net/reference/copy.md) : Copy a
  vector into a matrix

## Visualisation

A simple plotting function for displaying frequency and proportion data
as bar charts.

- [`bars()`](https://lsr.djnavarro.net/reference/bars.md) : Grouped bar
  plots with error bars

## Workspace utilities

Helper functions for inspecting and managing the R workspace, and for
miscellaneous statistical tasks not covered elsewhere.

- [`who()`](https://lsr.djnavarro.net/reference/who.md) : Contents of
  workspace
- [`modeOf()`](https://lsr.djnavarro.net/reference/mode.md)
  [`maxFreq()`](https://lsr.djnavarro.net/reference/mode.md) : Sample
  mode
- [`aad()`](https://lsr.djnavarro.net/reference/aad.md) : Mean absolute
  deviation
- [`rmAll()`](https://lsr.djnavarro.net/reference/rmAll.md) : Remove all
  objects from the workspace
- [`unlibrary()`](https://lsr.djnavarro.net/reference/unlibrary.md) :
  Unload a package
- [`importList()`](https://lsr.djnavarro.net/reference/importList.md) :
  Import a list into the workspace
- [`standardCoefs()`](https://lsr.djnavarro.net/reference/standardCoefs.md)
  : Standardised regression coefficients
- [`print(`*`<whoList>`*`)`](https://lsr.djnavarro.net/reference/print.whoList.md)
  : Print workspace summary
