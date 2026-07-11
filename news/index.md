# Changelog

## lsr 1.0.0

### Bug fixes

- [`correlate()`](https://lsr.djnavarro.net/reference/correlate.md):
  when a pair of variables had too few complete observations for
  [`cor.test()`](https://rdrr.io/r/stats/cor.test.html) to run, the
  entire call would abort. The affected cell is now left as `NA` and the
  remaining pairs are computed normally.

- [`correlate()`](https://lsr.djnavarro.net/reference/correlate.md),
  [`goodnessOfFitTest()`](https://lsr.djnavarro.net/reference/goodnessOfFitTest.md),
  [`associationTest()`](https://lsr.djnavarro.net/reference/associationTest.md):
  the internal `options(warn = 2)` used to intercept warnings was not
  guarded with [`on.exit()`](https://rdrr.io/r/base/on.exit.html), so an
  unexpected error could leave the session’s global warn level
  permanently elevated. All three functions now use
  [`on.exit()`](https://rdrr.io/r/base/on.exit.html) to guarantee
  restoration.

- [`cramersV()`](https://lsr.djnavarro.net/reference/cramersV.md):
  Yates’ continuity correction was applied by default for 2×2 tables
  (inherited from
  [`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html)), causing V
  to be less than 1 even for perfectly associated tables. Cramér’s V is
  now always computed from the Pearson chi-squared (no continuity
  correction), giving results on the correct 0-to-1 scale.

- [`goodnessOfFitTest()`](https://lsr.djnavarro.net/reference/goodnessOfFitTest.md),
  [`associationTest()`](https://lsr.djnavarro.net/reference/associationTest.md):
  when a factor variable had unused levels, the extra levels were
  silently included in the test with zero observed cases, changing
  degrees of freedom and p-values without any indication. Both functions
  now issue an informative warning when unused levels are detected, with
  a suggestion to call
  [`droplevels()`](https://rdrr.io/r/base/droplevels.html).

- [`modeOf()`](https://lsr.djnavarro.net/reference/mode.md),
  [`maxFreq()`](https://lsr.djnavarro.net/reference/mode.md): all-`NA`
  or zero-length input produced a cryptic base-R
  `"no non-missing arguments to max"` warning and returned empty output.
  Both functions now issue an informative warning and return `NA`.

- [`wideToLong()`](https://lsr.djnavarro.net/reference/wideToLong.md):
  when no column names contained the separator string, the error came
  from deep inside
  [`stats::reshape()`](https://rdrr.io/r/stats/reshape.html) with no
  hint that the cause was a naming-convention mismatch. The function now
  checks for this condition early and stops with a plain-English
  message.

- [`importList()`](https://lsr.djnavarro.net/reference/importList.md):
  passing an unnamed or partially-named list produced a cryptic
  `"'names' must be a character vector"` error from base R. The function
  now checks for missing names before proceeding and stops with an
  informative message. Passing an empty list now also produces a message
  rather than silently doing nothing.

### Bug fixes

- [`correlate()`](https://lsr.djnavarro.net/reference/correlate.md):
  when fewer than 2 numeric variables were present in the input, the
  function could iterate incorrectly due to `1:(n-1)` evaluating to
  `c(1, 0)` rather than an empty sequence. Fixed with `seq_len(n-1)`.

- [`oneSampleTTest()`](https://lsr.djnavarro.net/reference/oneSampleTTest.md):
  the `conf.level` argument was not forwarded to
  [`stats::t.test()`](https://rdrr.io/r/stats/t.test.html), so
  `$conf.int` always contained the 95% interval regardless of what the
  user requested ([\#9](https://github.com/djnavarro/lsr/issues/9)).

- [`independentSamplesTTest()`](https://lsr.djnavarro.net/reference/independentSamplesTTest.md),
  [`pairedSamplesTTest()`](https://lsr.djnavarro.net/reference/pairedSamplesTTest.md),
  [`associationTest()`](https://lsr.djnavarro.net/reference/associationTest.md):
  passing a tibble as `data` caused spurious type-check failures. Fixed
  by coercing `data` to a plain data frame on entry
  ([\#2](https://github.com/djnavarro/lsr/issues/2)).

### Dependencies

- `tibble` and `withr` added to `Suggests` (used in tests only).

------------------------------------------------------------------------

## lsr 0.5.2

CRAN release: 2021-12-01

- Updates maintainer email.
- Adds basic unit tests in preparation for refactoring.

## lsr 0.5.1

CRAN release: 2021-09-20

- Uses correct maintainer information.
- Ports documentation to roxygen2 and adds pkgdown site.
- Minor internal changes to fix CRAN notes.

## lsr 0.5

CRAN release: 2015-03-02

- Version number advanced to match next iteration of the book.
- [`maxFreq()`](https://lsr.djnavarro.net/reference/mode.md) and
  [`modeOf()`](https://lsr.djnavarro.net/reference/mode.md) argument
  checking is more permissive.
- [`correlate()`](https://lsr.djnavarro.net/reference/correlate.md)
  allows the user to input two numeric vectors instead of data frames /
  matrices.

## lsr 0.3.2

CRAN release: 2014-01-31

- Convenience functions
  [`goodnessOfFitTest()`](https://lsr.djnavarro.net/reference/goodnessOfFitTest.md),
  [`associationTest()`](https://lsr.djnavarro.net/reference/associationTest.md)
  added.
- Fixed a couple of bugs in the t-test functions.
- [`pairedSamplesTTest()`](https://lsr.djnavarro.net/reference/pairedSamplesTTest.md)
  now supports long-form and wide-form data.

## lsr 0.3.1

CRAN release: 2013-11-19

- [`cohensD()`](https://lsr.djnavarro.net/reference/cohensD.md) argument
  checking is more permissive.
- Convenience functions
  [`oneSampleTTest()`](https://lsr.djnavarro.net/reference/oneSampleTTest.md),
  [`pairedSamplesTTest()`](https://lsr.djnavarro.net/reference/pairedSamplesTTest.md),
  and
  [`independentSamplesTTest()`](https://lsr.djnavarro.net/reference/independentSamplesTTest.md)
  added.

## lsr 0.3

CRAN release: 2013-11-13

- [`bars()`](https://lsr.djnavarro.net/reference/bars.md) function
  added.
- [`correlate()`](https://lsr.djnavarro.net/reference/correlate.md)
  function added.
- In general, there is a lot more checking of the user input.
- `pooledSD` moved to a sub-function of
  [`cohensD()`](https://lsr.djnavarro.net/reference/cohensD.md).
- Formula argument to
  [`cohensD()`](https://lsr.djnavarro.net/reference/cohensD.md) is now
  handled using
  [`model.frame()`](https://rdrr.io/r/stats/model.frame.html).
- [`expandFactors()`](https://lsr.djnavarro.net/reference/expandFactors.md)
  now returns a data frame rather than a matrix (and does so correctly
  for data frames with only one row).
- [`expandFactors()`](https://lsr.djnavarro.net/reference/expandFactors.md)
  no longer drops cases with `NA`.
- Fixed bug in
  [`wideToLong()`](https://lsr.djnavarro.net/reference/wideToLong.md) in
  which the reshape function tries to create non-unique row names.
- Fixed bug in
  [`quantileCut()`](https://lsr.djnavarro.net/reference/quantileCut.md)
  when missing data are present.
- [`ciMean()`](https://lsr.djnavarro.net/reference/ciMean.md) now
  handles data frames and matrices.

## lsr 0.2.4

CRAN release: 2013-06-13

- Fixed a bug in
  [`etaSquared()`](https://lsr.djnavarro.net/reference/etaSquared.md)
  that prevented it from correctly determining marginal terms in Type II
  sum of squares, and another in which the residual sum of squares was
  being calculated incorrectly.
- Fixed a bug in
  [`wideToLong()`](https://lsr.djnavarro.net/reference/wideToLong.md)
  that stopped it working if between-subject variables did not uniquely
  identify the cases.
- [`cohensD()`](https://lsr.djnavarro.net/reference/cohensD.md) now
  includes a `formula` argument in order to more closely mimic the
  syntax of [`t.test()`](https://rdrr.io/r/stats/t.test.html).

## lsr 0.2.3

CRAN release: 2013-04-19

- Fixed a bug in which
  [`cramersV()`](https://lsr.djnavarro.net/reference/cramersV.md) was
  incorrectly calculated when used as a measure of effect size in a
  goodness of fit test.

## lsr 0.2.2

CRAN release: 2013-01-15

- Fixed a bug with
  [`longToWide()`](https://lsr.djnavarro.net/reference/longToWide.md)
  and
  [`wideToLong()`](https://lsr.djnavarro.net/reference/wideToLong.md) in
  which variable names were being treated as regular expressions.

## lsr 0.2.1

CRAN release: 2012-11-28

- Warning statements in the help files are more explicit regarding (lack
  of) backwards compatibility for pre-1.0 versions of the package.
- `xfun` removed from package (duplicates existing functionality).

## lsr 0.2

CRAN release: 2012-11-14

- New functions:
  [`rowCopy()`](https://lsr.djnavarro.net/reference/copy.md),
  [`colCopy()`](https://lsr.djnavarro.net/reference/copy.md),
  [`permuteLevels()`](https://lsr.djnavarro.net/reference/permuteLevels.md),
  [`expandFactors()`](https://lsr.djnavarro.net/reference/expandFactors.md),
  `xfun()`.
- [`etaSquared()`](https://lsr.djnavarro.net/reference/etaSquared.md)
  now supports Type II and Type III sum of squares.
- Clean up of the repeated measures reshaping functions.

## lsr 0.1.1

CRAN release: 2012-01-23

- [`who()`](https://lsr.djnavarro.net/reference/who.md) now has
  `expand = FALSE` as the default.
- [`sortFrame()`](https://lsr.djnavarro.net/reference/sortFrame.md) now
  correctly returns the original data frame when no sort terms are
  included.

## lsr 0.1

CRAN release: 2011-12-11

- Initial release. Functions included:
  [`aad()`](https://lsr.djnavarro.net/reference/aad.md),
  [`ciMean()`](https://lsr.djnavarro.net/reference/ciMean.md),
  [`cohensD()`](https://lsr.djnavarro.net/reference/cohensD.md),
  [`cramersV()`](https://lsr.djnavarro.net/reference/cramersV.md),
  [`etaSquared()`](https://lsr.djnavarro.net/reference/etaSquared.md),
  [`importList()`](https://lsr.djnavarro.net/reference/importList.md),
  `longRM()`,
  [`longToWide()`](https://lsr.djnavarro.net/reference/longToWide.md),
  [`maxFreq()`](https://lsr.djnavarro.net/reference/mode.md),
  [`modeOf()`](https://lsr.djnavarro.net/reference/mode.md),
  [`posthocPairwiseT()`](https://lsr.djnavarro.net/reference/posthocPairwiseT.md),
  [`quantileCut()`](https://lsr.djnavarro.net/reference/quantileCut.md),
  [`rmAll()`](https://lsr.djnavarro.net/reference/rmAll.md),
  [`sortFrame()`](https://lsr.djnavarro.net/reference/sortFrame.md),
  [`standardCoefs()`](https://lsr.djnavarro.net/reference/standardCoefs.md),
  [`tFrame()`](https://lsr.djnavarro.net/reference/tFrame.md),
  [`unlibrary()`](https://lsr.djnavarro.net/reference/unlibrary.md),
  [`who()`](https://lsr.djnavarro.net/reference/who.md), `wideRM()`,
  [`wideToLong()`](https://lsr.djnavarro.net/reference/wideToLong.md),
  `wideToMV()`.
