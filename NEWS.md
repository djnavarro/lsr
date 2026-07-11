# lsr 1.0.0

## Bug fixes

* `correlate()`: when fewer than 2 numeric variables were present in the
  input, the function could iterate incorrectly due to `1:(n-1)` evaluating
  to `c(1, 0)` rather than an empty sequence. Fixed with `seq_len(n-1)`.

* `oneSampleTTest()`: the `conf.level` argument was not forwarded to
  `stats::t.test()`, so `$conf.int` always contained the 95% interval
  regardless of what the user requested (#9).

* `independentSamplesTTest()`, `pairedSamplesTTest()`, `associationTest()`:
  passing a tibble as `data` caused spurious type-check failures. Fixed by
  coercing `data` to a plain data frame on entry (#2).

## Dependencies

* `tibble` and `withr` added to `Suggests` (used in tests only).

---

# lsr 0.5.2

* Updates maintainer email.
* Adds basic unit tests in preparation for refactoring.

# lsr 0.5.1

* Uses correct maintainer information.
* Ports documentation to roxygen2 and adds pkgdown site.
* Minor internal changes to fix CRAN notes.

# lsr 0.5

* Version number advanced to match next iteration of the book.
* `maxFreq()` and `modeOf()` argument checking is more permissive.
* `correlate()` allows the user to input two numeric vectors instead of
  data frames / matrices.

# lsr 0.3.2

* Convenience functions `goodnessOfFitTest()`, `associationTest()` added.
* Fixed a couple of bugs in the t-test functions.
* `pairedSamplesTTest()` now supports long-form and wide-form data.

# lsr 0.3.1

* `cohensD()` argument checking is more permissive.
* Convenience functions `oneSampleTTest()`, `pairedSamplesTTest()`, and
  `independentSamplesTTest()` added.

# lsr 0.3

* `bars()` function added.
* `correlate()` function added.
* In general, there is a lot more checking of the user input.
* `pooledSD` moved to a sub-function of `cohensD()`.
* Formula argument to `cohensD()` is now handled using `model.frame()`.
* `expandFactors()` now returns a data frame rather than a matrix (and does
  so correctly for data frames with only one row).
* `expandFactors()` no longer drops cases with `NA`.
* Fixed bug in `wideToLong()` in which the reshape function tries to create
  non-unique row names.
* Fixed bug in `quantileCut()` when missing data are present.
* `ciMean()` now handles data frames and matrices.

# lsr 0.2.4

* Fixed a bug in `etaSquared()` that prevented it from correctly determining
  marginal terms in Type II sum of squares, and another in which the residual
  sum of squares was being calculated incorrectly.
* Fixed a bug in `wideToLong()` that stopped it working if between-subject
  variables did not uniquely identify the cases.
* `cohensD()` now includes a `formula` argument in order to more closely
  mimic the syntax of `t.test()`.

# lsr 0.2.3

* Fixed a bug in which `cramersV()` was incorrectly calculated when used as
  a measure of effect size in a goodness of fit test.

# lsr 0.2.2

* Fixed a bug with `longToWide()` and `wideToLong()` in which variable names
  were being treated as regular expressions.

# lsr 0.2.1

* Warning statements in the help files are more explicit regarding (lack of)
  backwards compatibility for pre-1.0 versions of the package.
* `xfun` removed from package (duplicates existing functionality).

# lsr 0.2

* New functions: `rowCopy()`, `colCopy()`, `permuteLevels()`,
  `expandFactors()`, `xfun()`.
* `etaSquared()` now supports Type II and Type III sum of squares.
* Clean up of the repeated measures reshaping functions.

# lsr 0.1.1

* `who()` now has `expand = FALSE` as the default.
* `sortFrame()` now correctly returns the original data frame when no sort
  terms are included.

# lsr 0.1

* Initial release. Functions included: `aad()`, `ciMean()`, `cohensD()`,
  `cramersV()`, `etaSquared()`, `importList()`, `longRM()`, `longToWide()`,
  `maxFreq()`, `modeOf()`, `posthocPairwiseT()`, `quantileCut()`,
  `rmAll()`, `sortFrame()`, `standardCoefs()`, `tFrame()`, `unlibrary()`,
  `who()`, `wideRM()`, `wideToLong()`, `wideToMV()`.
